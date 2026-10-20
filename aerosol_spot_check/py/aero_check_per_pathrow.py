#import modules
import ee
import os
import time
import yaml
from pandas import read_csv

# Standalone Landsat 8/9 aerosol QA spot check (NOT part of the {targets}
# pipeline). For each sampled lake and Landsat 8/9 scene, this applies the same
# water/quality masks as production EXCEPT the aerosol mask, then tallies water
# pixels by SR_QA_AEROSOL level and summarizes reflectance per aerosol level and
# under each candidate masking rule.
#
# Helper functions below are copied from
# b_pull_Landsat_SRST_poi/py/run_GEE_per_pathrow.py at commit 32f0012. The
# production script runs the full pull on import, so it cannot be imported here.
#
# SR_QA_AEROSOL bits (Landsat 8/9 Collection 2):
#   bit 1: valid aerosol retrieval
#   bit 2: water pixel
#   bit 5: aerosol interpolated
#   bits 6-7: aerosol level (0 = climatology, 1 = low, 2 = medium, 3 = high)

# LOAD ALL THE CUSTOM FUNCTIONS -----------------------------------------------

def csv_to_eeFeat(df, proj):
  """Function to create an eeFeature from the location data

  Args:
      df: point locations .csv file with Latitude and Longitude
      proj: CRS projection of the points

  Returns:
      ee.FeatureCollection of the points
  """
  features=[]
  for i in range(len(df)):
    row = df.iloc[i]
    latlong = [row['Longitude'], row['Latitude']]
    loc_properties = {'system:index': str(row['id']), 'id': str(row['id'])}
    g = ee.Geometry.Point(latlong, proj)
    features.append(ee.Feature(g, loc_properties))
  return ee.FeatureCollection(features)


def apply_scale_factors(image):
  """ Applies scaling factors for Landsat Collection 2 surface reflectance
  and surface temperature products

  Args:
      image: one ee.Image of an ee.ImageCollection

  Returns:
      ee.Image with band values overwritten by scaling factors
  """
  opticalBands = image.select('SR_B.').multiply(0.0000275).add(-0.2)
  thermalBands = image.select('ST_B.*').multiply(0.00341802).add(149.0)
  return image.addBands(opticalBands, None, True).addBands(thermalBands, None,True)


def dp_buff(image):
  """ Buffer ee.FeatureCollection sites from csv_to_eeFeat by user-specified radius

  Args:
      image: ee.Image of an ee.ImageCollection

  Returns:
      ee.FeatureCollection of polygons resulting from buffered points
  """
  return image.buffer(ee.Number.parse(str(buffer)))


def apply_rad_mask(image):
  """Mask out all pixels that are radiometrically saturated using the QA_RADSAT
  QA band.

  Args:
      image: ee.Image of an ee.ImageCollection

  Returns:
      ee.Image where saturated pixels are masked
  """
  satQA = image.select('radsat_qa')
  satMask = satQA.eq(0)
  return image.updateMask(satMask)


def add_cf_mask(image):
  """Creates a binary band for contaminated or clear pixels

  Args:
      image: ee.Image of an ee.ImageCollection

  Returns:
      ee.Image with additional band called 'cfmask'
  """
  qa = image.select('pixel_qa')
  cloudqa = (qa.bitwiseAnd(1 << 1).rename('cfmask') #dialated clouds value 1
    .where(qa.bitwiseAnd(1 << 3), ee.Image(2)) # clouds value 2
    .where(qa.bitwiseAnd(1 << 4), ee.Image(3)) # cloud shadows value 3
    .where(qa.bitwiseAnd(1 << 5), ee.Image(4))) # snow value 4
  return image.addBands(cloudqa)


def apply_fill_mask_89(image):
  """ mask any fill values (0) in scaled raster for Landsat 8,9

  Args:
      image: ee.Image of an ee.ImageCollection

  Returns:
      an ee.Image where any values previously 0 are masked
  """
  b1_mask = image.select('SR_B1').gt(0)
  b2_mask = image.select('SR_B2').gt(0)
  b3_mask = image.select('SR_B3').gt(0)
  b4_mask = image.select('SR_B4').gt(0)
  b5_mask = image.select('SR_B5').gt(0)
  b6_mask = image.select('SR_B6').gt(0)
  b7_mask = image.select('SR_B7').gt(0)
  fill_mask = (b1_mask.eq(1)
    .And(b2_mask.eq(1))
    .And(b3_mask.eq(1))
    .And(b4_mask.eq(1))
    .And(b5_mask.eq(1))
    .And(b6_mask.eq(1))
    .And(b7_mask.eq(1))
    .selfMask()
    )
  return image.updateMask(fill_mask.eq(1))


# production applies the 457 version of this mask to Landsat 8/9 (it does not
# check the Aerosol band), so we do the same here for comparability
def add_realistic_mask_457(image):
  """ mask out unrealistic SR values (those less than -0.01)

  Args:
      image: ee.Image of an ee.ImageCollection

  Returns:
      an ee.Image with a 1/0 mask for realistic values
  """
  b1_mask = image.select('Blue').gt(-0.01)
  b2_mask = image.select('Green').gt(-0.01)
  b3_mask = image.select('Red').gt(-0.01)
  b4_mask = image.select('Nir').gt(-0.01)
  b5_mask = image.select('Swir1').gt(-0.01)
  b7_mask = image.select('Swir2').gt(-0.01)
  realistic = (b1_mask.eq(1)
    .And(b2_mask.eq(1))
    .And(b3_mask.eq(1))
    .And(b4_mask.eq(1))
    .And(b5_mask.eq(1))
    .And(b7_mask.eq(1))
    .selfMask()).rename('real')
  return image.addBands(realistic)


def add_sun_glint_mask(image):
  """ mask out pixels likely affected by sun glint (those greater than 0.2)

  Args:
      image: ee.Image of an ee.ImageCollection

  Returns:
      an ee.Image with a 1 (unlikely glint) / 0 (likely glint) mask for pixels in
      all bands but Aerosol
  """
  b1_mask = image.select('Blue').lt(0.2)
  b2_mask = image.select('Green').lt(0.2)
  b3_mask = image.select('Red').lt(0.2)
  b4_mask = image.select('Nir').lt(0.2)
  b5_mask = image.select('Swir1').lt(0.2)
  b7_mask = image.select('Swir2').lt(0.2)
  no_glint = (b1_mask.eq(1)
    .And(b2_mask.eq(1))
    .And(b3_mask.eq(1))
    .And(b4_mask.eq(1))
    .And(b5_mask.eq(1))
    .And(b7_mask.eq(1))
    .selfMask()).rename('no_glint')
  return image.addBands(no_glint)


def Mndwi(image):
  return (image.expression('(GREEN - SWIR1) / (GREEN + SWIR1)', {
    'GREEN': image.select(['Green']),
    'SWIR1': image.select(['Swir1'])
  })).rename('mndwi')


def Mbsrv(image):
  return (image.select(['Green']).add(image.select(['Red'])).rename('mbsrv'))


def Mbsrn(image):
  return (image.select(['Nir']).add(image.select(['Swir1'])).rename('mbsrn'))


def Ndvi(image):
  return (image.expression('(NIR - RED) / (NIR + RED)', {
    'RED': image.select(['Red']),
    'NIR': image.select(['Nir'])
  })).rename('ndvi')


def Awesh(image):
  return (image.expression('Blue + 2.5 * Green + (-1.5) * mbsrn + (-0.25) * Swir2', {
    'Blue': image.select(['Blue']),
    'Green': image.select(['Green']),
    'mbsrn': Mbsrn(image).select(['mbsrn']),
    'Swir2': image.select(['Swir2'])
  })).rename('awesh')


def DSWE(image):
  """calculate the dynamic surface water extent per pixel

  Args:
      image: ee.Image of an ee.ImageCollection

  Returns:
      band where values calculated are the DSWE value per pixel
  """
  mndwi = Mndwi(image)
  mbsrv = Mbsrv(image)
  mbsrn = Mbsrn(image)
  awesh = Awesh(image)
  swir1 = image.select(['Swir1'])
  nir = image.select(['Nir'])
  ndvi = Ndvi(image)
  blue = image.select(['Blue'])
  swir2 = image.select(['Swir2'])
  # These thresholds are taken from the LS Collection 2 DSWE Data Format Control Book
  t1 = mndwi.gt(0.124)
  t2 = mbsrv.gt(mbsrn)
  t3 = awesh.gt(0)
  t4 = (mndwi.gt(-0.44)
   .And(swir1.lt(0.09))
   .And(nir.lt(0.15))
   .And(ndvi.lt(0.7)))
  t5 = (mndwi.gt(-0.5)
   .And(blue.lt(0.1))
   .And(swir1.lt(0.3))
   .And(swir2.lt(0.1))
   .And(nir.lt(0.25)))
  t = (t1
    .add(t2.multiply(10))
    .add(t3.multiply(100))
    .add(t4.multiply(1000))
    .add(t5.multiply(10000)))
  noWater = (t.eq(0)
    .Or(t.eq(1))
    .Or(t.eq(10))
    .Or(t.eq(100))
    .Or(t.eq(1000)))
  hWater = (t.eq(1111)
    .Or(t.eq(10111))
    .Or(t.eq(11011))
    .Or(t.eq(11101))
    .Or(t.eq(11110))
    .Or(t.eq(11111)))
  mWater = (t.eq(111)
    .Or(t.eq(1011))
    .Or(t.eq(1101))
    .Or(t.eq(1110))
    .Or(t.eq(10011))
    .Or(t.eq(10101))
    .Or(t.eq(10110))
    .Or(t.eq(11001))
    .Or(t.eq(11010))
    .Or(t.eq(11100)))
  pWetland = t.eq(11000)
  lWater = (t.eq(11)
    .Or(t.eq(101))
    .Or(t.eq(110))
    .Or(t.eq(1001))
    .Or(t.eq(1010))
    .Or(t.eq(1100))
    .Or(t.eq(10000))
    .Or(t.eq(10001))
    .Or(t.eq(10010))
    .Or(t.eq(10100)))
  iDswe = (noWater.multiply(0)
    .add(hWater.multiply(1))
    .add(mWater.multiply(2))
    .add(pWetland.multiply(3))
    .add(lWater.multiply(4)))
  return iDswe.rename('dswe')


def calc_hill_shadows(image, geo):
  """ caluclate the hill shadow per pixel

  Args:
      image: ee.Image of an ee.ImageCollection
      geo: geometry of the WRS tile as wrs.geometry() in script

  Returns:
      a band named 'hillShadow' where values calculated are the hill shadow per
      pixel. output 1 where pixels are illumunated and 0 where they are shadowed.
  """
  MergedDEM = ee.Image("MERIT/DEM/v1_0_3").clip(geo.buffer(3000))
  hillShadow = ee.Terrain.hillShadow(MergedDEM,
    ee.Number(image.get('SUN_AZIMUTH')),
    ee.Number(90).subtract(image.get('SUN_ELEVATION')),
    30)
  hillShadow = hillShadow.rename(['hillShadow'])
  return hillShadow


def remove_geo(image):
  return image.setGeometry(None)


# SPOT CHECK-SPECIFIC FUNCTIONS -----------------------------------------------

# bands summarized by aerosol level and masking rule
sum_bands = ['Aerosol', 'Blue', 'Green', 'Red', 'Nir']

# aerosol levels from bits 6-7
aero_levels = {'clim': 0, 'low': 1, 'med': 2, 'high': 3}

# candidate masking rules, as the maximum aerosol level retained
aero_rules = {'nomask': 3, 'nohigh': 2, 'nomedhigh': 1}


def aero_check_89(image, feat):
  """ Applies production masks (except aerosol) for DSWE1 water and summarizes
  pixel counts and reflectance by SR_QA_AEROSOL level and masking rule

  Args:
      image: ee.Image of an ee.ImageCollection
      feat: ee.FeatureCollection of the buffered locations

  Returns:
      ee.FeatureCollection of summaries for each location in feat
  """
  clouds = add_cf_mask(image).select('cfmask').gte(1)
  real = add_realistic_mask_457(image).select('real').eq(1)
  no_glint = add_sun_glint_mask(image).select('no_glint').eq(1)
  hs = calc_hill_shadows(image, wrs.geometry()).select('hillShadow')
  d = DSWE(image).select('dswe')

  # high confidence water, fully illuminated, no clouds, realistic, no glint
  # (this mirrors production DSWE1 minus the aerosol mask)
  water = (d.eq(1)
    .updateMask(hs.eq(1))
    .updateMask(clouds.eq(0))
    .updateMask(real.eq(1))
    .updateMask(no_glint.eq(1))
    .selfMask())

  aero_qa = image.select('aerosol_qa')
  level = aero_qa.rightShift(6).bitwiseAnd(3)
  interp = aero_qa.rightShift(5).bitwiseAnd(1)
  valid = aero_qa.rightShift(1).bitwiseAnd(1)

  refl = image.select(sum_bands).updateMask(water)

  # count bands (pixels are counted where unmasked)
  counts = (water.rename('pCount_water')
    .addBands(interp.eq(1).updateMask(water).selfMask().rename('pCount_interp'))
    .addBands(valid.eq(1).updateMask(water).selfMask().rename('pCount_valid')))
  for name, val in aero_levels.items():
    counts = counts.addBands(level.eq(val).updateMask(water).selfMask().rename('pCount_' + name))

  # median reflectance per aerosol level and per masking rule
  med_imgs = []
  for name, val in aero_levels.items():
    med_imgs.append(refl.updateMask(level.eq(val))
      .rename(['med_' + b + '_' + name for b in sum_bands]))
  for name, val in aero_rules.items():
    med_imgs.append(refl.updateMask(level.lte(val))
      .rename(['med_' + b + '_' + name for b in sum_bands]))
  meds = ee.Image.cat(med_imgs)

  # single-output reducers via forEachBand are named by band name
  reducer = (ee.Reducer.count().unweighted()
      .forEachBand(counts)
    .combine(ee.Reducer.median().unweighted()
      .forEachBand(meds),
      sharedInputs = False))

  out = (counts.addBands(meds)
    .reduceRegions(feat, reducer, 30)
    .map(lambda f: f.set({
      'LANDSAT_PRODUCT_ID': image.get('LANDSAT_PRODUCT_ID'),
      'SPACECRAFT_ID': image.get('SPACECRAFT_ID'),
      'DATE_ACQUIRED': image.get('DATE_ACQUIRED'),
      'CLOUD_COVER': image.get('CLOUD_COVER'),
      'WRS2_PR': pr
      })))
  return out.map(remove_geo)


def maximum_no_of_tasks(MaxNActive, waitingPeriod):
  """ Function to limit the number of tasks sent to Earth Engine at one time to avoid time out errors

  Args:
      MaxNActive: maximum number of tasks that can be active in Earth Engine at one time
      waitingPeriod: time to wait between checking if tasks are completed, in seconds

  Returns:
      None.
  """
  ##maintain a maximum number of active tasks
  time.sleep(10)
  ## initialize submitting jobs
  ts = list(ee.batch.Task.list())
  NActive = 0
  for task in ts:
     if ('RUNNING' in str(task) or 'READY' in str(task)):
         NActive += 1
  ## wait if the number of current active tasks reach the maximum number
  ## defined in MaxNActive
  while (NActive >= MaxNActive):
    time.sleep(waitingPeriod) # if reach or over maximum no. of active tasks, wait for 2min and check again
    ts = list(ee.batch.Task.list())
    NActive = 0
    for task in ts:
      if ('RUNNING' in str(task) or 'READY' in str(task)):
        NActive += 1
  return()


# SETTINGS ---------------------------------------------------------------------

with open("aerosol_spot_check/config_aero_check.yml", "r") as file:
  cfg = yaml.safe_load(file)

ee.Initialize(project = cfg["google_settings"]["ee_proj"])

run_date = cfg["google_settings"]["run_date"]
folder_version = cfg["google_settings"]["drive_folder"] + "_v" + run_date
start_date = cfg["temporal_settings"]["start_date"]
end_date = cfg["temporal_settings"]["end_date"]
buffer = cfg["gee_settings"]["site_buffer"]
cloud_thresh = cfg["gee_settings"]["cloud_thresh"]
image_qual_min = cfg["gee_settings"]["image_qual_min"]

# get current tile
with open("aerosol_spot_check/mid/current_pathrow.txt", "r") as file:
  pr = file.read().strip()

locations_subset = read_csv(os.path.join("aerosol_spot_check/out/sample_locations/",
                                         "locations_" + pr + ".csv"),
                            dtype = {'id': str, 'WRS2_PR': str})


# BUILD STACK AND SEND TASK ----------------------------------------------------

wrs = (ee.FeatureCollection('projects/ee-ls-c2-srst/assets/WRS2_descending')
  .filterMetadata('PR', 'equals', pr))

w_p = int(pr[:3])
w_r = int(pr[-3:])

def get_coll(coll_id):
  return (ee.ImageCollection(coll_id)
    .filter(ee.Filter.eq("WRS_PATH", w_p))
    .filter(ee.Filter.eq("WRS_ROW", w_r))
    .filter(ee.Filter.lt("CLOUD_COVER", cloud_thresh))
    .filter(ee.Filter.gte("IMAGE_QUALITY_OLI", image_qual_min))
    .filterDate(start_date, end_date))

ls89 = get_coll("LANDSAT/LC08/C02/T1_L2").merge(get_coll("LANDSAT/LC09/C02/T1_L2"))

bn89 = (["SR_B1", "SR_B2", "SR_B3", "SR_B4", "SR_B5", "SR_B6", "SR_B7",
  "QA_PIXEL", "SR_QA_AEROSOL", "QA_RADSAT", "ST_B10"])
bns89 = (["Aerosol", "Blue", "Green", "Red", "Nir", "Swir1", "Swir2",
  "pixel_qa", "aerosol_qa", "radsat_qa", "SurfaceTemp"])

maximum_no_of_tasks(10, 120)

feat = (csv_to_eeFeat(locations_subset, "EPSG:4326")
  .filterBounds(wrs.geometry())
  .map(dp_buff))

stack = (ls89
  .filterBounds(feat.geometry())
  .map(apply_fill_mask_89)
  .map(apply_scale_factors)
  .select(bn89, bns89)
  .map(apply_rad_mask))

out = (stack.map(lambda image: aero_check_89(image, feat))
  .flatten()
  .filter(ee.Filter.gt('pCount_water', 0)))

count_cols = (['pCount_water', 'pCount_interp', 'pCount_valid'] +
  ['pCount_' + n for n in aero_levels])
med_cols = ['med_' + b + '_' + n
  for n in list(aero_levels) + list(aero_rules)
  for b in sum_bands]

task_name = "lakeSR_aero_check_LS89_" + pr + "_v" + run_date
task = (ee.batch.Export.table.toDrive(collection = out,
                                      description = task_name,
                                      folder = folder_version,
                                      fileFormat = "csv",
                                      selectors = (['id', 'WRS2_PR', 'LANDSAT_PRODUCT_ID',
                                        'SPACECRAFT_ID', 'DATE_ACQUIRED', 'CLOUD_COVER'] +
                                        count_cols + med_cols)))
task.start()
print("Task sent: aerosol spot check for tile " + pr)
