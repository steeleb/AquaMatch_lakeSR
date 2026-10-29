# Landsat C2 SR/ST pull for one WRS2 path-row. Sends one site export per
# mission group and chunk that summarizes all configured DSWE variants (DSWE1,
# DSWE1a, DSWE3). All per-image processing (masks, hillshade, hillshadow, DSWE)
# is done once per image for all variants. Locations are sorted spatially before
# chunking, so chunks share fewer image tiles. Downloaded files are split into
# per-variant files with split_combined_dswe() in c_collate_Landsat_data/src/.

#import modules
import ee
import time
from datetime import date, datetime
import os 
from pandas import read_csv
import math

# LOAD ALL THE CUSTOM FUNCTIONS -----------------------------------------------
# DSWE functions begin at 'def DSWE'; reflectance pull function at 'def ref_pull_combined'
# config and GEE pull code begin at 'IMPORT CONFIG VARIABLES'

def csv_to_eeFeat(df, proj):
  """Function to create an eeFeature from the location data

  Args:
      df: point locations .csv file with Latitude and Longitude; when chunking,
        this is the chunk subset from process_dataframe_in_chunks
      proj: CRS projection of the points

  Returns:
      ee.FeatureCollection of the points
  """
  features=[]
  # iterate by position over every row of df (already subset to the chunk), so
  # this works regardless of the row labels carried over from the full file
  for i in range(len(df)):
    row = df.iloc[i]
    x, y = row['Longitude'], row['Latitude']
    latlong = [x, y]
    loc_properties = {'system:index': str(row['id']), 'id': str(row['id'])}
    g = ee.Geometry.Point(latlong, proj)
    feature = ee.Feature(g, loc_properties)
    features.append(feature)
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
      ee.Image with additional band called 'radsat', where pixels with a value 
      of 0 are saturated for at least one SR band and a value of 1 is not saturated
  """
  #grab the radsat band
  satQA = image.select('radsat_qa')
  # all must be non-saturated per pixel
  satMask = satQA.eq(0)
  return image.updateMask(satMask)


def add_cf_mask(image):
  """Creates a binary band for contaminated or clear pixels
  
  Args:
      image: ee.Image of an ee.ImageCollection

  Returns:
      ee.Image with additional band called 'cfmask'
  """
  #grab just the pixel_qa info
  qa = image.select('pixel_qa')
  cloudqa = (qa.bitwiseAnd(1 << 1).rename('cfmask') #dialated clouds value 1
    # high aerosol for LS8/9 is taken care of in sr_aerosol function
    .where(qa.bitwiseAnd(1 << 3), ee.Image(2)) # clouds value 2
    .where(qa.bitwiseAnd(1 << 4), ee.Image(3)) # cloud shadows value 3
    .where(qa.bitwiseAnd(1 << 5), ee.Image(4))) # snow value 4
  return image.addBands(cloudqa)


### update these to use renamed bands
def apply_fill_mask_457(image):
  """ mask any fill values (0) in scaled raster for Landsat 4, 5, 7
  
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
  b7_mask = image.select('SR_B7').gt(0)
  fill_mask = (b1_mask.eq(1)
    .And(b2_mask.eq(1))
    .And(b3_mask.eq(1))
    .And(b4_mask.eq(1))
    .And(b5_mask.eq(1))
    .And(b7_mask.eq(1))
    .selfMask()
    )
  return image.updateMask(fill_mask.eq(1))


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


# This should be applied AFTER scaling factors
# Mask values less than -0.01
def add_realistic_mask(image):
  """ mask out unrealistic SR values (those less than -0.01) in Landsat 4-9.
  The Landsat 8/9 Aerosol band is intentionally not screened; Aerosol band
  summaries are provided for diagnostic use only.

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
    ).rename('real')
  return image.addBands(realistic)


# This should be applied AFTER scaling factors
# Mask values greater than 0.2
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
    ).rename('no_glint')
  return image.addBands(no_glint)

# This should be applied AFTER scaling factors
# Flag IR values greater than 0.1
def add_ir_glint_flag(image):
  """ flags pixels likely affected by sun glint in the infrared, where all three 
      infrared bands (Nir, Swir1, Swir2) are greater than or equal to 0.1. This 
      is a flag only and is not used to mask pixels.
  
  Args:
      image: ee.Image of an ee.ImageCollection

  Returns:
      an ee.Image with an additional band 'ir_glint', where pixels are 1 if 
      flagged for ir glint and masked otherwise
  """
  b4_flag = image.select('Nir').gte(0.1)
  b5_flag = image.select('Swir1').gte(0.1)
  b7_flag = image.select('Swir2').gte(0.1)
  ir_glint = (b4_flag.eq(1)
    .And(b5_flag.eq(1))
    .And(b7_flag.eq(1))
    .selfMask()).rename('ir_glint')
  return image.addBands(ir_glint)


# mask high opacity (>0.3 after scaling) pixels
def add_opac_mask(image):
  """ mask out instances where atmospheric opacity is greater than 0.3 in Landsat 
      4, 5, 7
  
  Args:
      image: ee.Image of an ee.ImageCollection

  Returns:
      an ee.Image with an additional mask band where any pixels with SR_ATMOS_OPACITY 
      greater than 0.3 are set to a value of 0 in the 'opac' band
  """
  opac = image.select("opacity_qa").multiply(0.001).lt(0.3).rename('opac')
  return image.addBands(opac)


# function to split QA bits
def extract_qa_bits(qa_band, start_bit, end_bit, band_name):
  """
  Extracts specified quality assurance (QA) bits from a QA band. This function originated
  from https://calekochenour.github.io/remote-sensing-textbook/03-beginner/chapter13-data-quality-bitmasks.html

  Args:
      qa_band (ee.Image): The earth engine image QA band to extract the bits from.
      start_bit (int): The start bit of the QA bits to extract.
      end_bit (int): The end bit of the QA bits to extract (not inclusive)
      band_name (str): The name to give to the output band.

  Returns:
      ee.Image: A single band image of the extracted QA bit values.
  """
  # Initialize QA bit string/pattern to check QA band against
  qa_bits = 0
  # Add each specified QA bit flag value/string/pattern to the QA bits to check/extract
  for bit in range(end_bit):
    qa_bits += (1 << bit)
  # Return a single band image of the extracted QA bit values
  return (qa_band
    # Rename output band to specified name
    .select([0], [band_name])
    # Check QA band against specified QA bits to see what QA flag values are set
    .bitwiseAnd(qa_bits)
    # Get value that matches bitmask documentation
    # (0 or 1 for single bit,  0-3 or 0-N for multiple bits)
    .rightShift(start_bit))


def add_sr_aero_mask(image):
  """Creates a binary maks for any pixels in Landsat 8 and 9 that have 'medium' 
  or 'high' aerosol QA flags from the SR_QA_AEROSOL band

  Args:
      image: ee.Image of an ee.ImageCollection

  Returns:
      ee.Image with additional band called 'aero', where pixels are given a value of 1
      if the aerosol QA flag is climatology or low (keep) and 0 if medium or high (mask)
  """
  aerosolQA = image.select('aerosol_qa')
  # bit 7 is set where aerosol level (bits 6-7) is medium (10) or high (11)
  medHighAero = aerosolQA.bitwiseAnd(1 << 7)
  sr_aero_mask = medHighAero.eq(0).rename('aero')
  return image.addBands(sr_aero_mask)


def Mndwi(image):
  """calculate the modified normalized difference water index per pixel

  Args:
      image: ee.Image of an ee.ImageCollection

  Returns:
      band where values calculated are the MNDWI value per pixel
  """
  return (image.expression('(GREEN - SWIR1) / (GREEN + SWIR1)', {
    'GREEN': image.select(['Green']),
    'SWIR1': image.select(['Swir1'])
  })).rename('mndwi')
  

def Mbsrv(image):
  """calculate the multi-band spectral relationship visible per pixel

  Args:
      image: ee.Image of an ee.ImageCollection

  Returns:
      band where values calculated are the MBSRV value per pixel
  """
  return (image.select(['Green']).add(image.select(['Red'])).rename('mbsrv'))


def Mbsrn(image):
  """calculate the multi-band spectral relationship near infrared per pixel

  Args:
      image: ee.Image of an ee.ImageCollection

  Returns:
      band where values calculated are the MBSRN value per pixel
  """
  return (image.select(['Nir']).add(image.select(['Swir1'])).rename('mbsrn'))


def Ndvi(image):
  """calculate the normalized difference vegetation index per pixel

  Args:
      image: ee.Image of an ee.ImageCollection

  Returns:
      band where values calculated are the NDVI value per pixel
  """
  return (image.expression('(NIR - RED) / (NIR + RED)', {
    'RED': image.select(['Red']),
    'NIR': image.select(['Nir'])
  })).rename('ndvi')


def Awesh(image):
  """calculate the automated water extent shadow per pixel

  Args:
      image: ee.Image of an ee.ImageCollection

  Returns:
      band where values calculated are the AWESH value per pixel
  """
  return (image.expression('Blue + 2.5 * Green + (-1.5) * mbsrn + (-0.25) * Swir2', {
    'Blue': image.select(['Blue']),
    'Green': image.select(['Green']),
    'mbsrn': Mbsrn(image).select(['mbsrn']),
    'Swir2': image.select(['Swir2'])
  })).rename('awesh')


## The DSWE Function itself    
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
  # Inputs are meant to be scaled reflectance values 
  t1 = mndwi.gt(0.124) # MNDWI greater than Wetness Index Threshold
  t2 = mbsrv.gt(mbsrn) # MBSRV greater than MBSRN
  t3 = awesh.gt(0) #AWESH greater than 0
  t4 = (mndwi.gt(-0.44)  #Partial Surface Water 1 thresholds
   .And(swir1.lt(0.09))
   .And(nir.lt(0.15)) 
   .And(ndvi.lt(0.7)))
  t5 = (mndwi.gt(-0.5) #Partial Surface Water 2 thresholds
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


def calc_hill_shades(image, geo):
  """ caluclate the hill shade per pixel

  Args:
      image: ee.Image of an ee.ImageCollection
      geo: geometry of the WRS tile as wrs.geometry() in script

  Returns:
      a band named 'hillShade' where values calculated are the hill shade per 
      pixel. output is 0-255. 
  """
  MergedDEM = ee.Image("MERIT/DEM/v1_0_3").clip(geo.buffer(3000))
  hillShade = ee.Terrain.hillshade(MergedDEM, 
    ee.Number(image.get('SUN_AZIMUTH')), 
    ee.Number(image.get('SUN_ELEVATION')))
  hillShade = hillShade.rename(['hillShade'])
  return hillShade


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
  # neighborhood size of 100 pixels (~3 km at the 30 m reduction scale). With
  # 30 pixels (~900 m), shadows cast by terrain farther away were missed or not
  # depending on how GEE split up the request, e.g. at low sun in SE Alaska,
  # where a site could be 0% or 93% shadowed. 100 removes these large errors
  # for ~2% more EECU; 200 was no more accurate (see
  # scratch/gee_test/hillshadow_scenes.py)
  hillShadow = ee.Terrain.hillShadow(MergedDEM,
    ee.Number(image.get('SUN_AZIMUTH')),
    ee.Number(90).subtract(image.get('SUN_ELEVATION')),
    100)
  hillShadow = hillShadow.rename(['hillShadow'])
  return hillShadow


def remove_geo(image):
  """ Funciton to remove the geometry from an ee.Image
  
  Args:
      image: ee.Image of an ee.ImageCollection
      
  Returns:
      ee.Image with the geometry removed
  """
  return image.setGeometry(None)




## Set up the combined reflectance pull
# The DSWE1, DSWE1a, and DSWE3 pulls share all of the per-image processing
# (masks, hillshade, hillshadow, DSWE) and differ only in the water mask used
# for the summaries and diagnostic counts. Here that shared processing is done
# once per image and summaries for each DSWE variant are calculated in a single
# reduceRegions call. Variant-specific outputs are prefixed with the variant
# name and two underscores (e.g. 'DSWE1a__med_Blue'); outputs that are the same
# for every variant (pCount_dswe*, prop_clouds, prop_hillShadow, mean_hillShade)
# are not prefixed. Once split by variant (see split_combined_dswe() in
# c_collate_Landsat_data/src/), the output has the same format as the
# per-variant exports of previous versions of this script, and for the same
# site order and hillshadow settings, the same values.

def get_variant_cols(group, variants):
  """ Get the output column names of ref_pull_combined, in the order of the
  reducer outputs

  Args:
      group: 'LS457' or 'LS89'
      variants: list of DSWE variants, any of 'DSWE1', 'DSWE1a', 'DSWE3'

  Returns:
      dictionary of lists of band names for each summary type
  """
  if group == 'LS457':
    sr_bands = ['Blue', 'Green', 'Red', 'Nir', 'Swir1', 'Swir2', 'SurfaceTemp']
    diag = ['high_opac', 'unreal_val', 'sun_glint', 'ir_glint']
  else:
    sr_bands = ['Aerosol', 'Blue', 'Green', 'Red', 'Nir', 'Swir1', 'Swir2', 'SurfaceTemp']
    diag = ['high_aero', 'unreal_val', 'sun_glint', 'ir_glint', 'aero_zero', 'aero_thresh']
  diag = diag + ['blue_zero', 'blue_thresh', 'green_zero', 'green_thresh',
    'red_zero', 'red_thresh', 'nir_zero', 'nir_thresh',
    'swir1_zero', 'swir1_thresh', 'swir2_zero', 'swir2_thresh',
    'blue_glint', 'green_glint', 'red_glint', 'nir_glint', 'swir1_glint', 'swir2_glint',
    'nir_ir_glint', 'swir1_ir_glint', 'swir2_ir_glint']
  return {
    'sr_bands': sr_bands,
    'med': [v + '__med_' + b for v in variants for b in sr_bands],
    'min': [v + '__min_SurfaceTemp' for v in variants],
    'sd': [v + '__sd_' + b for v in variants for b in sr_bands],
    'mean': [v + '__mean_' + b for v in variants for b in sr_bands],
    'diag': diag,
    'pCount': [v + '__pCount_' + x for v in variants for x in diag]
  }


def ref_pull_combined(image, feat, group, variants):
  """ This function applies all functions to the Landsat 4-7 or 8-9
  ee.ImageCollection, extracting summary statistics for each geometry area for
  each requested DSWE variant: DSWE1 (high confidence water), DSWE1a (DSWE1 or
  algal threshold met), DSWE3 (vegetated water)

  Args:
      image: ee.Image of an ee.ImageCollection
      feat: ee.FeatureGeometry of the buffered locations
      group: 'LS457' or 'LS89'
      variants: list of DSWE variants, any of 'DSWE1', 'DSWE1a', 'DSWE3'

  Returns:
      summaries for band data within any given geometry area for each DSWE
      variant
  """
  cols = get_variant_cols(group, variants)
  # process image with cfmask
  # where the mask is >= 1 (dilated cloud, cloud, cloud shadow, snow)
  # call that 1 (otherwise 0) and rename as clouds.
  clouds = add_cf_mask(image).select('cfmask').gte(1).rename('clouds')
  # add mask FOR low opacity (LS457) or low aerosol (LS89), realistic values,
  # sun glint, ir glint
  if group == 'LS457':
    atmos = add_opac_mask(image).select('opac').eq(1).rename('low_opac')
  else:
    atmos = add_sr_aero_mask(image).select('aero').eq(1).rename('low_aero')
  real = add_realistic_mask(image).select('real').eq(1).rename('is_real')
  no_glint = add_sun_glint_mask(image).select('no_glint').eq(1)
  ir_glint = add_ir_glint_flag(image).select('ir_glint').eq(1)
  # calculate hillshade
  h = calc_hill_shades(image, wrs.geometry()).select('hillShade')
  # calculate hillshadow
  hs = calc_hill_shadows(image, wrs.geometry()).select('hillShadow')
  # apply dswe function
  d = DSWE(image).select('dswe')

  def qa_mask(img):
    # hs = 1, fully illuminated pixels; add cloud, opac/aero, real and glint
    return (img
      .updateMask(hs.eq(1))
      .updateMask(clouds.eq(0))
      .updateMask(atmos.eq(1))
      .updateMask(real.eq(1))
      .updateMask(no_glint.eq(1))
      .selfMask())

  # create additive masks for dswe>0 (water of any type), dswe==1 (confident
  # open water), and dswe==3 (confident vegetated water)
  gt0 = qa_mask(d.gt(0).rename('dswe_gt0'))
  dswe1 = qa_mask(d.eq(1).rename('dswe1'))
  dswe3 = qa_mask(d.eq(3).rename('dswe3'))

  # define algae where DSWE > 1 and red/green threshold met
  grn_alg_thrsh = image.select('Green').gt(0.05)
  red_alg_thrsh = image.select('Red').lt(0.04)
  alg = (d.gt(1).rename('algae')
    .And(grn_alg_thrsh.eq(1))
    .And(red_alg_thrsh.eq(1))
    # add cloud, opac/aero and real
    .updateMask(clouds.eq(0))
    .updateMask(atmos.eq(1))
    .updateMask(real.eq(1))
    .updateMask(no_glint.eq(1))
    )

  # create additive mask for dswe1a: dswe = 1 or algal threshold met
  dswe1a = qa_mask(d.eq(1).Or(alg.eq(1)).rename('dswe1a'))

  # the summary mask and the water area for the diagnostic counts per variant.
  # The DSWE1a water area uses the algal thresholds directly rather than 'alg',
  # because 'alg' has the realistic value and sun glint masks applied and would
  # hide the pixels being counted
  summary_mask = {'DSWE1': dswe1, 'DSWE1a': dswe1a, 'DSWE3': dswe3}
  water_area = {
    'DSWE1': d.eq(1),
    'DSWE1a': d.eq(1).Or(d.gt(1).And(grn_alg_thrsh.eq(1)).And(red_alg_thrsh.eq(1))),
    'DSWE3': d.eq(3)
  }

  # diagnostic flags, prior to masking to the water area
  band_flags = {
    'blue_zero': image.select('Blue').lt(0),
    'blue_thresh': image.select('Blue').lt(-0.01),
    'green_zero': image.select('Green').lt(0),
    'green_thresh': image.select('Green').lt(-0.01),
    'red_zero': image.select('Red').lt(0),
    'red_thresh': image.select('Red').lt(-0.01),
    'nir_zero': image.select('Nir').lt(0),
    'nir_thresh': image.select('Nir').lt(-0.01),
    'swir1_zero': image.select('Swir1').lt(0),
    'swir1_thresh': image.select('Swir1').lt(-0.01),
    'swir2_zero': image.select('Swir2').lt(0),
    'swir2_thresh': image.select('Swir2').lt(-0.01),
    # >= 0.2
    'blue_glint': image.select('Blue').gte(0.2),
    'green_glint': image.select('Green').gte(0.2),
    'red_glint': image.select('Red').gte(0.2),
    'nir_glint': image.select('Nir').gte(0.2),
    'swir1_glint': image.select('Swir1').gte(0.2),
    'swir2_glint': image.select('Swir2').gte(0.2),
    # ir bands >= 0.1
    'nir_ir_glint': image.select('Nir').gte(0.1),
    'swir1_ir_glint': image.select('Swir1').gte(0.1),
    'swir2_ir_glint': image.select('Swir2').gte(0.1),
    # pixels that fail the realistic value or sun glint masks, or are flagged
    # for ir glint
    'unreal_val': real.eq(0),
    'sun_glint': no_glint.eq(0),
    'ir_glint': ir_glint
  }
  if group == 'LS89':
    band_flags['aero_zero'] = image.select('Aerosol').lt(0)
    band_flags['aero_thresh'] = image.select('Aerosol').lt(-0.01)

  # summary and diagnostic bands per variant
  sr = cols['sr_bands']
  variant_bands = {}
  for v in variants:
    summ = (image.select(sr, [v + '__med_' + b for b in sr])
      .addBands(image.select(['SurfaceTemp'], [v + '__min_SurfaceTemp']))
      .addBands(image.select(sr, [v + '__sd_' + b for b in sr]))
      .addBands(image.select(sr, [v + '__mean_' + b for b in sr]))
      .updateMask(summary_mask[v]))
    w = water_area[v]
    for x in cols['diag']:
      if x in ['high_opac', 'high_aero']:
        # pixels within the water area that fail the opacity/aerosol mask
        # (hillshadow and cloud masks applied)
        b = (atmos.eq(0).rename(v + '__pCount_' + x)
          .updateMask(hs.eq(1)).updateMask(clouds.eq(0)).updateMask(w).selfMask())
      else:
        # hillshadow, cloud and opacity/aerosol masks applied
        b = (band_flags[x].rename(v + '__pCount_' + x)
          .updateMask(hs.eq(1)).updateMask(clouds.eq(0)).updateMask(atmos.eq(1))
          .updateMask(w).selfMask())
      summ = summ.addBands(b)
    variant_bands[v] = summ

  allBands = variant_bands[variants[0]]
  for v in variants[1:]:
    allBands = allBands.addBands(variant_bands[v])
  allBands = (allBands
    # add bands back in for QA (prior to masking of dswe/hs/f/r)
    .addBands(gt0)
    .addBands(dswe1)
    .addBands(dswe3)
    .addBands(dswe1a)
    .addBands(clouds)
    .addBands(hs.eq(0).rename('hillShadow')) # invert so prop_hillShadow = proportion shaded
    .addBands(h)
    )

  # reduceRegions matches image bands to reducer inputs by position, so order
  # the bands to match the reducer below. A summary band stays first so the
  # default projection is the Landsat projection, as in ref_pull_*
  pixOut = allBands.select(cols['med'] + cols['min'] + cols['sd'] + cols['mean']
    + cols['pCount'] + ['dswe_gt0', 'dswe1', 'dswe3', 'dswe1a']
    + ['clouds', 'hillShadow', 'hillShade'])

  combinedReducer = (ee.Reducer.median().unweighted()
      .forEachBand(pixOut.select(cols['med']))
    .combine(ee.Reducer.min().unweighted()
      .forEachBand(pixOut.select(cols['min'])), sharedInputs = False)
    .combine(ee.Reducer.stdDev().unweighted()
      .forEachBand(pixOut.select(cols['sd'])), sharedInputs = False)
    .combine(ee.Reducer.mean().unweighted()
      .forEachBand(pixOut.select(cols['mean'])), sharedInputs = False)
    .combine(ee.Reducer.count().unweighted()
      .forEachBand(pixOut.select(cols['pCount'])), sharedInputs = False)
    .combine(ee.Reducer.count().unweighted()
      .forEachBand(pixOut.select(['dswe_gt0', 'dswe1', 'dswe3', 'dswe1a'])),
      outputPrefix = 'pCount_', sharedInputs = False)
    .combine(ee.Reducer.mean().unweighted()
      .forEachBand(pixOut.select(['clouds', 'hillShadow'])),
      outputPrefix = 'prop_', sharedInputs = False)
    .combine(ee.Reducer.mean().unweighted()
      .forEachBand(pixOut.select(['hillShade'])),
      outputPrefix = 'mean_', sharedInputs = False)
    )
  # Collect median reflectance and occurance values
  # Make a cloud score, and get the water pixel count
  lsout = (pixOut.reduceRegions(feat, combinedReducer, 30))
  out = lsout.map(remove_geo)
  return out


def maximum_no_of_tasks(MaxNActive, waitingPeriod):
  """ Function to limit the number of tasks sent to Earth Engine at one time to avoid time out errors
  
  Args:
      MaxNActive: maximum number of tasks that can be active in Earth Engine at one time
      waitingPeriod: time to wait between checking if tasks are completed, in seconds
      
  Returns:
      None.
  """
  ##maintain a maximum number of active tasks
  ## initialize submitting jobs
  ts = list(ee.batch.Task.list())
  NActive = 0
  for task in ts:
     if ('RUNNING' in str(task) or 'READY' in str(task)):
         NActive += 1
  ## wait if the number of current active tasks reach the maximum number
  ## defined in MaxNActive
  while (NActive >= MaxNActive):
    # if reach or over maximum no. of active tasks, wait for a certain amount 
    # of time ('waitingPeriod') and check again
    time.sleep(waitingPeriod) 
    ts = list(ee.batch.Task.list())
    NActive = 0
    for task in ts:
      if ('RUNNING' in str(task) or 'READY' in str(task)):
        NActive += 1
  return()


##############################################
##---- IMPORT CONFIG VARIABLES          ----##
##############################################

# Optional overrides, set as environment variables. These do not change the
# config file, so they do not invalidate any {targets}. Python reads the
# environment when it starts, so from R either use Sys.setenv() before Python is
# initialized (i.e. before python/pySetup.R), or set them in Python directly
# (e.g. reticulate::import("os")$environ$`__setitem__`(name, value)):
#   LAKESR_EE_PROJ: ee project to run the tasks from, in place of ee_proj in the
#     config (e.g. when the configured project has used its EECU quota)
#   LAKESR_YML_CSV: formatted yml file, in place of b_pull_Landsat_SRST_poi/mid/yml.csv
#   LAKESR_PATHROW_FILE: current path-row file, in place of
#     b_pull_Landsat_SRST_poi/out/current_pathrow.txt
yml_fn = os.environ.get("LAKESR_YML_CSV") or "b_pull_Landsat_SRST_poi/mid/yml.csv"
pathrow_fn = os.environ.get("LAKESR_PATHROW_FILE") or "b_pull_Landsat_SRST_poi/out/current_pathrow.txt"

# get locations and yml from data folder
yml = read_csv(yml_fn)

eeproj = os.environ.get("LAKESR_EE_PROJ") or yml["ee_proj"][0]
#initialize GEE
ee.Initialize(project = eeproj)

# get EE/Google settings from yml file
proj = yml["proj"][0]
proj_folder = yml["proj_folder"][0]

# get/save start date
yml_start = yml["start_date"][0]
yml_end = yml["end_date"][0]

# set yml_end as date
if yml_end == "today":
  yml_end = date.today().strftime("%Y-%m-%d")

# store run date for versioning
run_date = yml["run_date"][0]

# gee processing settings
buffer = yml["site_buffer"][0]
# cloud_filter may be read as a bool or a "True"/"False" string
cloud_filt = str(yml["cloud_filter"][0]).strip().lower() == "true"
cloud_thresh = yml["cloud_thresh"][0]

# create folder with version number
folder_version = proj_folder + "_v" + run_date

# get and format dswe value
try: 
  dswe = yml["DSWE_setting"][0].astype(str)
except AttributeError: 
  dswe = yml["DSWE_setting"][0]

# get extent info
extent = yml["extent"][0]

# get current tile
with open(pathrow_fn, "r") as file:
  pr = file.read()

print("Using " + yml_fn + " (run_date " + str(run_date) + "), path-row " + pr +
      ", ee project " + eeproj)

# create file name of location data
locs_fn = os.path.join("b_pull_Landsat_SRST_poi/out/locations/", ("locations_" + pr + ".csv"))

# read in locations file
locations_subset = read_csv(locs_fn)

##############################################
##---- CREATING EE FEATURECOLLECTIONS   ----##
##############################################

wrs = (ee.FeatureCollection('projects/ee-ls-c2-srst/assets/WRS2_descending')
  .filterMetadata('PR', 'equals', pr))

# store path and row for subsetting the stacks so there is not overlap between PR pulls
w_p = int(str(pr)[:3])
w_r = int(str(pr)[-3:])

#grab images and apply scaling factors
l7 = (ee.ImageCollection("LANDSAT/LE07/C02/T1_L2")
    .filter(ee.Filter.eq("WRS_PATH", w_p))
    .filter(ee.Filter.eq("WRS_ROW", w_r))
    .filterDate(yml_start, yml_end)
    .filterDate('1999-05-28', '2019-12-31')) # truncated to reflect drift issues
l5 = (ee.ImageCollection("LANDSAT/LT05/C02/T1_L2")
    .filter(ee.Filter.eq("WRS_PATH", w_p))
    .filter(ee.Filter.eq("WRS_ROW", w_r))
    .filterDate(yml_start, "2011-11-18")) # end of science mission
l4 = (ee.ImageCollection("LANDSAT/LT04/C02/T1_L2")
    .filter(ee.Filter.eq("WRS_PATH", w_p))
    .filter(ee.Filter.eq("WRS_ROW", w_r))
    .filterDate(yml_start, "1993-12-14")) # end of science mission
    
# merge collections by image processing groups
ls457 = ee.ImageCollection(l4.merge(l5).merge(l7))

# filter scenes by scene-level cloud cover if configured
if cloud_filt:
  ls457 = ls457.filter(ee.Filter.lt("CLOUD_COVER", ee.Number.parse(str(cloud_thresh))))
    
# existing band names
bn457 = (["SR_B1", "SR_B2", "SR_B3", "SR_B4", "SR_B5", "SR_B7", 
  "QA_PIXEL", "SR_ATMOS_OPACITY", "QA_RADSAT", "ST_B6"])
  
# new band names
bns457 = (["Blue", "Green", "Red", "Nir", "Swir1", "Swir2", 
  "pixel_qa", "opacity_qa", "radsat_qa", "SurfaceTemp"])
  


#grab images and apply scaling factors
l8 = (ee.ImageCollection("LANDSAT/LC08/C02/T1_L2")
    .filter(ee.Filter.eq("WRS_PATH", w_p))
    .filter(ee.Filter.eq("WRS_ROW", w_r))
    .filterDate(yml_start, yml_end))
l9 = (ee.ImageCollection("LANDSAT/LC09/C02/T1_L2")
    .filter(ee.Filter.eq("WRS_PATH", w_p))
    .filter(ee.Filter.eq("WRS_ROW", w_r))
    .filterDate(yml_start, yml_end))

# merge collections by image processing groups
ls89 = ee.ImageCollection(l8.merge(l9))

# filter scenes by scene-level cloud cover if configured
if cloud_filt:
  ls89 = ls89.filter(ee.Filter.lt("CLOUD_COVER", ee.Number.parse(str(cloud_thresh))))
    
# existing band names
bn89 = (["SR_B1", "SR_B2", "SR_B3", "SR_B4", "SR_B5", "SR_B6", "SR_B7", 
  "QA_PIXEL", "SR_QA_AEROSOL", "QA_RADSAT", "ST_B10"])
  
# new band names
bns89 = (["Aerosol", "Blue", "Green", "Red", "Nir", "Swir1", "Swir2",
  "pixel_qa", "aerosol_qa", "radsat_qa", "SurfaceTemp"])
 


##########################################
##---- LANDSAT ACQUISITION          ----##
##########################################

# get the DSWE variants to summarize from the DSWE setting. 1a also obtains
# DSWE1 for downstream comparison
variants = []
if '1' in dswe:
  variants.append('DSWE1')
if '1a' in dswe:
  variants.append('DSWE1a')
if '3' in dswe:
  variants.append('DSWE3')
# file name label for the combined export, e.g. 'DSWEcombo1-1a'. This does not
# match '_DSWE1_', '_DSWE1a_', or '_DSWE3_', so the combined files can not be
# mistaken for the per-variant files during collation
variant_label = 'DSWEcombo' + '-'.join(v.replace('DSWE', '') for v in variants)


def send_combined_task(stack, feat, group, chunk):
  """ Send the combined DSWE export task for one mission group and chunk

  Args:
      stack: pre-processed ee.ImageCollection for the mission group
      feat: ee.FeatureCollection of the buffered locations
      group: 'LS457' or 'LS89'
      chunk: chunk number

  Returns:
      None. Starts the export task.
  """
  cols = get_variant_cols(group, variants)
  locs_out = stack.map(lambda image: ref_pull_combined(image, feat, group, variants)).flatten()
  # keep a site-scene if any variant has a summary (same as the notNull filter
  # applied to each per-variant export)
  locs_out = locs_out.filter(ee.Filter.Or(*[ee.Filter.notNull([v + '__med_Blue']) for v in variants]))
  locs_srname = proj+'_site_'+group+'_C2_SRST_'+variant_label+'_'+str(pr)+'_'+str(chunk)+'_v'+run_date
  locs_dataOut = (ee.batch.Export.table.toDrive(collection = locs_out,
                                          description = locs_srname,
                                          folder = folder_version,
                                          fileFormat = 'csv',
                                          selectors = (['system:index']
                                            + cols['med'] + cols['min'] + cols['sd'] + cols['mean']
                                            + ['pCount_dswe_gt0', 'pCount_dswe1', 'pCount_dswe3', 'pCount_dswe1a']
                                            + cols['pCount']
                                            + ['prop_clouds', 'prop_hillShadow', 'mean_hillShade'])))
  #Send next task.
  locs_dataOut.start()
  print('Task sent: ' + group + ' ' + ', '.join(variants) + ' acquisitions for site configuration at tile ' + str(pr) + ' chunk ' + str(chunk + 1))


# Map the pull function over the 5000 or so created sites, as needed
def process_subset(df_subset, chunk, chunk_size):
  """
  This function processes a subset of the DataFrame, sending one combined DSWE
  task per mission group
  """

  # Check how many existing tasks are running and take a break of 120 secs if
  # it's more than 10, and keep checking to not overload the GEE server
  maximum_no_of_tasks(10, 120)

  locs_feature = csv_to_eeFeat(df_subset, yml['location_crs'][0])

  geo = wrs.geometry()

  ## get locs feature and buffer ##
  feat = (locs_feature
    .filterBounds(geo)
    .map(dp_buff))

  if len(variants) == 0:
    print('Not configured to acquire any DSWE stack for site configuration')
    return()

  ## pre-process 457 stack
  #snip the ls data by the geometry of the location points
  locs_stack_ls457 = (ls457
    .filterBounds(feat.geometry())
    # apply fill mask and scaling factors
    .map(apply_fill_mask_457)
    .map(apply_scale_factors))
  # rename bands for ease
  locs_stack_ls457 = locs_stack_ls457.select(bn457, bns457)
  # apply masks that require above rename
  locs_stack_ls457 = (locs_stack_ls457
    .map(apply_rad_mask))
  send_combined_task(locs_stack_ls457, feat, 'LS457', chunk)

  # pre-process the ls89 stack
  # snip the ls data by the geometry of the location points
  locs_stack_ls89 = (ls89
    .filterBounds(feat.geometry())
    # apply fill mask and scaling factors
    .map(apply_fill_mask_89)
    .map(apply_scale_factors))
  # rename bands for ease
  locs_stack_ls89 = locs_stack_ls89.select(bn89, bns89)
  # apply masks that require above rename
  locs_stack_ls89 = (locs_stack_ls89
    .map(apply_rad_mask))
  send_combined_task(locs_stack_ls89, feat, 'LS89', chunk)


def process_dataframe_in_chunks(df, chunk_size=5000):
    """
    Process a DataFrame in chunks of specified size.
    
    Args:
    df (pandas.DataFrame): The input DataFrame
    chunk_size (int): The number of rows in each chunk (default: 5000)
    
    Returns:
    list: A list of results from processing each chunk
    """

    # Calculate the number of chunks
    num_chunks = len(df) // chunk_size + (1 if len(df) % chunk_size != 0 else 0)
    
    for i in range(num_chunks):
        # Calculate start and end indices for the current chunk
        start_idx = i * chunk_size
        end_idx = min((i + 1) * chunk_size, len(df))
        
        # Subset the DataFrame
        df_subset = df.iloc[start_idx:end_idx]
        
        # Process the subset and store the result
        result = process_subset(df_subset, i, chunk_size)

        print(f"Processed chunk {i+1}/{num_chunks}")
    
    return ()

def hilbert_index(x, y, order = 16):
  """ Position of a point along a Hilbert curve, which keeps points that are
  close together in space close together in the ordering

  Args:
      x, y: integer grid coordinates, between 0 and 2^order - 1
      order: number of bits per coordinate

  Returns:
      integer position along the curve
  """
  d = 0
  s = 1 << (order - 1)
  while s > 0:
    rx = 1 if (x & s) > 0 else 0
    ry = 1 if (y & s) > 0 else 0
    d += s * s * ((3 * rx) ^ ry)
    # rotate the quadrant
    if ry == 0:
      if rx == 1:
        x = s - 1 - x
        y = s - 1 - y
      x, y = y, x
    s >>= 1
  return d


def sort_spatially(df):
  """ Sort locations along a Hilbert curve so that each chunk of locations is
  spatially compact. Chunks then overlap less, so fewer image and DEM tiles
  (and the per-tile hillshade, hillshadow, and DSWE calculations) are computed
  for more than one chunk. This only changes which chunk a location is in, not
  its output.

  Args:
      df: locations dataframe with Latitude and Longitude

  Returns:
      df sorted along a Hilbert curve (ties broken by id)
  """
  n = (1 << 16) - 1
  lon_range = max(df['Longitude'].max() - df['Longitude'].min(), 1e-9)
  lat_range = max(df['Latitude'].max() - df['Latitude'].min(), 1e-9)
  xi = ((df['Longitude'] - df['Longitude'].min()) / lon_range * n).astype(int)
  yi = ((df['Latitude'] - df['Latitude'].min()) / lat_range * n).astype(int)
  h = [hilbert_index(x, y) for x, y in zip(xi, yi)]
  return (df.assign(hilbert = h)
    .sort_values(['hilbert', 'id'], kind = 'mergesort')
    .drop(columns = 'hilbert')
    .reset_index(drop = True))


# and then actualy process the chunks! Only path-rows with more than one chunk
# are affected by the spatial sort
process_dataframe_in_chunks(sort_spatially(locations_subset))
   

##############################################
##---- LANDSAT 457 METADATA ACQUISITION ----##
##############################################

## get metadata ##
meta_srname_457 = proj+"_metadata_LS457_C2_"+str(pr)+"_v"+run_date
meta_dataOut_457 = (ee.batch.Export.table.toDrive(collection = ls457,
                                        description = meta_srname_457,
                                        folder = folder_version,
                                        fileFormat = "csv"))

#Send next task.                                        
meta_dataOut_457.start()


#############################################
##---- LANDSAT 89 METADATA ACQUISITION ----##
#############################################


## get metadata ##
meta_srname_89 = proj+"_metadata_LS89_C2_"+str(pr)+"_v"+run_date
meta_dataOut_89 = (ee.batch.Export.table.toDrive(collection = ls89,
                                        description = meta_srname_89,
                                        folder = folder_version,
                                        fileFormat = "csv"))

#Send next task.                                        
meta_dataOut_89.start()

print("Task sent: metadata acquisition for tile " +str(pr))
