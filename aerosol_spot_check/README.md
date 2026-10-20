# Landsat 8/9 aerosol QA spot check

A standalone workflow, **not part of the {targets} pipeline**, to decide how
lakeSR should handle Landsat 8/9 pixels with medium and high aerosol levels in
`SR_QA_AEROSOL`. Background: AquaSat/AquaMatch_siteSR#47. That issue reported
that the production aerosol mask is inverted (it keeps medium/high-aerosol
pixels instead of removing them) and asked whether medium-aerosol pixels should
be masked at all.

For a representative sample of lakes, this workflow pulls Landsat 8/9 DSWE 1
water pixels with all production masks **except** the aerosol mask. It then
summarizes:

- the share of water pixels at each aerosol level (climatology, low, medium,
  high) by ecoregion, season, smoke vs. non-smoke year, and satellite
- how often water pixels carry the interpolated-aerosol flag
- how many site-scenes meet the production minimum pixel count under each rule:
  no aerosol mask, mask high only, mask medium and high (documented), and the
  inverted rule used to create the released data
- how reflectance differs by aerosol level, and how much the choice of rule
  changes the reported median reflectance

## Sample design

`01_select_sample.R` selects 25 WRS2 path-rows and up to 50 lakes per
path-row (set in `config_aero_check.yml`):

1. Each lakeSR path-row is assigned to the EPA/CEC Level I ecoregion holding
   most of its lakes. Hawaii and the territories form their own stratum. Small,
   mostly Mexican ecoregions that only clip the US are merged into their US
   neighbor (see `merge_ecoregions` in the config).
2. Path-rows are allocated to ecoregions in proportion to the square root of
   their lake counts, with at least one per ecoregion. Alaska holds more than
   half of all lakeSR lakes, and the square root keeps it from dominating the
   sample.
3. Within an ecoregion, path-rows are drawn with probability proportional to
   their lake count. Where possible, path-rows within 250 km of one already
   drawn are skipped.
4. Lakes are sampled at random within each path-row.

`out/sample_design_summary.csv` and `out/sample_map.png` compare the sample to
the full lakeSR population. The pull covers 2019–2023: smoke years 2020, 2021
and 2023, and non-smoke years 2019 and 2022.

## How to run

Run each step from the repository root:

1. `01_select_sample.R`: needs the per-path-row location files in
   `b_pull_Landsat_SRST_poi/out/locations/` from the targets pipeline. Writes
   the sample to `out/`.
2. `02_run_aero_check.R`: activates the pipeline's conda environment
   (`python/pySetup.R`) and sends one Earth Engine export per path-row to the
   Drive folder `lakeSR_aerosol_spot_check_v<run_date>`.
3. Once the tasks are done (https://code.earthengine.google.com/tasks), run
   `03_download_results.R`.
4. Knit `04_analyze_aero_check.Rmd`.

`py/aero_check_per_pathrow.py` copies its helper functions from
`b_pull_Landsat_SRST_poi/py/run_GEE_per_pathrow.py` (commit 32f0012), because
the production script runs a full pull when it is imported. If production
masking changes, update this copy to match.

## Output columns (per lake × scene)

| column | description |
|---|---|
| `pCount_water` | DSWE 1 water pixels after cloud, hillshadow, realistic-value and sun glint masks (no aerosol mask) |
| `pCount_clim`, `pCount_low`, `pCount_med`, `pCount_high` | water pixels at each aerosol level (bits 6–7) |
| `pCount_interp` | water pixels flagged as interpolated aerosol (bit 5) |
| `pCount_valid` | water pixels flagged as a valid aerosol retrieval (bit 1) |
| `med_<band>_<level>` | median reflectance of water pixels at that aerosol level |
| `med_<band>_<rule>` | median reflectance under `nomask`, `nohigh` (mask high) or `nomedhigh` (mask medium and high) |
