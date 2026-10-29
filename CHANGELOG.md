# Changelog

All notable changes to this repository are documented in this file. Releases
are tagged by date (`vYYYY-MM-DD`) rather than semantic version, matching this
repo's [GitHub releases](https://github.com/AquaSat/AquaMatch_lakeSR/releases).

## [Unreleased]

### Fixed
- **Distance to shore and shoreline flags were wrong for Alaska and Hawaii**
  (`a_Calculate_Centers/src/calculate_bestres_centers.R`): the UTM EPSG code
  was built as `paste0("EPSG:326", zone)`, so UTM zones 1-9 (all of Alaska and
  Hawaii) were projected to EPSG:3261-3269, which are not UTM zones. POI
  locations were unaffected (they were transformed back through the same
  CRS; median shift in the new run is < 1 cm), but `poi_dist_m` was
  overestimated 10-30x in HUC2 19 and ~3x in HUC2 20. As a result, 0% of
  Alaska and 44% of Hawaii waterbodies were flagged by
  `flag_optical_shoreline` in the 2025-02-12 locations, versus 92% and 93% in
  the 2026-10-20 locations. The EPSG code is now `32600 + zone` (`32700 + zone`
  in the southern hemisphere). Puerto Rico and the Pacific territories (zones
  19, 20, 55) were not affected.
- The same UTM EPSG error was in the path-row containment check
  (`b_pull_Landsat_SRST_poi/src/check_if_fully_within_pr.R`), so for Alaska and
  Hawaii path-rows the site buffer was drawn in the wrong CRS when checking
  whether it was fully within the path-row. This is now fixed (e.g. path-row
  056019 gains one site).
- **POI ignored islands and could join polygon parts** (new
  `a_Calculate_Centers/src/calculate_poi.R`): all rings of a polygon were
  passed to `polylabelr::poi()` as a single ring, so island shorelines were
  not counted as shore, and multi-part polygons were joined into one shape.
  Each polygon part is now passed with its islands as holes, the part with the
  greatest distance to shore is kept, and each POI is checked to be within its
  own polygon (falling back to `sf::st_point_on_surface()` with a warning
  otherwise; no waterbodies in the 2026-10-20 locations needed the fallback).
- **Roy intermission handoff calculation** (`e_calculate_handoffs/src/get_matches.R`,
  `calculate_roy_handoff.R`): matched early/late-mission records shared
  unprefixed column names (`date`, `sat_id`, `source`, `prop_clouds`,
  `flag_temp_min/max`, `mission`) after the join, so filters and the
  surface-temperature NA check could silently operate on the wrong mission's
  data. All columns are now prefixed `early_`/`late_` at read-in, removing the
  ambiguity. Model x/y assignment was also corrected to consistently fit
  `sat_to ~ sat_corr`, so fitted coefficients apply forward without needing
  inversion downstream. This matches the original intent of the Roy handoff calculation 
  and mirrors the application for the Gardner handoff calculation. This impacted 42 
  lines of the handoff csv file.
- All DSWE1 Roy handoff coefficients and diagnostic figures were recalculated
  as a result; values differ from the previous release.
- **`prop_hillShadow` was inverted** (`b_pull_Landsat_SRST_poi/py/run_GEE_per_pathrow.py`):
  it was the mean of the `ee.Terrain.hillShadow` band (1 = illuminated), so it
  reported the proportion of the buffer that was *illuminated*, not shaded as
  documented. The band is now inverted before the reduction, so
  `prop_hillShadow` is the proportion of the buffer shaded by terrain. Values
  in previous releases equal 1 minus the documented value. This is a diagnostic
  tally only; masking of summarized pixels was not affected. 
- **Landsat 8/9 aerosol mask was inverted** (`b_pull_Landsat_SRST_poi/py/run_GEE_per_pathrow.py`,
  AquaSat/AquaMatch_siteSR#47, #66): `add_sr_aero_mask` returns 1 for
  climatology/low aerosol, but the `ref_pull_89_*` functions took `.eq(0)` of
  it, so medium/high-aerosol pixels were *kept* and climatology/low pixels
  were masked. The `ref_pull_89_*` functions now take `.eq(1)`, so
  medium/high-aerosol pixels are masked as documented. `pCount_high_aero`
  was inverted in the same way and now counts medium/high-aerosol pixels.
  All Landsat 8/9 summaries in previous releases are affected.
- **Chunked GEE pulls failed after the first 5,000 sites** (`csv_to_eeFeat` in
  `b_pull_Landsat_SRST_poi/py/run_GEE_per_pathrow.py`): a July 2025 change
  switched row lookup from label-based to positional (`df.iloc`) but kept the
  per-chunk index offsets, so any path-row with more than 5,000 locations (73
  path-rows) raised an `IndexError` on its second chunk. `csv_to_eeFeat` now
  iterates over every row of the chunk it is given, with no offsets. The
  2025-02-12 GEE run predates this change and is not affected.
- **`pCount_unreal_val` and `pCount_sun_glint` were always 0** (#58;
  `run_GEE_per_pathrow.py`): `add_realistic_mask` and `add_sun_glint_mask`
  applied `selfMask()`, so failing pixels were masked rather than 0 and could
  never be counted. `selfMask()` was removed (masking of summarized pixels is
  unchanged), and both tallies are now restricted to the DSWE water area for
  each product (DSWE1, DSWE1a, DSWE3) after the hillshade, cloud and
  opacity/aerosol masks. Both columns are documented in the bookdown
  diagnostic columns section.
- **`pCount_high_opac`, `pCount_high_aero` and `pCount_ir_glint` counted the
  whole buffer** (`run_GEE_per_pathrow.py`): for consistency with the other
  mask tallies, they are now restricted to the DSWE water area for each product
  after the hillshade and cloud masks (`pCount_ir_glint` also after the
  opacity/aerosol mask), and are documented in the bookdown diagnostic columns
  section. Values are not comparable to previous releases.
- **DSWE1a per-band `*_thresh` and `*_glint` tallies were always 0** (#57;
  `ref_pull_457_DSWE1a`, `ref_pull_89_DSWE1a`): these tallies were restricted
  to `d.eq(1).Or(alg.eq(1))`, but `alg` has the realistic value and sun glint
  masks applied, so pixels below -0.01 or at/above 0.2 were always hidden (and
  `*_ir_glint` was undercounted). The DSWE1a water area for the diagnostic
  tallies is now defined from the algal thresholds directly (`water_1a`).
  DSWE1 and DSWE3 tallies were not affected, and no summarized data change.
- **Hillshadow missed terrain shadows at low sun angles**
  (`calc_hill_shadows` in `b_pull_Landsat_SRST_poi/py/run_GEE_per_pathrow.py`):
  `ee.Terrain.hillShadow` was run with a neighborhood size of 30 pixels (~900 m
  at the 30 m reduction scale), which is shorter than the shadows cast by tall
  terrain at low sun. Whether those shadows were detected depended on how GEE
  split up the request (which sites were in the task and their order), so a
  site could be 0% or 93% shaded in the same scene. The neighborhood size is
  now 100 pixels (~3 km). In tests across seven scenes (sun elevation 16-62°,
  SE Alaska and Colorado), the share of sites more than 0.1 from a converged
  reference (neighborhood size 300-450) dropped from up to 33% to 0-10%;
  larger neighborhoods were not more accurate. This adds ~2% to the GEE
  compute. `prop_hillShadow`, and the pixel counts and summaries that use the
  hillshadow mask, are expected to change mostly at low sun in high-relief
  areas. Some dependence on how GEE splits the request remains (up to ~0.15
  in `prop_hillShadow` for a few sites per scene) at any neighborhood size.

### Added
- New worked example in `bookdown/07-intermission_handoffs.Rmd`: applies
  handoff coefficients to a single densely-sampled site (`1102_668`, Nee
  Noshe Reservoir, CO) across all bands and Landsat missions.

### Changed
- **New lakeSR locations version (`lakeSR_poi_with_flags_2026-10-20.csv`),
  NHD Best Resolution for all waterbodies** (`a_Calculate_Centers.R`,
  `a_Calculate_Centers/src/`): CONUS waterbodies were previously from
  NHDPlusV2 (via {nhdplusTools}) and only HUC2s 19-22 from NHD Best
  Resolution. All HUC4s now come from the NHD Best Resolution HU4 GeoPackages
  on The National Map, which increases the number of waterbodies from 729,941
  to 1,102,446. All `nhd_id` values are now NHD permanent identifiers
  (`nhd_source` is `NHDBestRes` for all rows).
  - **All `lakeSR_id` values changed.** Within each HUC4, `lakeSR_id` is now
    assigned in order of NHD permanent identifier, so ids are reproducible from
    the source data. Ids from the 2025-02-12 locations do not refer to the same
    waterbodies; use `nhd_id` to link between versions where possible.
  - Waterbodies that cross HUC4 boundaries are kept once (previously 248
    NHDPlusV2 COMIDs appeared under two `lakeSR_id`s), assigned to the HUC4
    matching their reach code, otherwise the lowest HUC4.
  - The list of HUC4s now comes from the GeoPackages available on The National
    Map (replacing the {tigris} state boundaries and `nhdplusTools::get_huc()`
    lookup) and is saved as `lakeSR_NHD_manifest_2026-10-20.csv` with each
    file's last-modified date and ETag. Downloads are checked against the
    manifest. In the Great Lakes, HUC4 0415 is replaced by 0416-0433 in the NHD
    Best Resolution HUC4s.
  - Dropped `nhdplusTools` and `rmapshaper` as dependencies of the centers
    targets; added `curl` and `xml2`.
- Bookdown chapters 1-2 updated to describe the new locations workflow and
  file names.
- `run_GEE_per_pathrow.py`: the `cloud_filter` config setting is now honored;
  previously it was read but ignored, so the scene-level `CLOUD_COVER` filter
  always ran. The filter is now applied after the per-mission collections are
  merged. No data change with the lakeSR config (`cloud_filter: True`).
- `run_GEE_per_pathrow.py`: corrected code comments and docstrings that did not
  match the code (cloud mask values, algae threshold, `add_opac_mask` missions,
  stale line numbers in the header). No change to the data.
- `bookdown/05-lakeSR_LS_C2_SRST.Rmd`: the cloud mask is documented as using
  `QA_PIXEL` (not `CLOUD_QA`) with the bits used; the sun glint mask is
  documented as excluding the Landsat 8/9 Aerosol band; function names
  corrected (`add_cf_mask`, `add_sun_glint_mask`). The diagnostic columns
  section now includes a diagram of how the masks are layered and where each
  diagnostic column is tallied.
- `run_GEE_per_pathrow.py`: consolidated `add_realistic_mask_457` and the
  unused `add_realistic_mask_89` into a single `add_realistic_mask`. All
  missions were already using the Landsat 4-7 version, so the Landsat 8/9
  Aerosol band has never been screened for unrealistic values; there is no
  change to the data. `bookdown/05-lakeSR_LS_C2_SRST.Rmd` now documents this
  and states that Aerosol band summaries are for diagnostic use only
  ([Maciel et al. 2023](https://doi.org/10.1002/lol2.10344)).
- `python/pySetup.R`: rewritten so a missing or empty `env/` is actually
  (re)built (the previous `tryCatch` install branches could never run).
  Updated to Python 3.12 and current `earthengine-api`/`pandas`/`numpy`/`pyyaml`
  (versions pinned in `py_versions`); dropped the unused `pyreadr`.
- **One GEE site export for all DSWE variants**
  (`b_pull_Landsat_SRST_poi/py/run_GEE_per_pathrow.py`): the DSWE1, DSWE1a,
  and DSWE3 summaries were sent as separate tasks, each repeating all of the
  per-image processing (masks, hillshade, hillshadow, DSWE). The
  `ref_pull_457_*` and `ref_pull_89_*` functions are replaced by
  `ref_pull_combined`, which does the shared processing once per image and
  summarizes every configured variant in one `reduceRegions` call, so each
  mission group and chunk is one task (`..._SRST_DSWEcombo1-1a_...csv`, with
  variant columns prefixed `DSWE1__`, `DSWE1a__`, etc.). For the same site
  order and hillshadow settings, the output is identical to the per-variant
  exports. In a test of four path-rows, this and the spatial sort below
  (before the hillshadow change above) used 65% of the EECU of the
  per-variant pull (53-88% per path-row).
- **Locations are sorted spatially before chunking** (`sort_spatially` in
  `run_GEE_per_pathrow.py`): path-rows with more than 5,000 locations are sent
  in 5,000-location chunks, and chunks taken in file order often spanned most
  of the path-row, so the same image tiles were processed for several chunks.
  Locations are now ordered along a Hilbert curve before chunking, so chunks
  are spatially compact (in the 127 path-rows with more than one chunk, the
  image tiles processed drop from ~1.30x to ~1.07x of those needed). This
  only changes which chunk a location is in.
- **New collation step to split the combined site files**
  (`c_split_combined_files` in `c_collate_Landsat_data.R`,
  `c_collate_Landsat_data/src/split_combined_dswe.R`): after download, each
  combined file is split into one file per DSWE variant
  (`..._SRST_DSWE1_...csv`, `..._SRST_DSWE1a_...csv`) in the format of the
  previous per-variant exports, which are then collated as before. Values are
  copied as text, so they are not reformatted.
- **`LAKESR_EE_PROJ` environment variable** (`run_GEE_per_pathrow.py`,
  `poi_wait_for_completion.py`, `check_for_failed_tasks.py`): overrides
  `ee_proj` in the GEE config, so a run can continue from a different Google
  Cloud project (e.g. when a project reaches its EECU quota) without changing
  the config file and invalidating completed targets. Documented in
  `run_targets.Rmd`. `run_GEE_per_pathrow.py` also accepts `LAKESR_YML_CSV`
  and `LAKESR_PATHROW_FILE` to run outside of {targets} without overwriting
  the files used by the pipeline.
- `bookdown/05-lakeSR_LS_C2_SRST.Rmd`: documents the hillshadow neighborhood
  size, the combined DSWE export, and the spatial sort; corrected the
  description of the DEM extent used for hillshadow (the WRS-2 path-row
  buffered by 3,000 m, not 3,000 m around each location). The
  `bookdown/01-Background.Rmd` file tree includes `split_combined_dswe.R`.
- Regenerated `collated_handoffs_GEEv2025-02-12_QAv2025-06-04.csv` and all
  `roy_handoff`/`roy_deming_residuals` diagnostic plots.
- Full bookdown re-render.

## [v2026-01-28] - 2026-01-28
"Finalized lakeSR codebase for publication." Minor changes not reflected in
the previous release: supervisor/USGS readthrough edits across bookdown
chapters 1-8, a reference update (closes #60), and small bookdown rendering
consistency fixes.

## [v2026-01-16] - 2026-01-16
"Updated lakeSR codebase for initial USGS release."

## [v2025-03-20] - 2025-03-20
"Pre-USGS review release." Version release for USGS review, March 2025.

[Unreleased]: https://github.com/AquaSat/AquaMatch_lakeSR/compare/v2026-01-28...HEAD
[v2026-01-28]: https://github.com/AquaSat/AquaMatch_lakeSR/releases/tag/v2026-01-28
[v2026-01-16]: https://github.com/AquaSat/AquaMatch_lakeSR/releases/tag/v2026-01-16
[v2025-03-20]: https://github.com/AquaSat/AquaMatch_lakeSR/releases/tag/v2025-03-20
