
# Source functions for this {targets} list
tar_source("a_Calculate_Centers/src/")


# Calculate POI Centers for all US and Territories Lakes -------------

# This {targets} list calculates the "Point of Inaccessibility" (POI), also known
# as the Chebyshev center, for all lakes/reservoirs greater than 1 ha in surface 
# area in the USGS National Hydrography Dataset (NHD) Best Resolution product, 
# using the `poi()` function in the {polylabelr} package. NHD Best Resolution 
# HU4 GeoPackages are downloaded directly from The National Map. The list of 
# HU4 files, along with each file's last-modified date and ETag, is recorded in
# a manifest so that the exact source data for each centers version is 
# documented and changes to the source files are detected.

# check for configuration setting to calculate centers, if TRUE, calculate centers
if (config::get(config = general_config)$calculate_centers) {
  
  # create list of targets to perform this task
  a_Calculate_Centers_list <- list(
    tar_target(
      name = a_check_dir_structure,
      command = {
        # make directories if needed
        directories = c("a_Calculate_Centers/mid/",
                        "a_Calculate_Centers/nhd/",
                        "a_Calculate_Centers/out/")
        walk(directories, function(dir) {
          if(!dir.exists(dir)){
            dir.create(dir)
          }
        })
      },
      cue = tar_cue("always")
    ),
    
    # list all NHD Best Resolution HU4 GeoPackages available on The National 
    # Map. This target is intentionally not re-run on every pipeline run so that
    # the source files are fixed for a centers version. To use updated NHD files,
    # run `tar_invalidate(a_HUC4_manifest)` and create a new centers version.
    tar_target(
      name = a_HUC4_manifest,
      command = get_bestres_manifest(),
      packages = c("tidyverse", "xml2")
    ),
    
    # save the manifest alongside the centers output
    tar_target(
      name = a_save_HUC4_manifest,
      command = {
        a_check_dir_structure
        file_path <- paste0("a_Calculate_Centers/out/lakeSR_NHD_manifest_", 
                            lakeSR_config$centers_version, 
                            ".csv")
        write_csv(a_HUC4_manifest, file_path)
        file_path
      },
      format = "file"
    ),
    
    # for each HUC4, download the NHD Best Resolution GeoPackage, subset the 
    # waterbodies to lakes/reservoirs >= 1 ha, and calculate the POI for each 
    # polygon
    tar_target(
      name = a_HUC4_poi,
      command = {
        # need to make sure that the directory structure has been created prior
        # to running this target
        a_check_dir_structure
        calculate_bestres_centers(a_HUC4_manifest)
      },
      packages = c("tidyverse", "sf", "polylabelr", "curl"),
      pattern = map(a_HUC4_manifest)
    ),
    
    # combine the POIs across HUC4s and assign the lakeSR_id
    tar_target(
      name = a_combined_poi,
      command = {
        # the point on surface fallback in `calculate_poi()` is a safeguard that
        # is not expected to be used, so `poi_method` is not included in the
        # output. If it is used, note which waterbodies are affected.
        fallback <- a_HUC4_poi %>%
          filter(poi_method != "poi")
        if (nrow(fallback) > 0) {
          warning(paste0(nrow(fallback), " waterbodies use a point on surface ",
                         "instead of the POI: ",
                         paste(fallback$permanent_identifier, collapse = ", ")))
        }
        a_HUC4_poi %>%
          select(-poi_method) %>%
          # waterbodies that cross HUC4 boundaries are included in more than one 
          # HU4 file; keep a single record, preferring the HUC4 that matches the
          # waterbody's reachcode, then the lowest HUC4
          mutate(reachcode_match = coalesce(str_sub(reachcode, 1, 4) == huc4, FALSE)) %>% 
          arrange(permanent_identifier, desc(reachcode_match), huc4) %>% 
          distinct(permanent_identifier, .keep_all = TRUE) %>% 
          # lakeSR_id is HUC4_n, where n is assigned in order of the NHD 
          # permanent identifier, so that ids are reproducible from the 
          # source data
          arrange(huc4, permanent_identifier) %>% 
          mutate(lakeSR_id = paste(huc4, row_number(), sep = "_"), 
                 .by = huc4) %>% 
          rename(nhd_id = permanent_identifier) %>% 
          mutate(nhd_source = "NHDBestRes") %>% 
          select(-c(reachcode_match, huc4)) %>% 
          relocate(lakeSR_id, nhd_id)
      }
    ),
    
    # and now, we'll add a flag to any locations whose distance to shore is less
    # than the yml configuration of buffer radius plus 30 m for optical bands. Additionally,
    # we will add a flag for any location that is less than buffer distance plus
    # 100 m from shoreline, indicating possible shoreline mixed pixels.
    tar_target(
      name = a_poi_with_flags,
      command = {
        poi_with_flags <- a_combined_poi %>% 
          mutate(flag_optical_shoreline = if_else(poi_dist_m <= as.numeric(b_yml_poi$site_buffer) + 30,
                                                  1,  # possible shoreline contamination
                                                  0), # no expected shoreline contamination
                 flag_thermal_TM_shoreline = if_else(poi_dist_m <= as.numeric(b_yml_poi$site_buffer) + 120,
                                                      1, # possible shoreline contamination
                                                      0), # no expected shoreline contamination
                 flag_thermal_ETM_shoreline = if_else(poi_dist_m <= as.numeric(b_yml_poi$site_buffer) + 60,
                                                      1, # possible shoreline contamination
                                                      0), # no expected shoreline contamination
                 flag_thermal_TIRS_shoreline = if_else(poi_dist_m <= as.numeric(b_yml_poi$site_buffer) + 100,
                                                       1, # possible shoreline contamination
                                                       0)) # no expected shoreline contamination
        # coerce NA -> "" when wb assignment but no gnis/name
        poi_with_flags <- poi_with_flags %>% 
          mutate(across(.cols = c(gnis_id, gnis_name), 
                        .fns = ~ if_else(!is.na(nhd_id) & is.na(.x), "", .x)))
        # save locally
        write_csv(poi_with_flags,
                  paste0("a_Calculate_Centers/out/lakeSR_poi_with_flags_", 
                         lakeSR_config$centers_version, 
                         ".csv"))
        poi_with_flags
      }
    ),
    
    # check for drive folder if set to export
    tar_target(
      name = a_check_Drive_targets_folder,
      command = {
        b_check_Drive_parent_folder
        tryCatch({
          drive_auth(lakeSR_config$google_email)
          folder_name = "lakeSR_targets"
          if (b_yml_poi$parent_folder != "") {
            path <- file.path("~",
                              b_yml_poi$parent_folder, 
                              folder_name)
          } else {
            path <- folder_name
          }
          drive_ls(path, type = "folder")
          return(path)
        }, error = function(e) {
          if (b_yml_poi$parent_folder != "") {
            drive_mkdir(name = folder_name,
                        path = b_yml_poi$parent_folder)
          } else {
            drive_mkdir(name = folder_name)
          }
          return(path)
        })
      },
      packages = "googledrive",
      cue = tar_cue("always"),
      deploy = "main"
    ),
    
    # send poi file to drive 
    tar_target(
      name = a_send_combined_poi_to_drive,
      command = {
        export_single_target(
          target = a_poi_with_flags, 
          drive_path = a_check_Drive_targets_folder,
          google_email = lakeSR_config$google_email,
          date_stamp = lakeSR_config$centers_version,
          file_type = ".csv")
      },
      packages = c("tidyverse", "googledrive")
    ),
    
    # get drive_ids, store
    tar_target(
      name = a_save_combined_poi_info,
      command = {
        drive_ids <- a_send_combined_poi_to_drive %>% 
          select(name, id)
        write_csv(drive_ids,
                  "a_Calculate_Centers/out/poi_flagged_drive_ids.csv")
        drive_ids
      },
      packages = c("tidyverse", "googledrive")
    )
    
  )
  
} else {
  
  # if the run is not set up to calculate centers, just store the combined poi 
  # data from a previous run, using the drive ids and the general configuration 
  # file for this repo
  
  a_Calculate_Centers_list <- list(
    
    tar_target(
      name = a_check_dir_structure,
      command = {
        # make `mid` directory
        if(!dir.exists("a_Calculate_Centers/mid/")){
          dir.create("a_Calculate_Centers/mid/")
        }
      },
      cue = tar_cue("always")
    ),
    
    tar_file_read(
      name = a_combined_poi_drive_ids,
      command ="a_Calculate_Centers/out/poi_flagged_drive_ids.csv",
      read = read_csv(!!.x),
      cue = tar_cue("always")
    ),
    
    tar_target(
      name = a_poi_with_flags,
      command = {
        a_check_dir_structure
        retrieve_target(
          target = "a_poi_with_flags", 
          id_df = a_combined_poi_drive_ids, 
          local_folder = "a_Calculate_Centers/mid/", 
          google_email = lakeSR_config$google_email, 
          version_date = lakeSR_config$centers_version,
          file_type = ".csv"
        )
      },
      packages = c("tidyverse", "googledrive")
    )
    
  )
  
}
