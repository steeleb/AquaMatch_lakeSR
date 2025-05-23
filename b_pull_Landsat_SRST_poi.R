# Source functions for this {targets} list
tar_source("b_pull_Landsat_SRST_poi/src/")

# Initiate pull of Landsat C2 SRST -------------

# This {targets} list initiates the pull of Landsat SRST for all POI calculated
# in the {targets} group "a_Calculate_Centers".

# check for configuration setting to run gee, if present, run GEE acquisition
if (config::get(config = general_config)$run_GEE) {
  
  # Set up python virtual environment ---------------------------------------
  
  tar_source("python/pySetup.R")
  
  b_pull_Landsat_SRST_poi_list <- list(
    
    # Check local directory structure -----------------------------------------
    
    tar_target(
      name = b_check_dir_structure,
      command = {
        # make directories if needed
        directories = c("b_pull_Landsat_SRST_poi/mid/",
                        "b_pull_Landsat_SRST_poi/out/")
        walk(directories, function(dir) {
          if(!dir.exists(dir)){
            dir.create(dir)
          }
        })
      },
      cue = tar_cue("always")
    ),
    
    
    # Set up GEE configuration ------------------------------------------------
    
    # read and track the config file
    tar_file_read(
      name = b_config_file_poi,
      command = poi_config,
      read = read_yaml(!!.x),
      packages = "yaml"
    ),
    
    # load, format, save yml as a csv
    tar_target(
      name = b_yml_poi,
      command = {
        # need to make sure that the directory structure has been created prior
        # to running this target
        b_check_dir_structure
        format_yaml(yml = b_config_file_poi)
      },
      packages = c("yaml", "tidyverse")
    ),
    
    # check for Drive folders and architecture per config setup
    tar_target(
      name = b_check_Drive_parent_folder,
      command = if (b_yml_poi$parent_folder != "") {
        tryCatch({
          drive_auth(b_yml_poi$google_email)
          drive_ls(b_yml_poi$parent_folder)
        }, error = function(e) {
          drive_mkdir(b_yml_poi$parent_folder)
        })
      },
      packages = "googledrive",
      cue = tar_cue("always")
    ),
    
    tar_target(
      name = b_check_Drive_GEE_folder,
      command =  {
        b_check_Drive_parent_folder
        tryCatch({
          drive_auth(b_yml_poi$google_email)
          if (b_yml_poi$parent_folder != "") {
            path <- file.path(b_yml_poi$parent_folder, 
                              paste0(b_yml_poi$proj_folder, 
                                     "_v",
                                     b_yml_poi$run_date))
          } else {
            path <- paste0(b_yml_poi$proj_folder, 
                           "_v",
                           b_yml_poi$run_date)
          }
          drive_ls(path)
        }, error = function(e) {
          drive_mkdir(path)
        })
      },
      packages = "googledrive",
      cue = tar_cue("always")
    ),
    
    
    # Format locations and check for containment in WRS -----------------------
    
    # reformat location file for run_GEE_per_tile using the combined_poi_points
    # from the a_Calculate_Centers group
    tar_target(
      name = b_ref_locations_poi,
      command = reformat_locations(yml = b_yml_poi, 
                                   locations = a_poi_with_flags)
    ),
    
    # get WRS tiles/indication of whether buffered points are contained by them
    tar_target(
      name = b_WRS_pathrow_poi,
      command = get_WRS_pathrow_poi(locations = b_ref_locations_poi, 
                                    yml = b_yml_poi)
    ),
    
    # check to see if geometry is completely contained in pathrow
    tar_target(
      name = b_poi_locs_filtered,
      command = check_if_fully_within_pr(WRS_pathrow = b_WRS_pathrow_poi, 
                                         locations = b_ref_locations_poi, 
                                         yml = b_yml_poi),
      pattern = map(b_WRS_pathrow_poi),
      packages = c("tidyverse", "sf", "arrow")
    ),
    
    
    # Get the Landsat Stacks! -------------------------------------------------
    
    # track python files for changes
    tar_file(
      name = b_eeRun_script,
      command = {
        b_check_Drive_GEE_folder
        "b_pull_Landsat_SRST_poi/py/run_GEE_per_pathrow.py"
      }
    ),
    
    tar_file(
      name = b_ee_complete_script,
      command = "b_pull_Landsat_SRST_poi/py/poi_wait_for_completion.py"
    ),
    
    tar_file(
      name = b_ee_fail_script,
      command = "b_pull_Landsat_SRST_poi/py/check_for_failed_tasks.py"
    ),
    
    # run the Landsat pull as function per tile - this is the longest step and can
    # not be run by multiple crew workers because the bottleneck is on the end of
    # GEE, not local compute.
    tar_target(
      name = b_eeRun_poi,
      command = {
        b_eeRun_script
        b_yml_poi
        run_GEE_per_pathrow(WRS_pathrow = b_WRS_pathrow_poi)
      },
      pattern = map(b_WRS_pathrow_poi),
      packages = "reticulate",
      # note, this cannot/should not be used in mulitcore processing mode - the
      # bottleneck here is at GEE, not local processing, and the way the {targets}
      # and python workflow work together requires only a single `run_GEE_per_tile`
      # at a time.
      deployment = "main"
    ),
    
    # check to see that all tasks are complete! This target will run until all
    # cued GEE tasks from the previous target are complete.
    tar_target(
      name = b_poi_tasks_complete,
      command = {
        b_eeRun_poi
        source_python(b_ee_complete_script)
      },
      packages = "reticulate",
      deployment = "main"
    ),
    
    # since we can't easily track if tasks have failed, and we send a lot of tasks
    # in this process, let's check for any failed tasks and add them to 
    # b_pull_Landsat_SRST_poi/out/GEE_failed_tasks_vRUN_DATE.txt
    tar_target(
      name = b_check_for_failed_tasks,
      command = {
        b_poi_tasks_complete
        source_python(b_ee_fail_script)
      },
      packages = "reticulate",
      deployment = "main"
    ),
    
    # save the yml file for posterity/for users who aren't running the pull
    tar_target(
      name = b_send_yml_to_drive,
      command = export_single_target(
        target = b_yml_poi, 
        drive_path = a_check_Drive_targets_folder,
        google_email = lakeSR_config$google_email,
        date_stamp = b_yml_poi$run_date),
      packages = c("tidyverse", "googledrive")
    ), 
    
    # get drive_ids, store
    tar_target(
      name = b_save_yml_Drive_info,
      command = {
        drive_ids <- b_send_yml_to_drive %>% 
          select(name, id)
        write_csv(drive_ids,
                  "b_pull_Landsat_SRST_poi/out/yml_drive_ids.csv")
        drive_ids
      },
      packages = c("tidyverse", "googledrive")
    )
    
  )
  
} else {
  
  # if the run is not set up to run GEE, just load the current b_yml_poi
  
  b_pull_Landsat_SRST_poi_list <- list(
    
    tar_file_read(
      name = b_save_yml_Drive_info,
      command = "b_pull_Landsat_SRST_poi/out/yml_drive_ids.csv",
      read = read_csv(!!.x),
      cue = tar_cue("always")
    ),
    
    tar_target(
      name = b_yml_poi,
      command = retrieve_target(
        target = "b_yml_poi", 
        id_df = b_save_yml_Drive_info, 
        local_folder = "b_pull_Landsat_SRST_poi/out/", 
        google_email = lakeSR_config$google_email, 
        version_date = lakeSR_config$collated_version
      ),
      packages = c("tidyverse", "googledrive"),
      cue = tar_cue("always")
    )
    
  )
  
}

