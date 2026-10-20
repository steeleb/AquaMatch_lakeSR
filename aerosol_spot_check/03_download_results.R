# Download the aerosol spot check exports from Google Drive. Standalone; NOT
# part of the {targets} pipeline. Run after all Earth Engine tasks from
# 02_run_aero_check.R have completed.

library(tidyverse)
library(yaml)
library(googledrive)

cfg <- read_yaml("aerosol_spot_check/config_aero_check.yml")
drive_auth(cfg$google_settings$google_email)

folder <- paste0(cfg$google_settings$drive_folder, "_v", cfg$google_settings$run_date)
dir_down <- "aerosol_spot_check/down/"
if (!dir.exists(dir_down)) dir.create(dir_down)

files <- drive_ls(folder, pattern = "^lakeSR_aero_check_LS89_.*\\.csv$")

sample_prs <- read_csv("aerosol_spot_check/out/sample_pathrows.csv",
                       col_types = cols(WRS2_PR = "c"))
missing <- setdiff(paste0("lakeSR_aero_check_LS89_", sample_prs$WRS2_PR,
                          "_v", cfg$google_settings$run_date, ".csv"),
                   files$name)
if (length(missing) > 0) {
  warning("Missing exports (task failed or still running?):\n",
          paste(missing, collapse = "\n"))
}

walk2(files$id, files$name, \(id, name) {
  drive_download(as_id(id), path = file.path(dir_down, name), overwrite = TRUE)
})
