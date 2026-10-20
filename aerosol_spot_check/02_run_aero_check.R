# Send one Earth Engine export task per sampled path-row for the Landsat 8/9
# aerosol QA spot check. Standalone; NOT part of the {targets} pipeline.
#
# Run from the repository root after 01_select_sample.R. Requires an
# authenticated Earth Engine account with access to the project in
# config_aero_check.yml.

library(tidyverse)
library(reticulate)

# activate (or build) the same conda environment the pipeline uses
source("python/pySetup.R")

dir_mid <- "aerosol_spot_check/mid/"
if (!dir.exists(dir_mid)) dir.create(dir_mid)

sample_prs <- read_csv("aerosol_spot_check/out/sample_pathrows.csv",
                       col_types = cols(WRS2_PR = "c"))

# one path-row at a time, as in production: the python script reads the current
# path-row from file
walk(sample_prs$WRS2_PR, \(pr) {
  write_lines(pr, file.path(dir_mid, "current_pathrow.txt"), sep = "")
  source_python("aerosol_spot_check/py/aero_check_per_pathrow.py")
})

message("All ", nrow(sample_prs), " tasks sent. Monitor progress at ",
        "https://code.earthengine.google.com/tasks, then run 03_download_results.R")
