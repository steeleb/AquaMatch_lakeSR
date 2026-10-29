#' @title Split a combined DSWE site file into per-variant files
#'
#' @description
#' Function to split a site .csv file exported by run_GEE_per_pathrow.py, which
#' summarizes all DSWE variants in one file (e.g.
#' LSC2_poi_site_LS89_C2_SRST_DSWEcombo1-1a_029027_0_v2026-10-22.csv), into
#' one file per DSWE variant (e.g. LSC2_poi_site_LS89_C2_SRST_DSWE1_029027_0_v2026-10-22.csv),
#' so that they can be collated per variant. Variant columns (prefixed with the
#' variant name and two underscores, e.g. "DSWE1a__med_Blue") are renamed
#' without the prefix, columns shared by all variants are kept, and site-scenes
#' without a summary for the variant are dropped. Values are read and written
#' as text, so they are not reformatted.
#'
#' @param file file path of the combined DSWE .csv file
#' @param out_dir directory to save the per-variant files to. Defaults to the
#' directory of `file`.
#' @returns vector of file paths of the per-variant files. Silently saves the
#' per-variant files in `out_dir`.
#'
#'
split_combined_dswe <- function(file, out_dir = dirname(file)) {
  combined <- fread(file, colClasses = "character", na.strings = "")
  variant_cols <- str_detect(names(combined), "^DSWE[0-9a]+__")
  variants <- unique(str_extract(names(combined)[variant_cols], "^DSWE[0-9a]+(?=__)"))
  map_chr(variants, \(v) {
    prefix <- paste0(v, "__")
    # keep shared columns and this variant's columns, in export order
    keep <- names(combined)[!variant_cols | str_starts(names(combined), prefix)]
    one_variant <- combined[, ..keep]
    setnames(one_variant, str_remove(keep, paste0("^", prefix)))
    one_variant <- one_variant[!is.na(med_Blue)]
    fp <- file.path(out_dir,
                    str_replace(basename(file), "_DSWEcombo[^_]+_", paste0("_", v, "_")))
    fwrite(one_variant, fp, na = "")
    fp
  })
}


#' @title Split all combined DSWE site files in a directory
#'
#' @description
#' Function to run split_combined_dswe() on each combined DSWE site file
#' (file names containing "_DSWEcombo") in a directory, skipping files that have
#' already been split (all per-variant files exist and are newer than the
#' combined file).
#'
#' @param directory directory of downloaded .csv files, e.g.
#' c_collate_Landsat_data/down/RUN_DATE/LS457
#' @param depends target object; any target that must be run prior to this
#' function. Defaults to NULL.
#' @returns vector of file paths of the per-variant files
#'
#'
split_combined_dswe_files <- function(directory, depends = NULL) {
  combined_files <- list.files(directory, pattern = "_DSWEcombo[^_]+_.*\\.csv$",
                               full.names = TRUE)
  map(combined_files, \(f) {
    variants <- str_extract(basename(f), "(?<=_DSWEcombo)[^_]+") %>%
      str_split_1("-") %>%
      paste0("DSWE", .)
    expected <- file.path(directory,
                          map_chr(variants, \(v) str_replace(basename(f), "_DSWEcombo[^_]+_",
                                                             paste0("_", v, "_"))))
    if (all(file.exists(expected)) && all(file.mtime(expected) >= file.mtime(f))) {
      expected
    } else {
      split_combined_dswe(f)
    }
  }) %>%
    unlist()
}
