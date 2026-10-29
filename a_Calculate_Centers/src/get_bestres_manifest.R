#' @title List the NHD Best Resolution HU4 GeoPackages on The National Map
#'
#' @description
#' Query the public S3 bucket behind The National Map for every NHD Best
#' Resolution HU4 GeoPackage and record each file's last-modified date and ETag.
#' This listing defines which HUC4s are processed and serves as the record of
#' exactly which version of each file was used to calculate centers.
#'
#' @returns tibble with one row per HUC4: huc4, url, last_modified, etag,
#' size_bytes
#'
#'
get_bestres_manifest <- function() {

  base_url <- "https://prd-tnm.s3.amazonaws.com/"
  prefix <- "StagedProducts/Hydrography/NHD/HU4/GPKG/"

  listing <- read_xml(paste0(base_url, "?list-type=2&prefix=", prefix)) %>%
    xml_ns_strip()

  # S3 returns at most 1000 keys per request; there are ~750 files under this
  # prefix (zip/xml/jpg per HUC4), so a single request is sufficient for now
  if (xml_text(xml_find_first(listing, "//IsTruncated")) == "true") {
    stop("S3 listing of NHD HU4 GeoPackages was truncated; pagination is needed.")
  }

  contents <- xml_find_all(listing, "//Contents")

  tibble(
    key = xml_text(xml_find_first(contents, "Key")),
    last_modified = xml_text(xml_find_first(contents, "LastModified")),
    etag = str_remove_all(xml_text(xml_find_first(contents, "ETag")), '"'),
    size_bytes = as.numeric(xml_text(xml_find_first(contents, "Size")))
  ) %>%
    filter(str_detect(key, "NHD_H_\\d{4}_HU4_GPKG\\.zip$")) %>%
    mutate(huc4 = str_extract(key, "(?<=NHD_H_)\\d{4}"),
           url = paste0(base_url, key)) %>%
    select(huc4, url, last_modified, etag, size_bytes) %>%
    arrange(huc4)

}
