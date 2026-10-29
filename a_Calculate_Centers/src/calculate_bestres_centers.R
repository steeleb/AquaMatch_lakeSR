#' @title Calculate POI centers for NHD Best Resolution waterbodies by HUC4
#'
#' @description
#' Download the NHD Best Resolution HU4 GeoPackage from The National Map, subset
#' the waterbodies to lakes/reservoirs >= 1ha (excluding small intermittent
#' lakes/ponds), and calculate the POI for each polygon. POI distance to shore
#' is calculated in meters using the UTM coordinate system and the POI is
#' returned as Latitude/Longitude in WGS84 decimal degrees.
#'
#' @param HUC4_manifest one-row tibble from `get_bestres_manifest()` with huc4,
#' url, and etag columns
#' @param dl_dir directory to temporarily store downloaded files
#'
#' @returns tibble of POI centers and associated waterbody metadata, or NULL if
#' no waterbodies in the HUC4 meet the criteria
#'
#'
calculate_bestres_centers <- function(HUC4_manifest,
                                      dl_dir = "a_Calculate_Centers/nhd/") {

  HUC4 <- HUC4_manifest$huc4
  message(paste0("Beginning center calculation for HUC4 ", HUC4))

  # make sure the file on The National Map is the same version recorded in the
  # manifest
  header <- curl_fetch_memory(HUC4_manifest$url,
                              handle = new_handle(nobody = TRUE))
  current_etag <- str_remove_all(parse_headers_list(header$headers)$etag, '"')
  if (!identical(current_etag, HUC4_manifest$etag)) {
    stop(paste0("The NHD GeoPackage for HUC4 ", HUC4, " has changed since the ",
                "manifest was created. Run `tar_invalidate(a_HUC4_manifest)` to ",
                "use the updated files (this is a new centers version)."))
  }

  # download and unzip in a HUC4-specific folder, removed upon exit
  huc_dir <- file.path(dl_dir, HUC4)
  dir.create(huc_dir, showWarnings = FALSE)
  on.exit(unlink(huc_dir, recursive = TRUE))

  options(timeout = 60000)
  zip_file <- file.path(huc_dir, basename(HUC4_manifest$url))
  download.file(HUC4_manifest$url, destfile = zip_file, quiet = TRUE)
  gpkg_name <- paste0("NHD_H_", HUC4, "_HU4_GPKG.gpkg")
  unzip(zip_file, files = gpkg_name, exdir = huc_dir)

  # read the NHDWaterbody layer, filtering for:
  #   ftypes of interest: 390 = lake/pond; 436 = reservoir
  #   area >= 1 hectare (0.01 km^2)
  #   and removing small (< 4 ha) lakes/ponds that are characterized as
  #   intermittent
  wbd <- st_read(file.path(huc_dir, gpkg_name),
                 query = "SELECT * FROM NHDWaterbody
                          WHERE ftype IN (390, 436)
                            AND areasqkm >= 0.01
                            AND NOT (areasqkm < 0.04 AND
                                     fcode IN (39001, 39005, 39006, 43614))",
                 quiet = TRUE)

  if (nrow(wbd) == 0) {
    message(paste0("HUC4 ", HUC4, " contains no waterbodies that meet criteria."))
    return(NULL)
  }

  # drop z coordinates and transform to WGS84 (NHD is in NAD83)
  wbd <- wbd %>%
    st_zm(drop = TRUE) %>%
    st_transform(crs = "EPSG:4326")

  # assign each polygon a UTM zone using the center of its bounding box
  bbox_center <- map(st_geometry(wbd), \(g) {
    bb <- st_bbox(g)
    c(x = mean(bb[c("xmin", "xmax")]), y = mean(bb[c("ymin", "ymax")]))
  }) %>%
    do.call(rbind, .)
  utm_zone <- floor((bbox_center[, "x"] + 180) / 6) + 1
  wbd$utm_epsg <- if_else(bbox_center[, "y"] >= 0,
                          32600 + utm_zone, # EPSG prefix for N hemisphere
                          32700 + utm_zone) # for S hemisphere

  # calculate POI for all polygons within each UTM zone
  poi <- split(wbd, wbd$utm_epsg) %>%
    map(\(zone_wbd) calculate_poi(zone_wbd, utm_epsg = unique(zone_wbd$utm_epsg))) %>%
    list_rbind()

  # polygons that are empty after validation have no location; note and remove
  no_location <- poi %>% filter(is.na(poi_Latitude) | is.na(poi_Longitude))
  if (nrow(no_location) > 0) {
    warning(paste0(nrow(no_location), " waterbodies in HUC4 ", HUC4,
                   " have no valid geometry and were dropped: ",
                   paste(no_location$permanent_identifier, collapse = ", ")))
  }

  wbd %>%
    st_drop_geometry() %>%
    select(permanent_identifier, gnis_id, gnis_name, areasqkm, reachcode, ftype, fcode) %>%
    inner_join(poi %>% filter(!is.na(poi_Latitude), !is.na(poi_Longitude)),
               by = "permanent_identifier") %>%
    mutate(huc4 = HUC4,
           across(c(permanent_identifier, gnis_id, gnis_name, reachcode), as.character)) %>%
    relocate(huc4, permanent_identifier, poi_Latitude, poi_Longitude, poi_dist_m, poi_method)

}
