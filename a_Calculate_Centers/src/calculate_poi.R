#' @title Calculate the point of inaccessibility for waterbody polygons within
#' a single UTM zone
#'
#' @description
#' Calculate the Point of Inaccessibility (POI, also known as the Chebyshev
#' center) for each polygon using `polylabelr::poi()` in the polygon's UTM zone
#' so that distance to shore is in meters. This is an adaptation of Xiao Yang's
#' code in EE - Yang, Xiao. (2020). Deepest point calculation for any given
#' polygon using Google Earth Engine JavaScript API (Version v1). Zenodo.
#' https://doi.org/10.5281/zenodo.4136755
#'
#' Each POI is checked for containment within its own polygon. Any polygon whose
#' POI could not be calculated or falls outside of the polygon is assigned a
#' point on surface instead (`poi_method` = "point_on_surface"), so no
#' waterbody is silently dropped.
#'
#' @param polys {sf} object of waterbody polygons with a `permanent_identifier`
#' column, all within the same UTM zone
#' @param utm_epsg numeric; EPSG code of the UTM zone for `polys`
#' @param precision numeric; precision (in meters) passed to `polylabelr::poi()`
#'
#' @returns tibble of permanent_identifier, poi_Latitude, poi_Longitude (WGS84
#' decimal degrees), poi_dist_m, and poi_method
#'
#'
calculate_poi <- function(polys, utm_epsg, precision = 0.01) {

  # project to UTM, fix any invalid geometries, and keep only the polygonal
  # parts (st_make_valid() can return geometry collections)
  geoms <- polys %>%
    st_geometry() %>%
    st_transform(crs = utm_epsg) %>%
    st_make_valid()
  if (any(st_is(geoms, "GEOMETRYCOLLECTION"))) {
    geoms <- st_collection_extract(geoms, "POLYGON", warn = FALSE)
  }

  # calculate the POI for each polygon
  pois <- map(geoms, \(g) {
    tryCatch(poi_sfg(g, precision = precision),
             error = \(e) list(x = NA_real_, y = NA_real_, dist = NA_real_))
  })
  poi_xy <- tibble(x = map_dbl(pois, "x"),
                   y = map_dbl(pois, "y"),
                   poi_dist_m = map_dbl(pois, "dist"),
                   poi_method = "poi")
  points <- st_as_sf(poi_xy, coords = c("x", "y"), crs = utm_epsg, na.fail = FALSE) %>%
    st_geometry()

  # check that each POI is within its own polygon
  within <- st_within(points, geoms)
  inside <- map2_lgl(seq_along(within), within, \(i, w) i %in% w)

  # fall back to a point on surface for any POI that failed or is outside its
  # polygon, and calculate distance to shore for that point
  fallback <- which(!inside & !st_is_empty(geoms))
  if (length(fallback) > 0) {
    surface_pts <- st_point_on_surface(geoms[fallback])
    points[fallback] <- surface_pts
    poi_xy$poi_dist_m[fallback] <- as.numeric(
      st_distance(surface_pts, st_boundary(geoms[fallback]), by_element = TRUE)
    )
    poi_xy$poi_method[fallback] <- "point_on_surface"
  }

  # re-calculate decimal degrees in WGS84
  coords <- points %>%
    st_transform(crs = "EPSG:4326") %>%
    st_coordinates()

  tibble(permanent_identifier = polys$permanent_identifier,
         poi_Latitude = coords[, "Y"],
         poi_Longitude = coords[, "X"],
         poi_dist_m = poi_xy$poi_dist_m,
         poi_method = poi_xy$poi_method)

}


#' @title Calculate the POI for a single POLYGON or MULTIPOLYGON
#'
#' @description
#' `polylabelr::poi()` expects the rings of a polygon to be separated by NA
#' rows, with the first ring as the exterior and any subsequent rings as holes.
#' Passing the output of `st_coordinates()` directly (or using polylabelr's own
#' POLYGON method) merges all rings and parts into one, which can place the POI
#' outside of the polygon. Here, each polygon part is handled separately, with
#' its holes, and the part with the greatest distance to shore is returned.
#'
#' @param g sfg POLYGON or MULTIPOLYGON in a projected CRS
#' @param precision numeric; precision passed to `polylabelr::poi()`
#'
#' @returns list of x, y, and dist
#'
#'
poi_sfg <- function(g, precision) {
  parts <- if (inherits(g, "MULTIPOLYGON")) unclass(g) else list(unclass(g))
  pois <- map(parts, \(rings) {
    # separate rings with NA rows (no trailing NA, which polylabelr reads as an
    # empty ring)
    xy <- do.call(rbind, map(rings, \(r) rbind(NA, r[, 1:2, drop = FALSE])))[-1, ]
    poi(xy, precision = precision)
  })
  pois[[which.max(map_dbl(pois, "dist"))]]
}
