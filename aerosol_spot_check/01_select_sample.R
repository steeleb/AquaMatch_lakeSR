# Select a representative sample of WRS2 path-rows and lakes for the Landsat 8/9
# aerosol QA spot check. This script is standalone and is NOT part of the
# {targets} pipeline. It relies on the per-path-row location files created by
# the b_pull_Landsat_SRST_poi group (b_pull_Landsat_SRST_poi/out/locations/).
#
# Sampling design:
#   1. Assign each lakeSR path-row to an EPA/CEC Level I ecoregion by majority
#      of its lakes (Hawaii and the territories, which fall outside the CEC
#      polygons, form their own stratum).
#   2. Allocate n_pathrows across ecoregions proportional to the square root of
#      the number of lakes in each ecoregion (minimum of 1 per ecoregion). The
#      square root keeps Alaska, which holds more than half of all lakeSR lakes,
#      from dominating the sample while still giving lake-rich regions more
#      path-rows.
#   3. Within each ecoregion, sample path-rows with probability proportional to
#      their number of lakes, skipping path-rows within 250 km of one already
#      chosen in that ecoregion (when possible) to spread the sample spatially.
#   4. Randomly sample up to max_lakes_per_pr lakes within each path-row.

library(tidyverse)
library(sf)
library(yaml)

sf_use_s2(FALSE)

cfg <- read_yaml("aerosol_spot_check/config_aero_check.yml")
set.seed(cfg$sample_settings$seed)

dir_in <- "aerosol_spot_check/in/"
dir_out <- "aerosol_spot_check/out/"
dir_locs <- file.path(dir_out, "sample_locations")
walk(c(dir_in, dir_out, dir_locs), \(d) if (!dir.exists(d)) dir.create(d, recursive = TRUE))


# Load lakeSR locations per path-row --------------------------------------

loc_files <- list.files("b_pull_Landsat_SRST_poi/out/locations/",
                        pattern = "^locations_\\d{6}\\.csv$",
                        full.names = TRUE)
if (length(loc_files) == 0) {
  stop("No location files found. Run the b_pull_Landsat_SRST_poi targets group first.")
}

locs <- map(loc_files, \(fp) read_csv(fp, col_types = cols(.default = "c"))) %>%
  bind_rows() %>%
  mutate(Latitude = as.numeric(Latitude),
         Longitude = as.numeric(Longitude),
         HUC2 = str_sub(id, 1, 2))

pr_counts <- locs %>%
  count(WRS2_PR, name = "n_lakes")


# Assign path-rows to ecoregions ------------------------------------------

eco_zip <- file.path(dir_in, "na_cec_eco_l1.zip")
if (!file.exists(eco_zip)) {
  download.file(cfg$sample_settings$ecoregion_url, eco_zip, mode = "wb")
}
eco_dir <- file.path(dir_in, "na_cec_eco_l1")
if (!dir.exists(eco_dir)) unzip(eco_zip, exdir = eco_dir)
eco <- list.files(eco_dir, pattern = "\\.shp$", recursive = TRUE, full.names = TRUE)[1] %>%
  read_sf() %>%
  # drop open water polygons (Great Lakes, etc.)
  filter(NA_L1CODE != "0") %>%
  select(NA_L1CODE, NA_L1NAME) %>%
  st_make_valid()

# a subset of lakes per path-row is plenty to determine the majority ecoregion
lake_eco <- locs %>%
  group_by(WRS2_PR) %>%
  slice_sample(n = 200) %>%
  ungroup() %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(st_crs(eco)) %>%
  st_join(eco) %>%
  st_drop_geometry() %>%
  # Hawaii (20), Caribbean (21), and Pacific islands (22) fall outside the CEC
  # polygons
  mutate(ecoregion = case_when(!is.na(NA_L1NAME) ~ str_to_title(NA_L1NAME),
                               HUC2 %in% c("20", "21", "22") ~ "Hawaii and Territories",
                               TRUE ~ NA_character_),
         ecoregion = recode(ecoregion, !!!cfg$sample_settings$merge_ecoregions))

pr_eco <- lake_eco %>%
  filter(!is.na(ecoregion)) %>%
  count(WRS2_PR, ecoregion) %>%
  group_by(WRS2_PR) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(WRS2_PR, ecoregion) %>%
  left_join(pr_counts, by = "WRS2_PR")

# path-row centroids for spatial spread and mapping
wrs <- read_sf("b_pull_Landsat_SRST_poi/in/WRS2_descending.shp") %>%
  filter(PR %in% pr_eco$WRS2_PR) %>%
  select(WRS2_PR = PR)
wrs_cent <- wrs %>%
  st_centroid() %>%
  suppressWarnings() %>%
  mutate(cent_lon = st_coordinates(.)[, 1],
         cent_lat = st_coordinates(.)[, 2]) %>%
  st_drop_geometry()

pr_eco <- pr_eco %>%
  left_join(wrs_cent, by = "WRS2_PR")


# Allocate path-rows across ecoregions ------------------------------------

eligible <- pr_eco %>%
  filter(n_lakes >= cfg$sample_settings$min_lakes_per_pr)

# lakes fall in more than one (overlapping) path-row, so count each lake once,
# using the ecoregion of the first path-row it appears in
pop_lakes <- locs %>%
  distinct(id, .keep_all = TRUE) %>%
  inner_join(pr_eco %>% select(WRS2_PR, ecoregion), by = "WRS2_PR") %>%
  count(ecoregion, name = "pop_lakes")

strata <- eligible %>%
  count(ecoregion, name = "n_pr_available") %>%
  left_join(pop_lakes, by = "ecoregion") %>%
  mutate(weight = sqrt(pop_lakes))

n_total <- cfg$sample_settings$n_pathrows
if (nrow(strata) > n_total) {
  stop("More ecoregions than path-rows to sample; increase n_pathrows.")
}

# one per ecoregion, then the remainder by largest remainder of sqrt weights
alloc <- strata %>%
  mutate(n_alloc = 1,
         share = weight / sum(weight) * (n_total - nrow(strata)),
         n_alloc = n_alloc + floor(share),
         remainder = share - floor(share))
n_left <- n_total - sum(alloc$n_alloc)
alloc <- alloc %>%
  arrange(desc(remainder)) %>%
  mutate(n_alloc = n_alloc + (row_number() <= n_left),
         n_alloc = pmin(n_alloc, n_pr_available)) %>%
  select(ecoregion, n_pr_available, n_alloc)


# Sample path-rows within ecoregions --------------------------------------

# great-circle distance in km between path-row centroids
dist_km <- function(lon1, lat1, lon2, lat2) {
  rad <- pi / 180
  dlat <- (lat2 - lat1) * rad
  dlon <- (lon2 - lon1) * rad
  a <- sin(dlat / 2)^2 + cos(lat1 * rad) * cos(lat2 * rad) * sin(dlon / 2)^2
  6371 * 2 * asin(pmin(1, sqrt(a)))
}

# keep candidates at least min_sep_km from every path-row chosen so far
# (across all ecoregions), relaxing the constraint only if too few remain
spread_pool <- function(pool, chosen, n_needed, min_sep_km = 250) {
  if (nrow(chosen) == 0) return(pool)
  far <- map_lgl(seq_len(nrow(pool)), \(i) {
    all(dist_km(pool$cent_lon[i], pool$cent_lat[i],
                chosen$cent_lon, chosen$cent_lat) >= min_sep_km)
  })
  if (sum(far) >= n_needed) pool[far, ] else pool
}

# sample strata with the fewest candidates first so the spacing constraint
# doesn't crowd them out
sample_prs <- eligible[0, ]
for (i in order(alloc$n_pr_available)) {
  pool <- eligible %>% filter(ecoregion == alloc$ecoregion[i])
  for (k in seq_len(alloc$n_alloc[i])) {
    pool <- spread_pool(pool, sample_prs, alloc$n_alloc[i] - k + 1)
    pick <- pool[sample(nrow(pool), 1, prob = pool$n_lakes), ]
    sample_prs <- bind_rows(sample_prs, pick)
    pool <- pool %>% filter(WRS2_PR != pick$WRS2_PR)
  }
}
sample_prs <- sample_prs %>% arrange(ecoregion, WRS2_PR)


# Sample lakes within path-rows -------------------------------------------

sample_lakes <- locs %>%
  filter(WRS2_PR %in% sample_prs$WRS2_PR) %>%
  group_by(WRS2_PR) %>%
  slice_sample(n = cfg$sample_settings$max_lakes_per_pr) %>%
  ungroup()

# write per-path-row location files in the same format as production
walk(unique(sample_lakes$WRS2_PR), \(pr) {
  sample_lakes %>%
    filter(WRS2_PR == pr) %>%
    select(id, poi_dist_m, nhd_source, nhd_id, WRS2_PR, Latitude, Longitude) %>%
    write_csv(file.path(dir_locs, paste0("locations_", pr, ".csv")))
})

sample_prs <- sample_prs %>%
  left_join(sample_lakes %>% count(WRS2_PR, name = "n_lakes_sampled"),
            by = "WRS2_PR")
write_csv(sample_prs, file.path(dir_out, "sample_pathrows.csv"))


# Document representativeness ---------------------------------------------

design <- pr_eco %>%
  count(ecoregion, name = "pop_pr") %>%
  left_join(pop_lakes, by = "ecoregion") %>%
  left_join(alloc %>% select(ecoregion, n_alloc), by = "ecoregion") %>%
  left_join(sample_prs %>%
              group_by(ecoregion) %>%
              summarize(sample_pr = n(), sample_lakes = sum(n_lakes_sampled)),
            by = "ecoregion") %>%
  mutate(across(c(n_alloc, sample_pr, sample_lakes), \(x) replace_na(x, 0)),
         pct_pop_lakes = round(100 * pop_lakes / sum(pop_lakes), 1),
         pct_sample_lakes = round(100 * sample_lakes / sum(sample_lakes), 1)) %>%
  arrange(desc(pop_lakes))
write_csv(design, file.path(dir_out, "sample_design_summary.csv"))
print(design, n = Inf, width = Inf)

wrs_sample <- wrs %>%
  left_join(pr_eco %>% select(WRS2_PR, ecoregion), by = "WRS2_PR") %>%
  mutate(sampled = WRS2_PR %in% sample_prs$WRS2_PR)

map <- ggplot() +
  geom_sf(data = wrs_sample, aes(fill = ecoregion), color = NA, alpha = 0.35) +
  geom_sf(data = filter(wrs_sample, sampled), fill = NA, color = "black", linewidth = 0.6) +
  # crop to the US and Caribbean (Pacific territories are off-map)
  coord_sf(crs = 4326, xlim = c(-180, -60), ylim = c(15, 72)) +
  labs(title = "Aerosol spot check: sampled path-rows (outlined)",
       subtitle = paste0(nrow(sample_prs), " path-rows, ",
                         nrow(sample_lakes), " lakes"),
       fill = "Level I ecoregion") +
  theme_bw() +
  theme(legend.position = "bottom", legend.text = element_text(size = 7)) +
  guides(fill = guide_legend(ncol = 3))
ggsave(file.path(dir_out, "sample_map.png"), map, width = 11, height = 8, dpi = 200)

message("Sampled ", nrow(sample_prs), " path-rows and ", nrow(sample_lakes),
        " lakes. Location files written to ", dir_locs)
