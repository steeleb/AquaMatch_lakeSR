#' @title Map one site-grouping example
#'
#' @param ex output of `collect_example()`
#' @param title optional plot title, defaults to the title stored in `ex`
#' @param show_buffers logical, whether to draw the extraction buffers
#' @param label_n minimum group size to label with the number of sites
#' @param color_context logical, whether to color the sites that are not part of
#' the example (and their flowlines) by the NHD flowline they are attributed to.
#' The `n_context` most common flowlines are colored, the rest are gray.
#' @param n_context number of flowlines to color if `color_context` is TRUE
#'
#' @returns a ggplot object
#'
map_crosswalk_example <- function(ex, title = ex$title, show_buffers = TRUE,
                                  label_n = 2, color_context = FALSE,
                                  n_context = 4) {

  bb <- ex$bbox
  sites <- ex$sites %>%
    mutate(type = case_when(role == "grouped" ~ "grouped",
                            n_sites_in_cluster == 1 ~ "ungrouped",
                            .default = "extraction"),
           type = factor(type, levels = c("extraction", "ungrouped", "grouped")))

  # sites that share a location with their extraction site are hidden beneath it
  grouped_visible <- sites %>%
    filter(type == "grouped", dist_to_extraction_site_m > 0)
  labels <- sites %>%
    filter(type == "extraction", focus, n_sites_in_cluster >= label_n)

  # a scale bar, in the lower left of the window
  w <- bb[["xmax"]] - bb[["xmin"]]
  sb_len <- c(50, 100, 200, 250, 500, 1000, 2000)
  sb_len <- max(sb_len[sb_len <= w / 4])
  sb_x <- bb[["xmin"]] + 0.04 * w
  sb_y <- bb[["ymin"]] + 0.05 * (bb[["ymax"]] - bb[["ymin"]])

  # label the largest waterbody polygons in the window (by name, or by type if
  # unnamed), so that it is clear what the blue areas are
  win_poly <- sf::st_as_sfc(bb)
  wb_labels <- suppressWarnings(ex$wb %>%
    sf::st_intersection(win_poly)) %>%
    mutate(visible_area = as.numeric(sf::st_area(.)),
           label = case_when(!is.na(gnis_name) & gnis_name != "" ~ gnis_name,
                             ftype == 390 ~ "Lake/Pond",
                             ftype == 436 ~ "Reservoir",
                             ftype == 493 ~ "Estuary",
                             .default = "Waterbody")) %>%
    filter(visible_area > 0.03 * as.numeric(sf::st_area(win_poly))) %>%
    arrange(desc(visible_area)) %>%
    slice_head(n = 2)
  if (nrow(wb_labels) > 0) {
    # place the label away from the site buffers when there is room to
    wb_labels <- suppressWarnings({
      free <- wb_labels %>% sf::st_make_valid()
      if (nrow(ex$buffers) > 0) {
        free_geom <- sf::st_difference(sf::st_geometry(free),
                                       sf::st_union(sf::st_geometry(ex$buffers)))
        keep <- !sf::st_is_empty(free_geom)
        sf::st_geometry(free)[keep] <- free_geom[keep]
      }
      sf::st_point_on_surface(free)
    })
  }

  p <- ggplot() +
    geom_sf(data = ex$wb, fill = "#D6E6F2", colour = "#9DB8CC", linewidth = 0.2) +
    # the waterbody that grouping is based on has a darker outline
    geom_sf(data = ex$wb %>% filter(focus), fill = NA, colour = "#3B6E94",
            linewidth = 0.6) +
    geom_sf(data = ex$fl, colour = "#9DB8CC", linewidth = 0.3) +
    geom_sf_text(data = wb_labels, aes(label = label), colour = "#3B6E94",
                 fontface = "italic", size = 3) +
    geom_sf(data = ex$fl %>% filter(focus), colour = "#3B6E94", linewidth = 1.2)

  if (show_buffers) {
    p <- p +
      geom_sf(data = ex$buffers, fill = NA, colour = "#C9591C",
              linewidth = 0.3, linetype = "dashed")
  }
  if (!is.null(ex$links)) {
    p <- p + geom_sf(data = ex$links %>%
                       filter(extraction_siteSR_id %in%
                                sites$extraction_siteSR_id[sites$focus]),
                     colour = "grey30", linewidth = 0.3)
  }

  # context sites (and their flowlines) colored by flowline attribution ----------
  # colorblind-friendly (Okabe-Ito) colors that are not used for the sites,
  # buffers, or water in the maps
  ctx_pal <- c("#009E73", "#CC79A7", "#F0E442", "#56B4E9")
  ctx_layers <- list()
  ctx_scale <- NULL
  if (color_context) {
    ctx_info <- sites %>%
      sf::st_drop_geometry() %>%
      filter(!focus, !is.na(fl_nhd_id)) %>%
      count(fl_nhd_id, fl_gnis_name, sort = TRUE) %>%
      slice_head(n = n_context) %>%
      mutate(base = if_else(is.na(fl_gnis_name) | fl_gnis_name == "",
                            "Unnamed flowline", fl_gnis_name),
             # flowlines that share a name (including the name of the
             # example's own flowline) are different reaches of the same feature
             n_same = ave(seq_along(base), base, FUN = length),
             ctx = case_when(n_same > 1 ~ paste0(base, ", reach ",
                                                 ave(seq_along(base), base, FUN = seq_along)),
                             base == ex$focus_name ~ paste0(base, ", other reach"),
                             .default = base))
    ctx_levels <- c(ctx_info$ctx, "Other flowlines or none")
    ctx_values <- setNames(c(ctx_pal[seq_len(nrow(ctx_info))], "grey55"), ctx_levels)
    ctx_sites <- sites %>%
      filter(!focus) %>%
      mutate(ctx = factor(coalesce(ctx_info$ctx[match(fl_nhd_id, ctx_info$fl_nhd_id)],
                                   "Other flowlines or none"), levels = ctx_levels))
    ctx_fl <- ex$fl %>%
      filter(!focus, permanent_identifier %in% ctx_info$fl_nhd_id) %>%
      mutate(ctx = factor(ctx_info$ctx[match(permanent_identifier, ctx_info$fl_nhd_id)],
                          levels = ctx_levels))
    # faded (alpha), with a gray underlay so that light colors remain visible
    ctx_layers <- list(
      geom_sf(data = ctx_fl, colour = "grey40", linewidth = 1.3, alpha = 0.5),
      geom_sf(data = ctx_fl, aes(colour = ctx), linewidth = 0.9, alpha = 0.75),
      geom_sf(data = ctx_sites, colour = "grey30", shape = 16, size = 2.2,
              alpha = 0.5),
      geom_sf(data = ctx_sites, aes(colour = ctx), shape = 16, size = 1.6,
              alpha = 0.8))
    ctx_scale <- scale_colour_manual(name = "Sites attributed to flowline",
                                     values = ctx_values, drop = FALSE)
  }

  # only list the kinds of sites that are part of the example in the legend
  legend_types <- levels(sites$type)[levels(sites$type) %in% sites$type[sites$focus]]

  legend_labels <- c(extraction = "Extraction site of a group (sent to GEE)",
                     ungrouped = "Ungrouped site (sent to GEE)",
                     grouped = "Grouped site (not sent to GEE)")

  p +
    # sites that are not part of the example are faded (or colored by flowline)
    (if (color_context) ctx_layers else list(
      geom_sf(data = sites %>% filter(!focus),
              colour = "grey55", fill = "grey85", shape = 21, size = 1.4,
              stroke = 0.4))) +
    ctx_scale +
    geom_sf(data = grouped_visible %>% filter(focus), aes(shape = type, fill = type),
            colour = "grey20", size = 1.8, stroke = 0.6, key_glyph = "point") +
    geom_sf(data = sites %>% filter(focus, type != "grouped"),
            aes(shape = type, fill = type), colour = "black", size = 2.6,
            stroke = 0.6, key_glyph = "point") +
    geom_sf_label(data = labels, aes(label = n_sites_in_cluster),
                  nudge_y = 0.07 * (bb[["ymax"]] - bb[["ymin"]]),
                  size = 2.8, label.padding = unit(0.12, "lines"),
                  fill = alpha("white", 0.8), linewidth = 0.15) +
    annotate("segment", x = sb_x, xend = sb_x + sb_len, y = sb_y, yend = sb_y,
             linewidth = 1) +
    annotate("text", x = sb_x + sb_len / 2, y = sb_y, label = paste(sb_len, "m"),
             vjust = -0.6, size = 2.8) +
    scale_shape_manual(
      name = NULL, breaks = legend_types,
      values = c(extraction = 23, ungrouped = 24, grouped = 21),
      labels = \(x) legend_labels[x]) +
    scale_fill_manual(
      name = NULL, breaks = legend_types,
      values = c(extraction = "#E8833A", ungrouped = "#7B3294", grouped = "white"),
      labels = \(x) legend_labels[x]) +
    # data are in a local UTM zone, axes are labeled in decimal degrees
    coord_sf(xlim = bb[c("xmin", "xmax")], ylim = bb[c("ymin", "ymax")],
             expand = FALSE, crs = sf::st_crs(ex$crs), datum = sf::st_crs(4326)) +
    scale_x_continuous(breaks = scales::breaks_extended(n = 3)) +
    scale_y_continuous(breaks = scales::breaks_extended(n = 4)) +
    labs(title = title, x = NULL, y = NULL) +
    theme_bw(base_size = 9) +
    theme(legend.position = "bottom",
          legend.direction = "vertical",
          legend.key = element_blank(),
          legend.background = element_blank(),
          axis.text = element_text(size = 6),
          panel.grid = element_blank(),
          panel.background = element_rect(fill = "#F7F5F0"),
          plot.title = element_text(size = 9, face = "bold"))

}


#' @title Figure height for a map of an example
#'
#' @description
#' `map_crosswalk_example()` keeps the map's aspect ratio, so the figure height
#' needed depends on the shape of the map window.
#'
#' @param ex output of `collect_example()`
#' @param width figure width in inches
#' @param extra inches added for the title, axis labels and legend
#'
#' @returns numeric figure height in inches
#'
crosswalk_fig_height <- function(ex, width = 6.5, extra = 2) {
  bb <- ex$bbox
  # the plot panel is narrower than the figure because of axis labels
  panel_w <- width - 0.8
  panel_w * (bb[["ymax"]] - bb[["ymin"]]) / (bb[["xmax"]] - bb[["xmin"]]) + extra
}
