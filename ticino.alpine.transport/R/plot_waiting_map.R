#' @title Plot a regional waiting-time accessibility map
#' @param waiting A data frame with one row per city, containing columns:
#'   city, latitude, longitude, population, median_wait (median waiting
#'   time in minutes), and is_origin (logical, TRUE for the origin station).
#' @param shp_path Path to a Swiss boundary shapefile (.shp) from the
#'   2026_GEOM_TK folder, e.g. a canton or national boundary layer.
#' @param region_name Name of the region, used in the plot title.
#' @description
#' Produces a waiting-time accessibility map of a Swiss region. The Swiss
#' base map is read from the provided shapefile and transformed to
#' longitude/latitude (EPSG:4326). Destination cities are shown as points
#' sized by population and coloured by median waiting time. The origin
#' station is highlighted with a star.
#'
#' @returns A ggplot object.
#' @export
plot_waiting_map <- function(waiting, shp_path, region_name = "Region") {

  # Read Swiss base map and transform to lon/lat
  ch <- sf::read_sf(shp_path)
  ch <- sf::st_transform(ch, 4326)

  origin <- waiting[waiting$is_origin, ]
  dests  <- waiting[!waiting$is_origin, ]

  ggplot2::ggplot() +
    ggplot2::geom_sf(data = ch, fill = "grey95", colour = "grey75",
                     linewidth = 0.3) +
    # destination points: size = population, colour = median wait
    ggplot2::geom_point(
      data = dests,
      ggplot2::aes(x = longitude, y = latitude,
                   size = population, colour = median_wait)
    ) +
    ggplot2::geom_text(
      data = dests,
      ggplot2::aes(x = longitude, y = latitude, label = city),
      vjust = -1, size = 3
    ) +
    # origin station as a star
    ggplot2::geom_point(
      data = origin,
      ggplot2::aes(x = longitude, y = latitude),
      shape = 8, size = 5, colour = "black", stroke = 1.2
    ) +
    ggplot2::geom_text(
      data = origin,
      ggplot2::aes(x = longitude, y = latitude, label = city),
      vjust = -1.5, fontface = "bold", size = 3.5
    ) +
    ggplot2::scale_colour_viridis_c(
      name = "Median wait (min)", option = "plasma", direction = -1
    ) +
    ggplot2::scale_size_continuous(name = "Population") +
    # zoom to the data with a small margin
    ggplot2::coord_sf(
      xlim = range(waiting$longitude) + c(-0.3, 0.3),
      ylim = range(waiting$latitude)  + c(-0.3, 0.3)
    ) +
    ggplot2::labs(
      title = paste("Public transport waiting times -", region_name),
      x = NULL, y = NULL
    ) +
    ggplot2::theme_minimal()
}


# --- Hardcoded demo data (remove once real data is wired in) ----------
demo_waiting <- data.frame(
  city        = c("Bellinzona", "Lugano", "Locarno", "Mendrisio", "Biasca"),
  latitude    = c(46.1956, 46.0037, 46.1670, 45.8696, 46.3590),
  longitude   = c( 9.0238,  8.9511,  8.7943,  8.9810,  8.9710),
  population  = c(  43670,   62000,   16000,   15500,    6100),
  median_wait = c(      0,      12,      18,      25,      35),
  is_origin   = c(   TRUE,   FALSE,   FALSE,   FALSE,   FALSE)
)

# Run this to preview (point shp_path at a .shp from 2026_GEOM_TK):
#   p <- plot_waiting_map(demo_waiting,
#                         shp_path = "2026_GEOM_TK/.../cantons.shp",
#                         region_name = "Ticino")
#   print(p)
