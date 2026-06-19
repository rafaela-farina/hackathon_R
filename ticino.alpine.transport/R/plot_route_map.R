#' @title Plot a regional route map with connecting lines
#' @param routes_all Output of \code{get_routes_all()}: one row per
#'   destination, with a nested \code{routes} list-column, each route
#'   carrying a \code{points} list-column of stop coordinates
#'   (name, lon, lat).
#' @param shp_path Path to a Swiss boundary shapefile (.shp) from the
#'   2026_GEOM_TK folder.
#' @param region_name Name of the region, used in the plot title.
#' @param origin_name Optional name of the origin station, highlighted
#'   with a star if its coordinates appear in the data.
#' @description
#' Draws simplified route lines from the assigned origin station to every
#' destination, by connecting consecutive stops of the first (main) route
#' with straight segments. The Swiss base map is read from the shapefile
#' and transformed to longitude/latitude (EPSG:4326). Each destination's
#' route is drawn in a different colour.
#'
#' @returns A ggplot object.
#' @export
#'
#' @importFrom rlang .data
plot_route_map <- function(routes_all, shp_path,
                           region_name = "Region",
                           origin_name = NULL) {

  stations <- read_csv_data()

  stations$station_id <- as.character(stations$station_id)
  routes_all$to <- as.character(routes_all$to)

  routes_all <- routes_all |>
    dplyr::left_join(
      stations |>
        dplyr::select(
          to = station_id,
          destination_name = station_name
        ),
      by = "to"
    )

  # Read and reproject the base map
  #ch <- sf::read_sf(shp_path)
  #ch <- sf::st_transform(ch, 4326)

  # Flatten the nested structure into one tidy table of points.
  # For each destination, take the first route and pull out its points.
  segments <- list()
  for (i in seq_len(nrow(routes_all))) {
    dest_routes <- routes_all$routes[[i]]
    if (is.null(dest_routes) || nrow(dest_routes) == 0) next

    # take the first (main) route to keep the map simple
    pts <- dest_routes$points[[1]]
    if (is.null(pts) || nrow(pts) == 0) next

    pts$dest_id <- routes_all$destination_name[i]
    pts$seq     <- seq_len(nrow(pts))
    segments[[length(segments) + 1]] <- pts
  }
  route_points <- dplyr::bind_rows(segments)

  # Build the plot
  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = ch, fill = "grey95", colour = "grey75",
                     linewidth = 0.3) +
    # route lines: one path per destination, ordered by stop sequence
    ggplot2::geom_path(
      data = route_points,
      ggplot2::aes(x = lon, y = lat,
                   group = dest_id, colour = dest_id),
      linewidth = 0.7
    ) +
    # stop points along the routes
    ggplot2::geom_point(
      data = route_points,
      ggplot2::aes(x = lon, y = lat, colour = dest_id),
      size = 1.2
    )

  # Optionally highlight the origin with a star
  if (!is.null(origin_name)) {
    origin_pt <- route_points[route_points$name == origin_name, ][1, ]
    if (!is.na(origin_pt$lon)) {
      p <- p +
        ggplot2::geom_point(
          data = origin_pt,
          ggplot2::aes(x = lon, y = lat),
          shape = 8, size = 5, colour = "black", stroke = 1.2
        )
    }
  }

  p +
    ggplot2::coord_sf(
      xlim = range(route_points$lon, na.rm = TRUE) + c(-0.1, 0.1),
      ylim = range(route_points$lat, na.rm = TRUE) + c(-0.1, 0.1)
    ) +
    ggplot2::labs(
      title = paste("Public transport routes -", region_name),
      colour = "Destination", x = NULL, y = NULL
    ) +
    ggplot2::theme_minimal()
}
