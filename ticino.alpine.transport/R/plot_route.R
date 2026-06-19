#' @title Plot a route map with connecting lines
#' @param routes Output of \code{get_routes()}: one row per route,
#'   with a nested \code{points} list-column containing stop coordinates
#'   (name, lon, lat).
#' @param shp_path Path to a Swiss boundary shapefile (.shp) from the
#'   2026_GEOM_TK folder.
#' @param route_index Index of the route to plot.
#' @param route_name Name used in the plot title.
#' @param origin_name Optional name of the origin station, highlighted
#'   with a star if its coordinates appear in the data.
#' @description
#' Draws one route by connecting consecutive points with straight segments.
#' The Swiss base map is read from the shapefile and transformed to
#' longitude/latitude (EPSG:4326).
#'
#' @returns A ggplot object.
#' @export
#'
#' @importFrom rlang .data
plot_route <- function(routes, shp_path,
                       route_index = 1,
                       route_name = "Route",
                       origin_name = NULL) {

  #ch <- sf::read_sf(shp_path)
  #ch <- sf::st_transform(ch, 4326)

  if (nrow(routes) == 0) {
    stop("routes is empty")
  }

  if (route_index < 1 || route_index > nrow(routes)) {
    stop("route_index is outside the available route range")
  }

  route_points <- routes$points[[route_index]]

  if (is.null(route_points) || nrow(route_points) == 0) {
    stop("Selected route has no points")
  }

  route_points$seq <- seq_len(nrow(route_points))

  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = ch,
      fill = "grey95",
      colour = "grey75",
      linewidth = 0.3
    ) +
    ggplot2::geom_path(
      data = route_points,
      ggplot2::aes(
        x = .data$lon,
        y = .data$lat
      ),
      linewidth = 0.7,
      colour = "steelblue"
    ) +
    ggplot2::geom_point(
      data = route_points,
      ggplot2::aes(
        x = .data$lon,
        y = .data$lat
      ),
      size = 1.2,
      colour = "steelblue"
    )

  if (!is.null(origin_name)) {
    origin_pt <- route_points[
      route_points$name == origin_name,
      ,
      drop = FALSE
    ]

    if (nrow(origin_pt) > 0) {
      origin_pt <- origin_pt[1, , drop = FALSE]

      p <- p +
        ggplot2::geom_point(
          data = origin_pt,
          ggplot2::aes(
            x = .data$lon,
            y = .data$lat
          ),
          shape = 8,
          size = 5,
          colour = "black",
          stroke = 1.2
        )
    }
  }

  p +
    ggplot2::coord_sf(
      xlim = range(route_points$lon, na.rm = TRUE) + c(-0.1, 0.1),
      ylim = range(route_points$lat, na.rm = TRUE) + c(-0.1, 0.1)
    ) +
    ggplot2::labs(
      title = route_name,
      x = NULL,
      y = NULL
    ) +
    ggplot2::theme_minimal()
}

#routes <- get_routes(
#  from = 8505300,
#  to = 8505000,
#  date = "06/20/2026",
#  time = "12:17",
#  num = 5
#)

#plot_route(
#  routes = routes,
#  shp_path = "data/2026_GEOM_TK/SHP/Boundaries_K4_Canton_20260101.shp",
#  route_index = 1,
#  route_name = "Lugano to Luzern",
#  origin_name = "Lugano"
#)
