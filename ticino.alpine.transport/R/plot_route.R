#' @title Plot a route map with connecting lines
#'
#' @param from Origin station ID.
#' @param to Destination station ID.
#' @param date Travel date in MM/DD/YYYY format.
#' @param time Travel time in HH:MM format.
#' @param num Number of routes to request.
#' @param route_index Index of the route to plot.
#'
#' @description
#' Gets routes using \code{get_routes()} and plots one selected route by
#' connecting consecutive points with straight segments.
#'
#' @return A ggplot object.
#'
#' @importFrom rlang .data
#' @export
plot_route <- function(
    from,
    to,
    date,
    time,
    num = 5,
    route_index = 1
) {
  routes <- get_routes(
    from = from,
    to = to,
    date = date,
    time = time,
    num = num
  )

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

  stations <- read_csv_data()

  stations$station_id <- as.character(stations$station_id)
  from <- as.character(from)
  to <- as.character(to)

  origin_name <- stations$station_name[
    stations$station_id == from
  ][1]

  destination_name <- stations$station_name[
    stations$station_id == to
  ][1]

  route_name <- paste(
    origin_name,
    "to",
    destination_name
  )

  route_points$seq <- seq_len(nrow(route_points))

  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = ticino.alpine.transport::saved_plot,
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

  origin_pt <- route_points[
    route_points$name == origin_name,
    ,
    drop = FALSE
  ]

  if (nrow(origin_pt) > 0) {
    p <- p +
      ggplot2::geom_point(
        data = origin_pt[1, , drop = FALSE],
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

  p +
    ggplot2::coord_sf(
      xlim = range(
        route_points$lon,
        na.rm = TRUE
      ) + c(-0.1, 0.1),
      ylim = range(
        route_points$lat,
        na.rm = TRUE
      ) + c(-0.1, 0.1)
    ) +
    ggplot2::labs(
      title = route_name,
      x = NULL,
      y = NULL
    ) +
    ggplot2::theme_minimal()
}
