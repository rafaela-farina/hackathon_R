#' @title Plot a regional route map
#'
#' @param from Origin station ID.
#' @param date Travel date in MM/DD/YYYY format.
#' @param time Travel time in HH:MM format.
#' @param num Number of routes requested per destination.
#'
#' @description
#' Gets routes from the origin station to all stations in the region and
#' plots the first route for each destination.
#'
#' @return A ggplot object.
#'
#' @importFrom rlang .data
#' @export
plot_route_map <- function(from, date, time, num = 3) {
  stations <- read_csv_data()

  stations$station_id <- as.character(stations$station_id)
  from <- as.character(from)

  origin <- stations[
    stations$station_id == from,
    ,
    drop = FALSE
  ]

  if (nrow(origin) == 0) {
    stop("Origin station ID was not found")
  }

  origin_name <- origin$station_name[1]
  region_name <- origin$region[1]

  routes_all <- get_routes_all(
    from = from,
    date = date,
    time = time,
    num = num
  )

  routes_all$to <- as.character(routes_all$to)

  routes_all <- routes_all |>
    dplyr::left_join(
      stations |>
        dplyr::select(
          to = .data$station_id,
          destination_name = .data$station_name
        ),
      by = "to"
    )

  segments <- list()

  for (i in seq_len(nrow(routes_all))) {
    dest_routes <- routes_all$routes[[i]]

    if (is.null(dest_routes) || nrow(dest_routes) == 0) {
      next
    }

    pts <- dest_routes$points[[1]]

    if (is.null(pts) || nrow(pts) == 0) {
      next
    }

    pts$dest_id <- routes_all$destination_name[i]
    pts$seq <- seq_len(nrow(pts))

    segments[[length(segments) + 1]] <- pts
  }

  route_points <- dplyr::bind_rows(segments)

  if (nrow(route_points) == 0) {
    stop("No route points were found")
  }

  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = ticino.alpine.transport::saved_plot,,
      fill = "grey95",
      colour = "grey75",
      linewidth = 0.3
    ) +
    ggplot2::geom_path(
      data = route_points,
      ggplot2::aes(
        x = .data$lon,
        y = .data$lat,
        group = .data$dest_id,
        colour = .data$dest_id
      ),
      linewidth = 0.7
    ) +
    ggplot2::geom_point(
      data = route_points,
      ggplot2::aes(
        x = .data$lon,
        y = .data$lat,
        colour = .data$dest_id
      ),
      size = 1.2
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
      title = paste(
        "Public transport routes -",
        region_name
      ),
      colour = "Destination",
      x = NULL,
      y = NULL
    ) +
    ggplot2::theme_minimal()
}
