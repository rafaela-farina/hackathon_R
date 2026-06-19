#' @title Calculate and plot regional waiting-time accessibility
#' @param date Travel date in MM/DD/YYYY format.
#' @param query_times Character vector of query times in HH:MM format.
#' @param shp_path Path to a Swiss boundary shapefile.
#' @param region_name Name of the region used in the plot title.
#' @param num Number of connections requested per destination.
#' @description
#' Calculates the next available connection waiting time for each destination
#' and query time, summarises the waiting time by destination, and plots the
#' result on a Swiss regional map.
#'
#' @returns A ggplot object.
#' @export
plot_waiting_map <- function(
    date,
    query_times,
    shp_path,
    region_name = "Region",
    num = 3
) {

  stations <- read_csv_data()

  stations$station_id <- as.character(stations$station_id)
  stations$is_origin <- as.logical(stations$is_origin)

  origin_id <- stations$station_id[stations$is_origin][1]

  waiting <- purrr::map_dfr(
    query_times,
    function(query_time) {

      routes_all <- get_routes_all(
        from = origin_id,
        date = date,
        time = query_time,
        num = num
      )

      query_date <- as.Date(
        date,
        format = "%m/%d/%Y"
      )

      query_ts <- as.POSIXct(
        paste(query_date, query_time),
        format = "%Y-%m-%d %H:%M",
        tz = "Europe/Zurich"
      )

      purrr::map_dfr(
        seq_len(nrow(routes_all)),
        function(i) {

          routes <- routes_all$routes[[i]]

          if (is.null(routes) || nrow(routes) == 0) {
            return(tibble::tibble())
          }

          departure_ts <- as.POSIXct(
            routes$departure,
            format = "%Y-%m-%d %H:%M:%S",
            tz = "Europe/Zurich"
          )

          wait_min <- as.numeric(
            difftime(
              departure_ts,
              query_ts,
              units = "mins"
            )
          )

          wait_min <- wait_min[
            !is.na(wait_min) &
              wait_min >= 0
          ]

          if (length(wait_min) == 0) {
            return(tibble::tibble())
          }

          tibble::tibble(
            station_id = as.character(routes_all$to[i]),
            query_time = query_time,
            wait_min = min(wait_min)
          )
        }
      )
    }
  )

  waiting_summary <- waiting |>
    dplyr::group_by(.data$station_id) |>
    dplyr::summarise(
      median_wait = median(.data$wait_min),
      mean_wait = mean(.data$wait_min),
      n_queries = dplyr::n(),
      .groups = "drop"
    )

  waiting_data <- stations |>
    dplyr::left_join(
      waiting_summary,
      by = "station_id"
    )

  waiting_data$median_wait[waiting_data$is_origin] <- 0
  waiting_data$mean_wait[waiting_data$is_origin] <- 0
  waiting_data$n_queries[waiting_data$is_origin] <- length(query_times)

  ch <- sf::read_sf(shp_path)
  ch <- sf::st_transform(ch, 4326)

  origin <- waiting_data[waiting_data$is_origin, ]
  dests <- waiting_data[
    !waiting_data$is_origin &
      !is.na(waiting_data$median_wait),
  ]

  ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = ch,
      fill = "grey95",
      colour = "grey75",
      linewidth = 0.3
    ) +
    ggplot2::geom_point(
      data = dests,
      ggplot2::aes(
        x = .data$longitude,
        y = .data$latitude,
        size = .data$population,
        colour = .data$median_wait
      )
    ) +
    ggplot2::geom_text(
      data = dests,
      ggplot2::aes(
        x = .data$longitude,
        y = .data$latitude,
        label = .data$city
      ),
      vjust = -1,
      size = 3
    ) +
    ggplot2::geom_point(
      data = origin,
      ggplot2::aes(
        x = .data$longitude,
        y = .data$latitude
      ),
      shape = 8,
      size = 5,
      colour = "black",
      stroke = 1.2
    ) +
    ggplot2::geom_text(
      data = origin,
      ggplot2::aes(
        x = .data$longitude,
        y = .data$latitude,
        label = .data$city
      ),
      vjust = -1.5,
      fontface = "bold",
      size = 3.5
    ) +
    ggplot2::scale_colour_viridis_c(
      name = "Median wait (min)",
      option = "plasma",
      direction = -1
    ) +
    ggplot2::scale_size_continuous(
      name = "Population"
    ) +
    ggplot2::coord_sf(
      xlim = range(
        waiting_data$longitude,
        na.rm = TRUE
      ) + c(-0.3, 0.3),
      ylim = range(
        waiting_data$latitude,
        na.rm = TRUE
      ) + c(-0.3, 0.3)
    ) +
    ggplot2::labs(
      title = paste(
        "Public transport waiting times -",
        region_name
      ),
      x = NULL,
      y = NULL
    ) +
    ggplot2::theme_minimal()
}
#plot_waiting_map(
#  date = "06/22/2026",
#  query_times = c("08:00", "10:00", "12:00"),
#  shp_path = "data/2026_GEOM_TK/SHP/Boundaries_K4_Canton_20260101.shp",
#  region_name = "Ticino / Alpine region",
#  num = 3
#)
