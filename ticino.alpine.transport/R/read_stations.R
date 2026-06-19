#' @title Calculate waiting time by destination
#' @param date Travel date in MM/DD/YYYY format.
#' @param query_times Character vector of query times in HH:MM format.
#' @param num Number of connections requested per destination.
#' @description
#' For each query time, calculates the waiting time until the next available
#' connection for every destination. Keeps the smallest non-negative waiting
#' time per destination and query time, then summarises waiting times by
#' destination.
#'
#' @returns A data frame with station data, median_wait, mean_wait,
#'   and n_queries.
#' @export
calculate_waiting <- function(date, query_times, num = 3) {

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

      query_date <- as.Date(date, format = "%m/%d/%Y")

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
            !is.na(wait_min) & wait_min >= 0
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
      median_wait = stats::median(.data$wait_min),
      mean_wait = mean(.data$wait_min),
      n_queries = dplyr::n(),
      .groups = "drop"
    )

  result <- stations |>
    dplyr::left_join(
      waiting_summary,
      by = "station_id"
    )

  result$median_wait[result$is_origin] <- 0
  result$mean_wait[result$is_origin] <- 0
  result$n_queries[result$is_origin] <- length(query_times)

  return(result)
}
#waiting <- calculate_waiting(
#  date = "06/22/2026",
#  query_times = c("08:00", "10:00", "12:00", "14:00"),
#  num = 3
#)
