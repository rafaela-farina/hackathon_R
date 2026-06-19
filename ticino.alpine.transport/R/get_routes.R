convert_time <- function(time) {
  new_time <- as.POSIXct(
    time,
    format = "%H:%M"
  ) |>
    lubridate::ceiling_date("30 minutes") |>
    format("%H:%M")

  return(new_time)
}


#' @title Retrieve Routes
#'
#' @param from Character or numeric station identifier for the departure
#'   station.
#' @param to Character or numeric station identifier for the destination
#'   station.
#' @param date Character string specifying the travel date in the format
#'   accepted by the search.ch timetable API.
#' @param time Character string specifying the departure time in `"HH:MM"`
#'   format. The time is rounded up to the next 30-minute interval.
#' @param num Integer specifying the maximum number of connections to retrieve.
#'   Defaults to 5.
#'
#' @description
#' Retrieves public transport routes between the specified departure and
#' destination stations. Results are cached as RDS files to avoid repeated API
#' requests for identical parameters.
#'
#' @return A tibble containing the retrieved routes. The `points` column is a
#'   list-column containing the geographic points belonging to each route.
#'
#' @importFrom dplyr filter
#' @export
get_routes <- function(from, to, date, time, num = 5) {
  actual_time <- convert_time(time)

  clean_date <- gsub("/", "_", date)
  clean_time <- gsub(":", "_", actual_time)

  cache_filename <- paste0(
    "cache/",
    from, "_",
    to, "_",
    clean_date, "_",
    clean_time, "_",
    num,
    ".rds"
  )

  if (!dir.exists("cache")) {
    dir.create(
      "cache",
      recursive = TRUE
    )
  }

  if (file.exists(cache_filename)) {
    return(readRDS(cache_filename))
  }

  params <- list(
    from = from,
    to = to,
    date = date,
    time = actual_time,
    num = num
  )

  response <- httr::GET(
    "https://search.ch/timetable/api/route.json",
    query = params
  )

  httr::stop_for_status(response)

  api_response <- httr::content(
    response,
    as = "parsed",
    type = "application/json"
  )

  df_response <- purrr::imap_dfr(
    api_response$connections,
    function(route, route_index) {
      points <- purrr::imap_dfr(
        route$legs,
        function(leg, leg_index) {
          start_point <- if (leg_index == 1) {
            tibble::tibble(
              name = leg$name,
              arrival_time = leg$departure,
              lon = leg$lon,
              lat = leg$lat
            )
          } else {
            tibble::tibble()
          }

          stop_points <- if (
            !is.null(leg$stops) &&
            length(leg$stops) > 0
          ) {
            purrr::map_dfr(
              leg$stops,
              function(stop) {
                tibble::tibble(
                  name = stop$name,
                  arrival_time = if (!is.null(stop$arrival)) {
                    stop$arrival
                  } else {
                    NA_character_
                  },
                  lon = stop$lon,
                  lat = stop$lat
                )
              }
            )
          } else {
            tibble::tibble()
          }

          exit_point <- if (!is.null(leg$exit)) {
            tibble::tibble(
              name = leg$exit$name,
              arrival_time = leg$exit$arrival,
              lon = leg$exit$lon,
              lat = leg$exit$lat
            )
          } else {
            tibble::tibble()
          }

          dplyr::bind_rows(
            start_point,
            stop_points,
            exit_point
          )
        }
      ) |>
        dplyr::filter(
          !is.na(.data$lon),
          !is.na(.data$lat)
        ) |>
        dplyr::distinct()

      tibble::tibble(
        route_id = paste0("route", route_index),
        duration = route$duration,
        from = route$from,
        departure = route$departure,
        to = route$to,
        arrival = route$arrival,
        is_main = route$is_main,
        occupancy = route$occupancy,
        points = list(points)
      )
    }
  )

  saveRDS(
    df_response,
    file = cache_filename
  )

  return(df_response)
}
