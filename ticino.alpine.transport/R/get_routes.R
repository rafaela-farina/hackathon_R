library(lubridate)
library(httr)
library(purrr)
library(dplyr)
library(tibble)

convert_time <- function(time) {
  new_time <- as.POSIXct(time, format = "%H:%M") |>
    ceiling_date("30 minutes") |>
    format("%H:%M")
  return(new_time)
}


#' @title TODO
#' @param TODO
#' @description
#' TODO
#'
#' @returns TODO
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
    dir.create("cache", recursive = TRUE)
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
          !is.na(lon),
          !is.na(lat)
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

  saveRDS(df_response, file = cache_filename)

  return(df_response)
}
