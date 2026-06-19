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
get_routes_all <- function(from, date, time, num = 3) {
  actual_time <- convert_time(time)

  stations <- read_csv_data()

  destination_ids <- stations |>
    dplyr::filter(as.character(station_id) != as.character(from)) |>
    dplyr::pull(station_id) |>
    as.character()

  clean_date <- gsub("/", "_", date)
  clean_time <- gsub(":", "_", actual_time)

  cache_filename <- paste0(
    "cache/",
    from, "_all_",
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

  to_params <- stats::setNames(
    as.list(destination_ids),
    paste0("to[", seq_along(destination_ids) - 1, "]")
  )

  params <- c(
    list(
      from = from,
      date = date,
      time = actual_time,
      num = num,
      one_to_many = 1
    ),
    to_params
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
    api_response$results,
    function(result, result_index) {

      routes <- purrr::imap_dfr(
        result$connections,
        function(route, route_index) {

          points <- purrr::imap_dfr(
            route$legs,
            function(leg, leg_index) {

              start_point <- if (leg_index == 1) {
                tibble::tibble(
                  name = leg$name,
                  arrival_time = leg$departure,
                  lon = as.numeric(leg$lon),
                  lat = as.numeric(leg$lat)
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
                      lon = as.numeric(stop$lon),
                      lat = as.numeric(stop$lat)
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
                  lon = as.numeric(leg$exit$lon),
                  lat = as.numeric(leg$exit$lat)
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
            duration = as.numeric(route$duration),
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

      tibble::tibble(
        from = as.character(from),
        to = destination_ids[result_index],
        routes = list(routes)
      )
    }
  )

  saveRDS(df_response, file = cache_filename)

  return(df_response)
}
