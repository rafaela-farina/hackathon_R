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
  actual_time = convert_time(time)

  clean_date <- gsub("/", "_", date)
  clean_time <- gsub(":", "_", actual_time)

  cache_dir <- tools::R_user_dir(
    "ticino.alpine.transport",
    which = "cache"
  )

  dir.create(
    cache_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )

  cache_filename <- file.path(
    cache_dir,
    paste0(
      from, "_",
      to, "_",
      clean_date, "_",
      clean_time, "_",
      num,
      ".rds"
    )
  )

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

  response <- GET("https://search.ch/timetable/api/route.json", query = params)

  stop_for_status(response)

  api_response <- content(response, as = "parsed", type = "application/json")

  df_response <- map_dfr(
    api_response$connections,
    function(route) {
      route$legs <- NULL
      as_tibble(route)
    }
  )

  saveRDS(df_response, file = cache_filename)
  return(df_response)

}
