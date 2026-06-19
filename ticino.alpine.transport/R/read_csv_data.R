library(tidyverse)
library(zoo)
#library(devtools)
library(roxygen2)
library(testthat)
library(knitr)

#' @title TODO
#' @param TODO
#' @description
#' TODO
#'
#' @returns TODO
#' @export
read_csv_data <- function() {
  csv_path <- system.file(
    "extdata",
    "SwissCities.csv",
    package = "ticino.alpine.transport"
  )

  swiss_df <- read.csv(csv_path, sep = ",")

  swiss_df$city <- gsub("G<c3><b6>schenen", "Göschenen", swiss_df$city)
  names(swiss_df) <- tolower(names(swiss_df))
  names(swiss_df) <- gsub(" ", "_", names(swiss_df))

  # remove duplicated rows
  swiss_df <- unique(swiss_df)

  # Convert numeric columns
  if ("latitude" %in% names(swiss_df)) {
    swiss_df$latitude <- as.numeric(swiss_df$latitude)
  }

  if ("longitude" %in% names(swiss_df)) {
    swiss_df$longitude <- as.numeric(swiss_df$longitude)
  }

  if ("population" %in% names(swiss_df)) {
    swiss_df$population <- as.numeric(swiss_df$population)
  }
  swiss_df <- swiss_df[rowSums(is.na(swiss_df) | swiss_df == "") < ncol(swiss_df), ]

  swiss_df <- unique(swiss_df)

  group <- swiss_df[swiss_df$group_id == 4, ]

  return(group)
}
