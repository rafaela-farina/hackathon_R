# R script extracted from R Markdown file
# Source: e2489e45-ad6d-42d6-be3c-2e6d7bd029c5.Rmd

# ---- Chunk 1 ----
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)

# ---- Chunk 2 ----
library(tidyverse)
library(zoo)
#library(devtools)
library(roxygen2)
library(testthat)
library(knitr)

# ---- Chunk 3 ----
#read_station_data <- function(path = "data/SwissCities.csv") {
#  swiss_df <- read.csv(path, fileEncoding = "UTF-8")
 # swiss_df <- unique(swiss_df)
#  return(swiss_df)
#}
#swiss_df <- read_station_data()

# ---- Chunk 4 ----
read_station_data <- function(path = "inst/extdata/SwissCities.csv") {
  swiss_df <- read.csv(path)

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


  # show data
  #head(swiss_df)

  swiss_df <- unique(swiss_df)

  group <- swiss_df[swiss_df$group_id == 4, ]

  return(group)
}
swiss_df <- read_station_data()



