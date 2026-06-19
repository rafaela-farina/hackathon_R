test_that("route_query_table returns a tibble", {
  result <- route_query_table()
  expect_s3_class(result, "tbl_df")
})

test_that("route_query_table has the expected columns", {
  result <- route_query_table()
  expect_named(
    result,
    c("group_id", "region", "from_city", "to_city",
      "from_station_id", "to_station_id", "query_date", "query_time")
  )
})

test_that("row count matches destinations x query_times", {
  station_data <- read_csv_data()
  n_destinations <- sum(station_data$is_origin == FALSE)
  n_times <- 5  # default query_times length
  
  result <- route_query_table()
  expect_equal(nrow(result), n_destinations * n_times)
})

test_that("custom query_date and query_times are respected", {
  result <- route_query_table(query_date = "2025-07-01", query_times = c("08:00"))
  expect_true(all(result$query_date == "2025-07-01"))
  expect_equal(unique(result$query_time), "08:00")
})

test_that("there is exactly one unique origin city", {
  result <- route_query_table()
  expect_equal(length(unique(result$from_city)), 1)
})