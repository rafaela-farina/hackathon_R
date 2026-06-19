test_that("plot_waiting_map", {
  p <- plot_waiting_map(
    date = "06/22/2026",
    query_times = c("08:00", "10:00", "12:00"),
    num = 3
  )

  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$title, "Public transport waiting times - Ticino / Alpine region")
})
