test_that("read data test", {
  df_test <- read_csv_data()
  expect_equal(nrow(df_test), 15)
})
