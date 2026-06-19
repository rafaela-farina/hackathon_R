test_that("Check api sanity", {
  df_resp <- get_routes(8505300, 8505000, "06/19/2026", "11:17", 5)

  expect_equal(nrow(df_resp), 5)
})
