test_that("Check get_routes()", {
  df_resp <- get_routes(8505300, 8505000, "06/20/2026", "17:17", 5)

  expect_equal(nrow(df_resp), 5)
})

test_that("Check get_routes_all()", {
  df_resp <- get_routes_all(8505300, "06/20/2026", "17:17", 1)

  expect_equal(nrow(df_resp), 14)
})
