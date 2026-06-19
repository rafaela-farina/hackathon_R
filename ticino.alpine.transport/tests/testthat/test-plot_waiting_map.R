test_that("plot_waiting_map", {
  sf_poly <- sf::st_sf(ID = 1, geometry = sf::st_sfc(sf::st_polygon(list(matrix(c(8,45, 9,45, 9,46, 8,46, 8,45), ncol=2, byrow=TRUE))), crs=4326))
  test_path <- tempfile(fileext = ".shp")
  sf::write_sf(sf_poly, test_path)
  withr::defer(unlink(sub("\\.shp$", ".*", test_path)))

  p <- plot_waiting_map(date = "06/19/2026", query_times = "08:00",
                        shp_path = test_path, region_name = "Ticino")

  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$title, "Public transport waiting times - Ticino")
})
