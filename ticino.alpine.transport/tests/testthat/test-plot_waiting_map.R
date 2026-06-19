library(testthat)
library(sf)
library(ggplot2)

# Usiamo un percorso temporaneo moderno per lo shapefile
test_path <- tempfile(fileext = ".shp")

# Creiamo il file temporaneo direttamente all'inizio del file
p1 <- matrix(c(8.5, 45.5, 9.5, 45.5, 9.5, 46.5, 8.5, 46.5, 8.5, 45.5), ncol = 2, byrow = TRUE)
poly <- sf::st_polygon(list(p1))
geom <- sf::st_sfc(poly, crs = 4326)
df_sf <- sf::st_sf(ID = 1, geometry = geom)
sf::write_sf(df_sf, test_path)

# Dati di prova (mock) per il test
mock_waiting <- data.frame(
  city        = c("Bellinzona", "Lugano"),
  latitude    = c(46.1956, 46.0037),
  longitude   = c( 9.0238,  8.9511),
  population  = c(  43670,   62000),
  median_wait = c(      0,      12),
  is_origin   = c(   TRUE,  FALSE)
)

# Il test vero e proprio
test_that("plot_waiting_map genera un grafico ggplot corretto", {
  # Quando finisce questo blocco di test, puliamo i file temporanei
  withr::defer(if (file.exists(test_path)) {
    file.remove(test_path)
    file.remove(sub("\\.shp$", ".shx", test_path))
    file.remove(sub("\\.shp$", ".dbf", test_path))
    file.remove(sub("\\.shp$", ".prj", test_path))
  })

  p <- plot_waiting_map(waiting = mock_waiting,
                        shp_path = test_path,
                        region_name = "Ticino")

  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$title, "Public transport waiting times - Ticino")
})
