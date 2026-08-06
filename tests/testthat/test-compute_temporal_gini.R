test_that("compute_temporal_gini returns a valid temporal_gini sf object", {

  skip_if_not_installed("sf")

  # Build a minimal sf fixture with yearly mean NTL columns + population
  geom <- sf::st_sfc(
    sf::st_point(c(0, 0)),
    sf::st_point(c(1, 1)),
    sf::st_point(c(2, 2))
  )

  fixture <- sf::st_sf(
    shapeID              = c("A", "B", "C"),
    NTL_Yearly_Mean_Y2017 = c(10, 20, 30),
    NTL_Yearly_Mean_Y2019 = c(15, 25, 35),
    NTL_Yearly_Mean_Y2021 = c(20, 30, 40),
    pop                  = c(100, 200, 300),
    geometry             = geom
  )

  result <- compute_temporal_gini(fixture)

  # Correct class and type
  expect_s3_class(result, "temporal_gini")
  expect_s3_class(result, "sf")

  # New columns were added
  expect_true("Gini_Coefficient" %in% names(result))
  expect_true("Gini_Category" %in% names(result))

  # One Gini value per region, geometry preserved
  expect_equal(nrow(result), nrow(fixture))

  # Gini coefficient is within the expected 0–100 range
  expect_true(all(result$Gini_Coefficient >= 0 &
                  result$Gini_Coefficient <= 100, na.rm = TRUE))
})