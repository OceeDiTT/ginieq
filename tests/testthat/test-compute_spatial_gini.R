test_that("compute_spatial_gini returns a valid per-year Gini table", {

  skip_if_not_installed("sf")

  # Build a small sf fixture with yearly mean NTL columns and a pop column
  df <- data.frame(
    NTL_Yearly_Mean_Y2019 = c(10, 20, 30),
    NTL_Yearly_Mean_Y2021 = c(15, 25, 35),
    pop                   = c(100, 200, 300),
    lon                   = c(3.0, 3.1, 3.2),
    lat                   = c(6.0, 6.1, 6.2),
    stringsAsFactors      = FALSE
  )
  test_sf <- sf::st_as_sf(df, coords = c("lon", "lat"), crs = 4326)

  result <- compute_spatial_gini(test_sf)

  # Correct class and type
  expect_s3_class(result, "spatial_gini")
  expect_s3_class(result, "data.frame")

  # Expected columns present
  expect_true(all(c("Year", "Mean_NTL",
                    "Gini_Coefficient", "Gini_Category") %in% names(result)))

  # One row per year plus the summary row
  expect_equal(nrow(result), 3)  # Y2019, Y2021, All_Years_Mean
  expect_true("All_Years_Mean" %in% result$Year)

  # Gini coefficients are numeric and within valid 0-100 range
  expect_type(result$Gini_Coefficient, "double")
  expect_true(all(result$Gini_Coefficient >= 0 &
                  result$Gini_Coefficient <= 100))
})