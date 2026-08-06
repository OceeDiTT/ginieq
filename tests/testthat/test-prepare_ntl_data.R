test_that("prepare_ntl_data returns a valid spatial NTL object", {

  result <- prepare_ntl_data(country_code = "NGA")

  # Correct classes
  expect_s3_class(result, "gini_data")
  expect_s3_class(result, "sf")

  # Has geometry and a valid CRS
  expect_true("geometry" %in% attr(result, "sf_column") ||
                inherits(sf::st_geometry(result), "sfc"))
  expect_equal(sf::st_crs(result)$epsg, 4326)

  # Region identifier is retained
  expect_true("shapeID" %in% names(result))

  # Yearly aggregated columns are present
  expect_true(any(grepl("^NTL_Yearly_Mean_Y\\d{4}$", names(result))))

  # Raw geometry column has been consumed/removed
  expect_false(".geo" %in% names(result))

  # Country filter worked (all rows belong to NGA)
  if ("shapeGroup" %in% names(result)) {
    expect_true(all(result$shapeGroup == "NGA"))
  }

  # There is data
  expect_gt(nrow(result), 0)
})