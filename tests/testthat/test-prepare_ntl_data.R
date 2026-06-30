test_that("prepare_ntl_data works", {
  expect_no_error(prepare_ntl_data())
})

test_that("prepare_ntl_data returns an sf object", {
  x <- prepare_ntl_data()

  expect_s3_class(x, "sf")
  expect_true(inherits(x, "data.frame"))
})

test_that("prepare_ntl_data creates expected columns", {

  x <- prepare_ntl_data()

  expected_cols <- c(
    "NTL_Jan_Mean",
    "NTL_Feb_Mean",
    "NTL_Mar_Mean",
    "NTL_Apr_Mean",
    "NTL_May_Mean",
    "NTL_Jun_Mean",
    "NTL_Jul_Mean",
    "NTL_Aug_Mean",
    "NTL_Sep_Mean",
    "NTL_Oct_Mean",
    "NTL_Nov_Mean",
    "NTL_Dec_Mean"
  )

  expect_true(all(expected_cols %in% names(x)))
})

test_that("prepare_ntl_data returns valid geometry", {

  x <- prepare_ntl_data()

  expect_s3_class(sf::st_geometry(x), "sfc")
})