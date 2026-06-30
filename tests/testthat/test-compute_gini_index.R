test_that("compute_gini_index returns an ntl_data object", {

  cleaned_sf <- prepare_ntl_data()

  out_dir <- tempdir()

  gini <- compute_gini_index(
    cleaned_sf,
    output_dir = out_dir
  )

  expect_s3_class(gini, "gini_data")
  expect_s3_class(gini, "sf")

})