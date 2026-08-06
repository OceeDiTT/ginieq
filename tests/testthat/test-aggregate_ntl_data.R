test_that("aggregate_ntl_data returns a valid merged NTL object", {

  result <- aggregate_ntl_data()

  # Correct class and type
  expect_s3_class(result, "ntl_merged")
  expect_s3_class(result, "data.frame")

  # Region identifier is present
  expect_true("shapeID" %in% names(result))

  # Yearly aggregated columns were created for each packaged year
  expect_true(any(grepl("^NTL_Yearly_Mean_Y\\d{4}$", names(result))))
  expect_true(any(grepl("^NTL_Yearly_Sum_Y\\d{4}$",  names(result))))

  # No raw monthly columns should remain
  expect_false(any(grepl("mean_\\d{4}-\\d{2}-\\d{2}$", names(result))))
  expect_false(any(grepl("sum_\\d{4}-\\d{2}-\\d{2}$",  names(result))))

  # There is data
  expect_gt(nrow(result), 0)
})