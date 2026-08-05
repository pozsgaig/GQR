test_that("the complete workflow runs on the bundled dummy RDA example", {
  data <- gqr_example_data("dummy_data")

  expect_identical(names(data), c("Respondent", paste0("Q", 1:9)))

  result <- gqr_analysis(
    data,
    analysis_cols = paste0("Q", 1:9),
    id_col = "Respondent",
    dummy_mode = "all",
    n_components = 3,
    rotation = "varimax",
    respondent_regression = FALSE
  )

  expect_s3_class(result, "gqr_analysis")
  expect_equal(dim(result$D), c(512L, 9L))
  expect_equal(dim(result$W), c(512L, 10L))
  expect_equal(ncol(result$pca$scores), 3L)
  expect_false(is.null(result$statement_regression))
})
