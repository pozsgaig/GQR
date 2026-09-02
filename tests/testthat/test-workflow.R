test_that("the complete workflow runs on the bundled dummy RDA example", {
  data <- gqr_example_data("dummy_data")

  expect_identical(
    names(data),
    c(
      "Respondent", paste0("Q", 1:9),
      "Numeric_covariate", "Factor_covariate"
    )
  )

  result <- gqr_analysis(
    data,
    analysis_cols = paste0("Q", 1:9),
    id_col = "Respondent",
    covariate_cols = c("Numeric_covariate", "Factor_covariate"),
    dummy_mode = "all",
    n_components = 3,
    rotation = "varimax"
  )

  expect_s3_class(result, "gqr_analysis")
  expect_equal(dim(result$D), c(512L, 9L))
  expect_equal(dim(result$W), c(512L, 10L))
  expect_equal(ncol(result$pca$scores), 3L)
  expect_false(is.null(result$statement_regression))
  expect_false(is.null(result$respondent_regression))
  expect_identical(
    result$respondent_regression$covariates,
    c("Numeric_covariate", "Factor_covariate")
  )
})

test_that("workflow can use compact PCA without materialising W", {
  data <- gqr_example_data("dummy_data")

  result <- gqr_analysis(
    data,
    analysis_cols = paste0("Q", 1:9),
    id_col = "Respondent",
    dummy_mode = "all",
    n_components = 3,
    respondent_regression = FALSE,
    compact_threshold_mb = 0,
    w_memory_limit_mb = 0
  )

  expect_identical(result$pca_engine, "design")
  expect_false(result$W_materialised)
  expect_null(result$W)
  expect_equal(dim(result$pca$scores), c(512L, 3L))
})
