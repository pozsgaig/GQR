test_that("package identity and public API are GQR", {
  expect_identical(as.character(utils::packageVersion("GQR")), "0.1.0")

  exports <- getNamespaceExports("GQR")
  expected <- c(
    "gqr_analysis", "gqr_estimate_design", "gqr_example_data",
    "gqr_filter_data", "gqr_generate_dummies", "gqr_make_w", "gqr_pca",
    "gqr_prepare_data", "gqr_read", "gqr_regress_respondents",
    "gqr_regress_statements", "gqr_transform_data", "run_gqr"
  )

  expect_setequal(exports, expected)
  expect_true(all(grepl("^(gqr_|run_gqr$)", exports)))
})
