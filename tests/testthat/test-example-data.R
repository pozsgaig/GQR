test_that("bundled examples load through the accessor", {
  dummy <- gqr_example_data("dummy_data")
  garden <- gqr_example_data("gardening")

  expect_s3_class(dummy, "data.frame")
  expect_equal(dim(dummy), c(10L, 12L))
  expect_true(all(c(
    "Respondent", paste0("Q", 1:9),
    "Numeric_covariate", "Factor_covariate"
  ) %in% names(dummy)))
  expect_true(is.numeric(dummy$Numeric_covariate))
  expect_true(is.factor(dummy$Factor_covariate))
  expect_identical(levels(dummy$Factor_covariate), c("A", "B"))

  expect_s3_class(garden, "data.frame")
  expect_equal(dim(garden), c(5524L, 25L))
  expect_true(all(c("ID", "Country_code", "NUTS") %in% names(garden)))
})

test_that("bundled examples load through data", {
  e <- new.env(parent = emptyenv())
  utils::data("dummy_data", package = "GQR", envir = e)
  utils::data("gardening", package = "GQR", envir = e)

  expect_s3_class(e$dummy_data, "data.frame")
  expect_s3_class(e$gardening, "data.frame")
  expect_equal(dim(e$dummy_data), c(10L, 12L))
  expect_true(is.numeric(e$dummy_data$Numeric_covariate))
  expect_true(is.factor(e$dummy_data$Factor_covariate))
  expect_equal(dim(e$gardening), c(5524L, 25L))
})

test_that("bundled example roles are shared by GUI and programmatic workflows", {
  dummy_roles <- gqr_example_roles("dummy_data")
  expect_identical(dummy_roles$analysis_cols, paste0("Q", 1:9))
  expect_identical(
    dummy_roles$covariate_cols,
    c("Numeric_covariate", "Factor_covariate")
  )
  expect_identical(dummy_roles$id_col, "Respondent")

  garden_roles <- gqr_example_roles("gardening")
  expect_identical(garden_roles$id_col, "ID")
  expect_false("ID" %in% garden_roles$analysis_cols)
  expect_false("ID" %in% garden_roles$covariate_cols)
  expect_true(length(garden_roles$analysis_cols) > 0L)
  expect_true(length(garden_roles$covariate_cols) > 0L)
})

test_that("datasets are not namespace exports", {
  exports <- getNamespaceExports("GQR")
  expect_false(any(c("dummy_data", "gardening") %in% exports))
})
