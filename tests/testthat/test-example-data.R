test_that("bundled examples load through the accessor", {
  dummy <- gqr_example_data("dummy_data")
  garden <- gqr_example_data("gardening")

  expect_s3_class(dummy, "data.frame")
  expect_equal(dim(dummy), c(10L, 10L))
  expect_true(all(c("Respondent", paste0("Q", 1:9)) %in% names(dummy)))

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
  expect_equal(dim(e$dummy_data), c(10L, 10L))
  expect_equal(dim(e$gardening), c(5524L, 25L))
})

test_that("datasets are not namespace exports", {
  exports <- getNamespaceExports("GQR")
  expect_false(any(c("dummy_data", "gardening") %in% exports))
})
