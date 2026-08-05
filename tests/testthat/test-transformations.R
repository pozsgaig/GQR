test_that("standardisation is column-wise in automatic mode", {
  data <- data.frame(A = c(1, 2, 3), B = c(10, 20, 30))
  result <- gqr_transform_data(data, c("A", "B"), "standardise", "auto")

  expect_equal(colMeans(result), c(A = 0, B = 0), tolerance = 1e-12)
  expect_equal(vapply(result, sd, numeric(1)), c(A = 1, B = 1), tolerance = 1e-12)
})

test_that("relative importance is row-wise in automatic mode", {
  data <- data.frame(A = c(1, 2), B = c(3, 2))
  result <- gqr_transform_data(data, c("A", "B"), "relative", "auto")

  expect_equal(rowSums(result), c(1, 1))
})

test_that("transformations preserve missing values", {
  data <- data.frame(A = c(1, NA, 3))
  result <- gqr_transform_data(data, "A", "standardise")
  expect_true(is.na(result$A[2]))
})

test_that("gqr_read supports RDA files", {
  example_data <- data.frame(ID = c("A", "B"), Q1 = c(1, 2))
  path <- tempfile(fileext = ".rda")
  save(example_data, file = path)

  result <- gqr_read(path)

  expect_s3_class(result, "data.frame")
  expect_equal(result, example_data)
})
