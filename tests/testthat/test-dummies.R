test_that("full dummy design follows binary counting", {
  D <- gqr_generate_dummies(c("A", "B", "C"), mode = "all")

  expect_equal(dim(D), c(8L, 3L))
  expect_equal(D[1, ], c(A = 0L, B = 0L, C = 0L))
  expect_equal(D[8, ], c(A = 1L, B = 1L, C = 1L))
  expect_equal(colSums(D), c(A = 4L, B = 4L, C = 4L))
})

test_that("grouped design selects one variable per group", {
  groups <- data.frame(
    group = c("G1", "G1", "G2", "G2"),
    variable = c("A", "B", "C", "D")
  )

  D <- gqr_generate_dummies(
    c("A", "B", "C", "D"),
    mode = "group_one_per",
    groups = groups
  )

  expect_equal(nrow(D), 4L)
  expect_true(all(rowSums(D[, c("A", "B")]) == 1L))
  expect_true(all(rowSums(D[, c("C", "D")]) == 1L))
})

test_that("random designs are reproducible", {
  D1 <- gqr_generate_dummies(c("A", "B"), mode = "random", n_patterns = 20, seed = 4)
  D2 <- gqr_generate_dummies(c("A", "B"), mode = "random", n_patterns = 20, seed = 4)
  expect_identical(D1, D2)
})
