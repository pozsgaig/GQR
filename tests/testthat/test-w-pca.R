test_that("W is D multiplied by transposed respondent data", {
  data <- data.frame(
    ID = c("a", "b"),
    A = c(1, 10),
    B = c(2, 20)
  )
  D <- gqr_generate_dummies(c("A", "B"), mode = "all")

  W <- gqr_make_w(data, c("A", "B"), D, id_col = "ID")

  expect_equal(dim(W), c(4L, 2L))
  expect_equal(W["S1", ], c(a = 0, b = 0))
  expect_equal(W["S4", ], c(a = 3, b = 30))
})

test_that("PCA returns respondent loadings and combination scores", {
  data <- data.frame(
    ID = paste0("R", 1:4),
    A = c(1, 2, 3, 4),
    B = c(4, 1, 3, 2),
    C = c(2, 3, 1, 4)
  )
  D <- gqr_generate_dummies(c("A", "B", "C"), mode = "all")
  W <- gqr_make_w(data, c("A", "B", "C"), D, id_col = "ID")
  pca <- gqr_pca(W, n_components = 2, rotation = "varimax")

  expect_equal(dim(pca$loadings), c(4L, 2L))
  expect_equal(dim(pca$scores), c(8L, 2L))
  expect_equal(rownames(pca$loadings), data$ID)
  expect_equal(rownames(pca$scores), rownames(D))
})
