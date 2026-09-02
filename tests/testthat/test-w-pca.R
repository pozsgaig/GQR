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


test_that("compact design PCA matches PCA on materialised W", {
  data <- data.frame(
    ID = paste0("R", 1:6),
    A = c(1, 2, 3, 4, 5, 7),
    B = c(6, 2, 5, 1, 4, 3),
    C = c(2, 5, 1, 6, 3, 4),
    D = c(4, 1, 6, 2, 7, 5)
  )
  vars <- c("A", "B", "C", "D")
  D <- gqr_generate_dummies(vars, mode = "all")
  W <- gqr_make_w(data, vars, D, id_col = "ID")

  full <- gqr_pca(W, n_components = 3, rotation = "none")
  compact <- gqr_pca_design(
    data,
    D,
    analysis_cols = vars,
    id_col = "ID",
    n_components = 3,
    rotation = "none"
  )

  expect_equal(compact$eigenvalues, full$eigenvalues, tolerance = 1e-8)
  expect_equal(
    abs(compact$loadings),
    abs(full$loadings),
    tolerance = 1e-7
  )
  expect_equal(
    abs(compact$scores),
    abs(full$scores),
    tolerance = 1e-7
  )
})

test_that("W previews can materialise only selected design rows", {
  data <- data.frame(ID = c("a", "b"), A = c(1, 10), B = c(2, 20))
  D <- gqr_generate_dummies(c("A", "B"), mode = "all")
  W <- gqr_make_w(
    data,
    c("A", "B"),
    D,
    id_col = "ID",
    rows = 1:2
  )
  expect_equal(dim(W), c(2L, 2L))
  expect_equal(rownames(W), c("S1", "S2"))
})
