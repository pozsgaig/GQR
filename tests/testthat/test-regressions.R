test_that("grouped statement regressions omit one baseline per group", {
  data <- data.frame(
    ID = paste0("R", 1:5),
    A = c(1, 2, 3, 4, 5),
    B = c(2, 1, 4, 3, 5),
    C = c(5, 4, 3, 2, 1),
    D = c(1, 3, 2, 5, 4)
  )
  groups <- data.frame(
    group = c("G1", "G1", "G2", "G2"),
    variable = c("A", "B", "C", "D")
  )

  result <- gqr_analysis(
    data,
    analysis_cols = c("A", "B", "C", "D"),
    id_col = "ID",
    dummy_mode = "group_one_per",
    groups = groups,
    n_components = 2,
    respondent_regression = FALSE
  )

  expect_equal(result$statement_regression$baselines, c(G1 = "A", G2 = "C"))
  expect_equal(result$statement_regression$predictors, c("B", "D"))
})
