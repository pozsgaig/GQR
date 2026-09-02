test_that("SPSS-style Varimax preserves the original GQR component convention", {
  dat <- gqr_example_data("dummy_data")
  vars <- paste0("Q", 1:9)

  groups <- data.frame(
    group = c(rep("G1", 4), rep("G2", 3), rep("G3", 2)),
    variable = vars,
    stringsAsFactors = FALSE
  )

  D <- gqr_generate_dummies(
    variables = vars,
    mode = "group_one_per",
    groups = groups
  )

  W <- gqr_make_w(
    data = dat,
    analysis_cols = vars,
    D = D,
    id_col = "Respondent"
  )

  expect_warning(
    pca <- gqr_pca(
      W,
      n_components = 5,
      rotation = "varimax",
      method = "correlation"
    ),
    "Matrix was not positive definite"
  )

  reg <- gqr_regress_statements(
    pca = pca,
    D = D,
    groups = groups,
    standardise = TRUE
  )

  get_beta <- function(variable) {
    out <- reg$coefficients[
      reg$coefficients$term == variable &
        reg$coefficients$component %in% paste0("PC", 1:5),
      c("component", "estimate"),
      drop = FALSE
    ]
    out <- out[match(paste0("PC", 1:5), out$component), , drop = FALSE]
    unname(out$estimate)
  }

  # These values reproduce the original psych::principal()-based Shiny
  # implementation (rounded values previously cross-checked against the
  # external SPSS-style calculation).  The tolerance allows only rounding-level
  # differences; component reordering or a different Varimax implementation
  # should fail this test.
  expect_equal(
    get_beta("Q2"),
    c(0.67, -0.43, -0.23, -0.14, 0.37),
    tolerance = 0.04
  )
  expect_equal(
    get_beta("Q3"),
    c(0.68, -0.02, 0.77, -0.49, 0.14),
    tolerance = 0.04
  )
})
