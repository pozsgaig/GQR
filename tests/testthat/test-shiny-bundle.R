shiny_app_path <- function() {
  # During devtools::test(), the app is in the source tree. During
  # R CMD check, files under inst/ are installed at the package root, so the
  # reliable path is system.file("shiny", package = "GQR").
  source_root <- normalizePath(
    file.path(testthat::test_path(), "..", ".."),
    winslash = "/",
    mustWork = FALSE
  )
  source_app <- file.path(source_root, "inst", "shiny")

  if (dir.exists(source_app)) {
    return(source_app)
  }

  installed_app <- system.file("shiny", package = "GQR")
  if (nzchar(installed_app) && dir.exists(installed_app)) {
    return(installed_app)
  }

  stop("The bundled Shiny application could not be located.", call. = FALSE)
}


test_that("the complete Shiny interface is bundled", {
  app <- shiny_app_path()

  expect_true(dir.exists(app))
  expect_true(file.exists(file.path(app, "app.R")))

  modules <- c(
    "home_tab.R",
    "data_module.R",
    "dummies_tab.R",
    "pca_tab.R",
    "PCA_regression_tab.R",
    "output_tab.R"
  )

  expect_true(all(file.exists(file.path(app, "R", modules))))
})

test_that("the Shiny landing page includes gardening provenance", {
  app <- shiny_app_path()
  home <- paste(
    readLines(file.path(app, "R", "home_tab.R"), warn = FALSE),
    collapse = "\n"
  )

  expect_match(home, "selection of columns", fixed = TRUE)
  expect_match(home, "10.1038/s41597-026-07887-9", fixed = TRUE)
  expect_match(home, "10.47743/ejes-2023-0201", fixed = TRUE)
})

test_that("the Shiny app handles absent covariates without a reactive error", {
  app <- shiny_app_path()
  output_module <- paste(
    readLines(file.path(app, "R", "output_tab.R"), warn = FALSE),
    collapse = "\n"
  )
  support_module <- paste(
    readLines(file.path(app, "R", "support_functions.R"), warn = FALSE),
    collapse = "\n"
  )

  expect_match(output_module, "No selected covariates", fixed = TRUE)
  expect_match(output_module, "respondent_ids <- rownames", fixed = TRUE)
  expect_false(grepl("across\\(all_of\\(dummy_vars\\), scale,", support_module))
})


test_that("large app calculations expose warnings, progress, and cancellation", {
  app <- shiny_app_path()
  data_module <- paste(readLines(file.path(app, "R", "data_module.R"), warn = FALSE), collapse = "\n")
  dummies_module <- paste(readLines(file.path(app, "R", "dummies_tab.R"), warn = FALSE), collapse = "\n")
  pca_module <- paste(readLines(file.path(app, "R", "pca_tab.R"), warn = FALSE), collapse = "\n")
  support_module <- paste(readLines(file.path(app, "R", "support_functions.R"), warn = FALSE), collapse = "\n")

  expect_match(data_module, "Proceed without variable groups?", fixed = TRUE)
  expect_match(dummies_module, "cancel_dummies", fixed = TRUE)
  expect_match(pca_module, "cancel_pca", fixed = TRUE)

  # The PCA module deliberately dispatches heavy work through the background
  # task helper rather than calling gqr_pca_design() directly. Verify both
  # sides of that dispatch: pca_tab.R requests the compact task and
  # support_functions.R maps it to the package-level exact PCA function.
  expect_match(pca_module, 'task = "pca_design"', fixed = TRUE)
  expect_match(support_module, 'identical(task, "pca_design")', fixed = TRUE)
  expect_match(support_module, "GQR::gqr_pca_design", fixed = TRUE)
})

test_that("PCA regression heatmap preserves grouped variables and reference zeros", {
  app <- shiny_app_path()
  regression_module <- paste(
    readLines(file.path(app, "R", "PCA_regression_tab.R"), warn = FALSE),
    collapse = "\n"
  )

  expect_match(regression_module, "heatmap_layout", fixed = TRUE)
  expect_match(regression_module, "zero_reference", fixed = TRUE)
  expect_match(regression_module, "geom_hline", fixed = TRUE)
  expect_match(regression_module, "Reference variables shown as 0", fixed = TRUE)
})

test_that("analytical tabs use clear names and collapsible explanations", {
  app <- shiny_app_path()
  pca_module <- paste(readLines(file.path(app, "R", "pca_tab.R"), warn = FALSE), collapse = "\n")
  regression_module <- paste(readLines(file.path(app, "R", "PCA_regression_tab.R"), warn = FALSE), collapse = "\n")
  output_module <- paste(readLines(file.path(app, "R", "output_tab.R"), warn = FALSE), collapse = "\n")

  expect_match(pca_module, "Principal Component Analysis", fixed = TRUE)
  expect_match(pca_module, "gqr_info_box", fixed = TRUE)
  expect_match(regression_module, "Statement–Component Regression", fixed = TRUE)
  expect_match(regression_module, "gqr_info_box", fixed = TRUE)
  expect_match(output_module, "Component–Covariate Regression", fixed = TRUE)
  expect_match(output_module, "gqr_info_box", fixed = TRUE)
})

test_that("group labels use a dedicated strip to the right of the regression heatmap", {
  app <- shiny_app_path()
  regression_module <- paste(
    readLines(file.path(app, "R", "PCA_regression_tab.R"), warn = FALSE),
    collapse = "\n"
  )

  expect_match(regression_module, "X = length(pcs) + 0.75", fixed = TRUE)
  expect_match(regression_module, "scale_x_continuous", fixed = TRUE)
  expect_match(regression_module, "geom_vline", fixed = TRUE)
  expect_match(regression_module, 'legend.position = "bottom"', fixed = TRUE)
})

test_that("built-in example roles and respondent regressions delegate to package functions", {
  app <- shiny_app_path()
  data_module <- paste(
    readLines(file.path(app, "R", "data_module.R"), warn = FALSE),
    collapse = "\n"
  )
  output_module <- paste(
    readLines(file.path(app, "R", "output_tab.R"), warn = FALSE),
    collapse = "\n"
  )
  pca_module <- paste(
    readLines(file.path(app, "R", "pca_tab.R"), warn = FALSE),
    collapse = "\n"
  )

  expect_match(data_module, "GQR::gqr_example_roles", fixed = TRUE)
  expect_match(output_module, "GQR::gqr_regress_respondents", fixed = TRUE)
  expect_false(grepl("stats::lm\\(", output_module))
  expect_match(pca_module, "result = pca_value", fixed = TRUE)
})
