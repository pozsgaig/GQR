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
