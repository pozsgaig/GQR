# GQR Shiny application ----------------------------------------------

# Runtime dependencies are declared in DESCRIPTION. Keep a direct diagnostic
# here as well so the bundled app gives the real namespace-loading error when
# launched manually rather than incorrectly reporting an installed package as
# absent.
required_packages <- c(
  "shiny", "tidyr", "callr", "dplyr", "DT", "purrr", "tibble",
  "ggplot2", "ggnewscale", "Polychrome", "psych",
  "RColorBrewer", "scales"
)

dependency_errors <- vapply(
  required_packages,
  function(package) {
    tryCatch(
      {
        loadNamespace(package)
        ""
      },
      error = function(e) conditionMessage(e)
    )
  },
  character(1)
)

failed_packages <- nzchar(dependency_errors)
if (any(failed_packages)) {
  stop(
    "One or more packages required by the GQR Shiny application could not be loaded:\n\n",
    paste0(
      required_packages[failed_packages], ": ", dependency_errors[failed_packages],
      collapse = "\n"
    ),
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  library(GQR)
  library(shiny)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(tibble)
  library(ggplot2)
})

source("R/data_functions.R", local = TRUE)
source("R/support_functions.R", local = TRUE)
source("R/ui_style.R", local = TRUE)
source("R/home_tab.R", local = TRUE)
source("R/data_module.R", local = TRUE)
source("R/dummies_tab.R", local = TRUE)
source("R/pca_tab.R", local = TRUE)
source("R/PCA_regression_tab.R", local = TRUE)
source("R/output_tab.R", local = TRUE)
source("R/reproducibility_tab.R", local = TRUE)

gardening <- GQR::gqr_example_data("gardening")
dummy_data <- GQR::gqr_example_data("dummy_data")

# The legacy interface uses `ID` internally for respondent metadata.
# Keep the packaged dataset unchanged, but adapt its identifier inside the app.
if (!"ID" %in% names(dummy_data) && "Respondent" %in% names(dummy_data)) {
  names(dummy_data)[names(dummy_data) == "Respondent"] <- "ID"
}

ui <- shiny::navbarPage(
  id = "main_tabs",
  title = "GQR Graphical User Interface",
  homeTabUI("home"),
  dataTabUI("data"),
  dummiesTabUI("dummies"),
  pcaTabUI("pca"),
  PCAregressionsTabUI("pca_reg"),
  outputTabUI("output"),
  reproducibilityTabUI("reproducibility")
)

server <- function(input, output, session) {
  homeTabServer("home")

  data_state <- dataTabServer(
    "data",
    built_in_data = list(
      "Gardening" = gardening,
      "Dummy data" = dummy_data
    )
  )

  dummies_state <- dummiesTabServer(
    "dummies",
    data_state,
    shiny::reactive(input$main_tabs)
  )

  pca_state <- pcaTabServer("pca", data_state, dummies_state)

  statement_state <- PCAregressionsTabServer(
    "pca_reg",
    data_state,
    pca_state,
    dummies_state,
    active_tab = shiny::reactive(input$main_tabs)
  )

  respondent_state <- outputTabServer(
    "output",
    data_state,
    pca_state,
    dummies_state
  )

  reproducibilityTabServer(
    "reproducibility",
    data_state = data_state,
    dummies_state = dummies_state,
    pca_state = pca_state,
    statement_state = statement_state,
    respondent_state = respondent_state
  )
}

shiny::shinyApp(ui, server)
