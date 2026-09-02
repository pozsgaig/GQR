# GQR Shiny application ----------------------------------------------

required_packages <- c(
  "shiny",
  "tidyr",
  "callr",
  "dplyr",
  "DT",
  "purrr",
  "tibble",
  "readr",
  "ggplot2",
  "ggnewscale",
  "Polychrome",
  "psych",
  "RColorBrewer",
  "scales"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0L) {
  stop(
    "Install the following packages before running the app: ",
    paste(missing_packages, collapse = ", "),
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
  outputTabUI("output")
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

  PCAregressionsTabServer(
    "pca_reg",
    data_state,
    pca_state,
    dummies_state
  )

  outputTabServer(
    "output",
    data_state,
    pca_state,
    dummies_state
  )
}

shiny::shinyApp(ui, server)
