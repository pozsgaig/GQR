# Landing page ------------------------------------------------------------

homeTabUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tabPanel(
    "Home",
    gqr_css(),
    shiny::div(
      class = "q-container q-home",
      shiny::h2("GQR: Generalised Q Analysis in R"),
      shiny::p(
        class = "lead",
        "GQR provides a complete Generalised Q workflow through this interactive Shiny application and through ordinary R functions."
      ),
      shiny::div(
        class = "panel panel-primary",
        shiny::div(
          class = "panel-heading",
          shiny::h4("Methodological basis")
        ),
        shiny::div(
          class = "panel-body q-panel",
          shiny::p(
            "GQR implements the Generalised Q method introduced by Dentinho, Kourtit and Nijkamp (2023). The method recombines smaller groups of simple ranked or scored statements into a larger set of synthetic combined statements, constructs the W matrix, and applies principal component analysis to identify shared respondent structures."
          ),
          shiny::p(
            shiny::strong("What are dummies? "),
            "In GQR, a dummy is a binary indicator in the design matrix D. Each column represents one original simple statement and each row represents one synthetic combined statement. A value of 1 means that the statement is included in that combination; 0 means that it is not included. For example, the pattern (1, 0, 1) combines the first and third statements but excludes the second."
          ),
          shiny::p(
            "Statement-content and respondent-covariate regressions support component interpretation. The synthetic evaluation is additive, so researchers should consider whether the sum of constituent evaluations is appropriate for their application."
          ),
          shiny::p(
            shiny::strong("Reference: "),
            "Dentinho, T. P., Kourtit, K., & Nijkamp, P. (2023). Generalized Q analysis as a new tool in social science research: A pedagogical introduction. Eastern Journal of European Studies, 14(2), 5–21. ",
            shiny::tags$a(
              href = "https://doi.org/10.47743/ejes-2023-0201",
              target = "_blank",
              "https://doi.org/10.47743/ejes-2023-0201"
            )
          )
        )
      ),
      shiny::fluidRow(
        shiny::column(
          4,
          shiny::div(
            class = "q-home-card",
            shiny::h4("1. Prepare data"),
            shiny::p("Load a bundled RDA example or upload your data, assign column roles, edit labels, transform variables, and define thematic groups.")
          )
        ),
        shiny::column(
          4,
          shiny::div(
            class = "q-home-card",
            shiny::h4("2. Construct and analyse"),
            shiny::p("Generate full, grouped, or random dummy combinations; construct W; select the component count; and apply PCA and optional Varimax rotation.")
          )
        ),
        shiny::column(
          4,
          shiny::div(
            class = "q-home-card",
            shiny::h4("3. Interpret results"),
            shiny::p("Use design and W heatmaps, scree and loading displays, statement regressions, respondent regressions, and covariate graphics.")
          )
        )
      ),
      shiny::div(
        class = "panel panel-primary",
        shiny::div(
          class = "panel-heading",
          shiny::h4("Bundled gardening example")
        ),
        shiny::div(
          class = "panel-body q-panel",
          shiny::p(
            "The Gardening example contains a selection of columns from the multilingual Central and Eastern European gardening questionnaire dataset published by Varga-Szilay and colleagues in Scientific Data. The object is included only to demonstrate GQR and does not replace the complete published dataset or its documentation."
          ),
          shiny::p(
            shiny::strong("Reference: "),
            "Varga-Szilay, Z., Šerić Jelaska, L., Vilumets, S., Barševskis, A., Benedek, K., Bevk, D., Jojczyk, A., Krištín, A., Růžičková, J., Veromann, E., Fetykó, K. G., Szövényi, G., & Pozsgai, G. (2026). A multilingual, multi-country dataset on gardening and biodiversity awareness across Central and Eastern Europe. Scientific Data. ",
            shiny::tags$a(
              href = "https://doi.org/10.1038/s41597-026-07887-9",
              target = "_blank",
              "https://doi.org/10.1038/s41597-026-07887-9"
            )
          )
        )
      ),
      shiny::div(
        class = "q-home-start",
        shiny::p(
          shiny::strong("Start on the Data tab."),
          " The Gardening and Dummy data examples are loaded from bundled RDA files. The Dummy data example includes one numeric and one two-level factor covariate for demonstrating Component–Covariate Regression."
        )
      )
    )
  )
}

homeTabServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    invisible(NULL)
  })
}
