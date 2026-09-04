# Reproducible R script tab ------------------------------------------------

reproducibilityTabUI <- function(id) {
  ns <- shiny::NS(id)

  copy_js <- sprintf(
    paste0(
      "(function(){",
      "var el=document.getElementById('%s');",
      "if(!el){return;}",
      "var text=el.value;",
      "function fallbackCopy(){",
      "el.focus();el.select();el.setSelectionRange(0,el.value.length);",
      "try{document.execCommand('copy');}catch(e){}",
      "}",
      "if(navigator.clipboard && window.isSecureContext){",
      "navigator.clipboard.writeText(text).catch(function(){fallbackCopy();});",
      "}else{fallbackCopy();}",
      "})();"
    ),
    ns("script_text")
  )

  shiny::tabPanel(
    "Reproducible R script",
    gqr_css(),
    shiny::div(
      class = "q-container",
      shiny::h2("Reproducible R script"),
      shiny::p(
        "This tab converts the analysis completed in the graphical interface into executable R code. ",
        "The code uses the same exported GQR functions as the Shiny application and can be copied into RStudio or downloaded as an .R file."
      ),
      gqr_info_box(
        "What is recorded?",
        shiny::p(
          "The script records the dataset source, selected analysis variables and covariates, transformations, final grouping, dummy-matrix settings, the current respondent filters, PCA settings, regression settings, and the final plot controls used across the analysis tabs."
        ),
        shiny::p(
          "For uploaded files, GQR records the original file name, file size and MD5 checksum. Shiny's temporary upload path is never exported. The script therefore expects the original input file to be available in the R working directory, although the path can be edited after copying."
        ),
        shiny::p(
          "All uploaded datasets and grouping files from the session are listed in the script header. Files not used in the current frozen analysis are retained as provenance comments but are not read by the executable code."
        ),
        open = TRUE
      ),
      shiny::h4("Current analysis provenance"),
      shiny::verbatimTextOutput(ns("provenance_summary")),
      shiny::fluidRow(
        shiny::column(
          6,
          shiny::checkboxInput(
            ns("verify_files"),
            "Include MD5 checksum verification for uploaded data",
            value = FALSE
          )
        ),
        shiny::column(
          6,
          shiny::checkboxInput(
            ns("include_session_info"),
            "Append sessionInfo()",
            value = TRUE
          )
        )
      ),
      shiny::div(
        style = "margin-bottom: 10px;",
        shiny::tags$button(
          type = "button",
          class = "btn btn-default",
          onclick = copy_js,
          "Copy code"
        ),
        shiny::span(" "),
        shiny::downloadButton(ns("download_script"), "Download .R")
      ),
      shiny::textAreaInput(
        ns("script_text"),
        label = NULL,
        value = "",
        rows = 34,
        width = "100%",
        resize = "vertical"
      ),
      shiny::tags$style(
        shiny::HTML(sprintf(
          "#%s { font-family: Consolas, 'Courier New', monospace; font-size: 13px; white-space: pre; }",
          ns("script_text")
        ))
      )
    )
  )
}

reproducibilityTabServer <- function(
    id,
    data_state,
    dummies_state,
    pca_state,
    statement_state = NULL,
    respondent_state = NULL) {

  shiny::moduleServer(id, function(input, output, session) {

    analysis_state <- shiny::reactive({
      snapshot <- data_state$snapshot()
      if (is.null(snapshot)) return(NULL)

      dummy <- if (!is.null(dummies_state$dummy_settings)) {
        dummies_state$dummy_settings()
      } else {
        list(
          mode = if (is.null(snapshot$groups)) "all" else "group_one_per",
          include_empty = TRUE,
          allow_ungrouped = !is.null(snapshot$groups),
          max_patterns = 1000000L
        )
      }

      pca <- NULL
      if (!is.null(pca_state$result()) && !is.null(pca_state$settings)) {
        pca <- pca_state$settings()
      }

      statement <- NULL
      if (!is.null(statement_state) && !is.null(statement_state$settings)) {
        statement <- statement_state$settings()
      }

      respondent <- NULL
      if (!is.null(respondent_state) && !is.null(respondent_state$regression_settings)) {
        respondent <- respondent_state$regression_settings()
      }

      data_filter <- NULL
      if (!is.null(dummies_state$filter_spec)) {
        data_filter <- dummies_state$filter_spec()
      }

      plot_settings <- list(
        dummies = if (!is.null(dummies_state$plot_settings)) dummies_state$plot_settings() else NULL,
        statement = if (!is.null(statement_state) && !is.null(statement_state$plot_settings)) statement_state$plot_settings() else NULL,
        respondent = if (!is.null(respondent_state) && !is.null(respondent_state$plot_settings)) respondent_state$plot_settings() else NULL
      )

      list(
        package_version = as.character(utils::packageVersion("GQR")),
        generated_at = Sys.time(),
        data = snapshot$data_source,
        uploads = snapshot$upload_history %||% list(),
        analysis_cols = snapshot$analysis_cols,
        covariate_cols = snapshot$covariate_cols %||% character(0),
        factor_cols = snapshot$factor_cols %||% character(0),
        column_renames = snapshot$column_renames,
        transform = snapshot$transform_method %||% "none",
        groups = snapshot$groups,
        group_source = snapshot$group_source,
        dummy = dummy,
        data_filter = data_filter,
        pca = pca,
        statement_regression = statement,
        respondent_regression = respondent,
        plot_settings = plot_settings
      )
    })

    script_text <- shiny::reactive({
      state <- analysis_state()
      if (is.null(state)) {
        return(paste(
          "# Reproducible GQR analysis",
          "#",
          "# No frozen analysis is available yet.",
          "# Select a dataset and analysis settings on the Data tab, then click Next.",
          sep = "\n"
        ))
      }

      GQR::gqr_reproducible_script(
        state,
        include_session_info = isTRUE(input$include_session_info),
        verify_files = isTRUE(input$verify_files)
      )
    })

    shiny::observe({
      shiny::updateTextAreaInput(
        session,
        "script_text",
        value = script_text()
      )
    })

    output$provenance_summary <- shiny::renderText({
      state <- analysis_state()
      if (is.null(state)) {
        return("No frozen analysis is available. Complete the Data tab and click Next.")
      }

      source <- state$data
      source_label <- if (identical(source$type, "example")) {
        paste0("Bundled example: ", source$dataset)
      } else {
        paste0("Uploaded file: ", source$name)
      }

      n_groups <- if (is.null(state$groups)) 0L else length(unique(state$groups$group))
      pca_label <- if (is.null(state$pca)) {
        "not yet run"
      } else if (identical(state$pca$engine, "matrix")) {
        "SPSS-compatible correlation PCA (materialised W)"
      } else {
        "compact exact ordinary PCA"
      }

      statement_label <- if (!is.null(state$statement_regression) && isTRUE(state$statement_regression$enabled)) "included" else "not yet used"
      respondent_label <- if (!is.null(state$respondent_regression) && isTRUE(state$respondent_regression$enabled)) "included" else "not yet run"

      paste0(
        "Dataset: ", source_label, "\n",
        "Rows in source: ", source$n_rows %||% NA_integer_, "\n",
        "Analysis variables: ", length(state$analysis_cols), "\n",
        "Covariates: ", length(state$covariate_cols), "\n",
        "Transformation: ", state$transform, "\n",
        "Dummy mode: ", state$dummy$mode, "\n",
        "Synthetic combinations: ", state$dummy$patterns %||% NA_real_, "\n",
        "Groups: ", n_groups, "\n",
        "PCA: ", pca_label, "\n",
        "Statement-Component Regression: ", statement_label, "\n",
        "Component-Covariate Regression: ", respondent_label, "\n",
        "Uploaded input files recorded in session: ", length(state$uploads)
      )
    })

    output$download_script <- shiny::downloadHandler(
      filename = function() {
        paste0("GQR_reproducible_analysis_", format(Sys.Date(), "%Y%m%d"), ".R")
      },
      content = function(file) {
        writeLines(script_text(), con = file, useBytes = TRUE)
      }
    )

    list(
      state = analysis_state,
      script = script_text
    )
  })
}
