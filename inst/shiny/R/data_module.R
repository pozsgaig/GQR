# src/data_module.R

#========================
# UI
#========================
dataTabUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tabPanel(
    "Data",
    gqr_css(),
    shiny::div(
      class = "q-container",
      shiny::h2("Data preparation"),

      # 1. Data input
      shiny::div(
        class = "panel panel-primary",
        shiny::div(
          class = "panel-heading",
          shiny::tags$div(
            class = "q-panel-header",
            `data-toggle` = "collapse",
            `data-target` = paste0("#", ns("sec_data")),
            shiny::HTML(" 1. Data input (click to expand/collapse)")
          )
        ),
        shiny::div(
          id = ns("sec_data"),
          class = "panel-collapse collapse in",
          shiny::div(
            class = "panel-body q-panel",
            shiny::fluidRow(
              shiny::column(
                5,
                shiny::fileInput(
                  ns("file"),
                  "Choose data file",
                  accept = c(".csv", ".rds", ".rda", ".RData")
                ),
                shiny::checkboxInput(ns("use_example"), "Use built-in dataset", FALSE),
                shiny::conditionalPanel(
                  condition = sprintf("input['%s'] == true", ns("use_example")),
                  shiny::selectInput(
                    ns("example_dataset"),
                    "Built-in dataset:",
                    choices  = c("Gardening", "Dummy data"),
                    selected = "Gardening"
                  ),
                  shiny::uiOutput(ns("example_dataset_note"))
                )
              ),
              shiny::column(
                5,
                shiny::br(),
                shiny::actionButton(ns("open_data_view"), "Open data viewer")
              )
            ),
            shiny::uiOutput(ns("data_read_error"))
          )
        )
      ),

      # 2. Column names
      shiny::div(
        class = "panel panel-primary",
        shiny::div(
          class = "panel-heading",
          shiny::tags$div(
            class = "q-panel-header",
            `data-toggle` = "collapse",
            `data-target` = paste0("#", ns("sec_labels")),
            shiny::HTML(" 2. Column names (click to expand/collapse)")
          )
        ),
        shiny::div(
          id = ns("sec_labels"),
          class = "panel-collapse collapse in",
          shiny::div(
            class = "panel-body q-panel",
            shiny::p("Edit the column names used by GQR. Names are converted automatically to unique R-compatible names when saved."),
            shiny::uiOutput(ns("label_edit_ui")),
            shiny::actionButton(ns("save_labels"), "Save column names")
          )
        )
      ),

      # 3. Column roles & transformations
      shiny::div(
        class = "panel panel-primary",
        shiny::div(
          class = "panel-heading",
          shiny::tags$div(
            class = "q-panel-header",
            `data-toggle` = "collapse",
            `data-target` = paste0("#", ns("sec_roles")),
            shiny::HTML(" 3. Column roles and transformations (click to expand/collapse)")
          )
        ),
        shiny::div(
          id = ns("sec_roles"),
          class = "panel-collapse collapse in",
          shiny::div(
            class = "panel-body q-panel",
            shiny::fluidRow(
              shiny::column(
                6,
                shiny::uiOutput(ns("col_roles_ui"))
              ),
              shiny::column(
                6,
                shiny::h4("Transformation"),
                shiny::radioButtons(
                  ns("transform_method"),
                  "Standardisation method",
                  choices = c(
                    "No transformation"           = "none",
                    "Standardise (z-score)"       = "standardise",
                    "Normalise to [0,1]"          = "normalise",
                    "Relative importance (sum=1)" = "relative",
                    "Shannon entropy"             = "entropy"
                  ),
                  selected = "none"
                )
              )
            )
          )
        )
      ),

      # 4. Groups
      shiny::div(
        class = "panel panel-primary",
        shiny::div(
          class = "panel-heading",
          shiny::tags$div(
            class = "q-panel-header",
            `data-toggle` = "collapse",
            `data-target` = paste0("#", ns("sec_groups")),
            shiny::HTML(" 4. Groups of variables (click to expand/collapse)")
          )
        ),
        shiny::div(
          id = ns("sec_groups"),
          class = "panel-collapse collapse in",
          shiny::div(
            class = "panel-body q-panel",
            shiny::fluidRow(
              shiny::column(
                4,
                shiny::h4("Upload grouping"),
                shiny::fileInput(
                  ns("group_file"),
                  "Grouping vector (CSV, columns: group, variable)",
                  accept = ".csv"
                ),
                shiny::checkboxInput(
                  ns("use_random_groups"),
                  "Use random grouping (if no explicit groups)",
                  value = FALSE
                ),
                shiny::numericInput(
                  ns("n_random_groups"),
                  "Number of random groups",
                  value = 3, min = 1, step = 1
                ),
                shiny::hr(),
                shiny::h4("Create / edit groups"),
                shiny::textInput(ns("new_group_name"), "New group name", ""),
                shiny::actionButton(ns("add_group"), "Add group"),
                shiny::tags$script(HTML(sprintf("
$(document).on('keydown', '#%s', function(e) {
  if (e.key === 'Enter') {
    e.preventDefault();
    $('#%s').click();
  }
});
", ns("new_group_name"), ns("add_group"))))
              ),
              shiny::column(
                8,
                shiny::h4("Assign columns to groups"),
                shiny::uiOutput(ns("group_assign_ui"))
              )
            )
          )
        )
      ),

      shiny::br(),
      shiny::div(
        style = "text-align: right; margin-top: 10px;",
        shiny::actionButton(ns("next_to_dummies"), "Next \u2192"),

        shiny::br()
      )
    )
  )
}

#========================
# SERVER
#========================
dataTabServer <- function(id, built_in_data = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    group_mapping <- shiny::reactiveVal(
      tibble::tibble(group = character(), variable = character())
    )

    data_snapshot <- shiny::reactiveVal(NULL)
    raw_data <- shiny::reactiveVal(NULL)
    imported_names <- shiny::reactiveVal(character())
    labels <- shiny::reactiveVal(NULL)
    data_read_error <- shiny::reactiveVal(NULL)
    source_type <- shiny::reactiveVal(NULL)
    role_analysis <- shiny::reactiveVal(NULL)
    role_covariates <- shiny::reactiveVal(NULL)
    factor_columns <- shiny::reactiveVal(character())

    # Keep provenance for every external input uploaded during the session.
    # Shiny's temporary `datapath` is never exported; only the original name,
    # checksum and basic file metadata are retained.
    upload_history <- shiny::reactiveVal(list())
    last_group_upload <- shiny::reactiveVal(NULL)

    append_upload_provenance <- function(provenance) {
      current <- upload_history()
      upload_history(append(current, list(provenance)))
      invisible(provenance)
    }

    show_data_error <- function(message) {
      data_read_error(as.character(message))
      shiny::showNotification(
        paste0("Data input error: ", message),
        type = "error",
        duration = NULL
      )
      invisible(NULL)
    }

    clear_working_data <- function() {
      raw_data(NULL)
      imported_names(character())
      labels(NULL)
      role_analysis(NULL)
      role_covariates(NULL)
      factor_columns(character())
      group_mapping(tibble::tibble(group = character(), variable = character()))
      data_snapshot(NULL)
      source_type(NULL)
      invisible(NULL)
    }

    initialise_working_data <- function(df, type) {
      df <- check_loaded_data(df)
      raw_data(df)
      imported_names(names(df))
      labels(tibble::tibble(name = names(df), label = names(df)))
      role_analysis(NULL)
      role_covariates(NULL)
      factor_columns(names(df)[vapply(df, is.factor, logical(1))])
      group_mapping(tibble::tibble(group = character(), variable = character()))
      data_snapshot(NULL)
      source_type(type)
      data_read_error(NULL)
      invisible(df)
    }

    shiny::observeEvent(input$file, {
      shiny::req(input$file)
      tryCatch(
        {
          provenance <- GQR::gqr_file_provenance(
            path = input$file$datapath,
            original_name = input$file$name,
            role = "dataset"
          )
          append_upload_provenance(provenance)
        },
        error = function(e) {
          shiny::showNotification(
            paste0("Could not record file provenance: ", conditionMessage(e)),
            type = "warning"
          )
        }
      )
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$group_file, {
      shiny::req(input$group_file)
      tryCatch(
        {
          provenance <- GQR::gqr_file_provenance(
            path = input$group_file$datapath,
            original_name = input$group_file$name,
            role = "groups"
          )
          last_group_upload(provenance)
          append_upload_provenance(provenance)
        },
        error = function(e) {
          shiny::showNotification(
            paste0("Could not record grouping-file provenance: ", conditionMessage(e)),
            type = "warning"
          )
        }
      )
    }, ignoreInit = TRUE)

    check_loaded_data <- function(df) {
      if (!is.data.frame(df)) {
        stop("The selected object is not a data frame.", call. = FALSE)
      }

      if (nrow(df) < 1L || ncol(df) < 1L) {
        stop("The selected dataset contains no rows or columns.", call. = FALSE)
      }

      if (ncol(df) == 1L && grepl("[,;\\t]", names(df)[1L])) {
        stop(
          "The data were read as one column. Check the file delimiter or encoding.",
          call. = FALSE
        )
      }

      df
    }

    # Data import is deliberately isolated from the rest of the reactive graph.
    # A malformed/unsupported file clears the working data and displays an
    # in-app message instead of allowing the read error to cascade through the
    # remaining tabs.
    shiny::observeEvent(
      list(input$file, input$use_example, input$example_dataset),
      {
        tryCatch(
          {
            if (isTRUE(input$use_example)) {
              ds <- input$example_dataset
              shiny::req(built_in_data, ds)
              initialise_working_data(built_in_data[[ds]], "example")
            } else if (!is.null(input$file)) {
              df <- gqr_read_upload(input$file$datapath, input$file$name)
              initialise_working_data(df, "file")
            } else {
              clear_working_data()
              data_read_error(NULL)
            }
          },
          error = function(e) {
            clear_working_data()
            show_data_error(conditionMessage(e))
          }
        )
      },
      ignoreInit = FALSE
    )

    output$data_read_error <- shiny::renderUI({
      msg <- data_read_error()
      if (is.null(msg) || !nzchar(msg)) return(NULL)

      shiny::div(
        class = "alert alert-danger",
        shiny::strong("The dataset could not be loaded. "),
        msg,
        shiny::br(),
        "The application is still running; correct or re-export the file and upload it again."
      )
    })

    output$example_dataset_note <- shiny::renderUI({
      if (!isTRUE(input$use_example)) {
        return(NULL)
      }

      if (identical(input$example_dataset, "Dummy data")) {
        return(
          shiny::div(
            class = "help-block",
            "This synthetic example contains nine Q statement variables plus Numeric_covariate and Factor_covariate (levels A/B). The two covariates are selected automatically on the Data tab."
          )
        )
      }

      if (identical(input$example_dataset, "Gardening")) {
        return(
          shiny::div(
            class = "help-block",
            "This example is a selected-column extract from the gardening dataset published in Scientific Data (2026) and analysed in Urban Forestry & Urban Greening (2024, 2025). Full references are provided on the Home tab and in ?gardening."
          )
        )
      }

      NULL
    })

    # ---- column names ----
    output$label_edit_ui <- shiny::renderUI({
      shiny::req(labels())
      labs <- labels()
      marked_factors <- shiny::isolate(factor_columns())

      n <- nrow(labs)
      per_row <- 2
      rows <- split(seq_len(n), ceiling(seq_len(n) / per_row))

      shiny::tagList(
        lapply(rows, function(idx_vec) {
          shiny::fluidRow(
            lapply(idx_vec, function(i) {
              shiny::column(
                width = 12 / per_row,
                shiny::div(
                  style = "margin-bottom:12px;",
                  shiny::div(
                    style = paste(
                      "display:flex; align-items:center; justify-content:space-between;",
                      "gap:10px; min-height:28px; margin-bottom:4px;"
                    ),
                    shiny::tags$strong(
                      title = labs$name[i],
                      style = paste(
                        "flex:1 1 auto; min-width:0; white-space:nowrap;",
                        "overflow:hidden; text-overflow:ellipsis;"
                      ),
                      labs$name[i]
                    ),
                    shiny::tags$label(
                      style = paste(
                        "flex:0 0 auto; display:inline-flex; align-items:center;",
                        "gap:5px; margin:0; font-weight:normal; line-height:1;"
                      ),
                      shiny::tags$input(
                        id = ns(paste0("factor_", i)),
                        type = "checkbox",
                        style = "margin:0; position:static; vertical-align:middle;",
                        checked = if (labs$name[i] %in% marked_factors) "checked" else NULL
                      ),
                      shiny::tags$span("Factor")
                    )
                  ),
                  shiny::textInput(
                    ns(paste0("lab_", i)),
                    label = "Rename to",
                    value = labs$label[i],
                    width = "100%"
                  )
                )
              )
            })
          )
        })
      )
    })

    factor_selections <- shiny::reactive({
      labs <- labels()
      shiny::req(labs)

      values <- lapply(
        seq_len(nrow(labs)),
        function(i) input[[paste0("factor_", i)]]
      )
      if (any(vapply(values, is.null, logical(1)))) return(NULL)

      selected <- vapply(values, isTRUE, logical(1))
      labs$name[selected]
    })

    shiny::observeEvent(factor_selections(), {
      if (is.null(raw_data())) return(NULL)

      current_names <- names(raw_data())
      new_factors <- intersect(as.character(factor_selections()), current_names)
      old_factors <- intersect(factor_columns(), current_names)
      added <- setdiff(new_factors, old_factors)

      factor_columns(new_factors)

      # Factor-marked variables cannot be used as numeric Q-analysis columns.
      # When a numeric-coded category is newly marked as a factor, move it out
      # of the analysis role and make it available as a covariate immediately.
      if (length(added) > 0L) {
        analysis_now <- role_analysis()
        if (!is.null(analysis_now)) {
          role_analysis(setdiff(analysis_now, added))
        }

        covariates_now <- role_covariates()
        if (is.null(covariates_now)) covariates_now <- character()
        role_covariates(unique(c(covariates_now, added)))
      }
    }, ignoreInit = FALSE)

    shiny::observeEvent(input$save_labels, {
      df <- raw_data()
      labs <- labels()
      shiny::req(df, labs)

      tryCatch(
        {
          old_names <- names(df)
          requested <- vapply(
            seq_len(nrow(labs)),
            function(i) {
              value <- input[[paste0("lab_", i)]]
              if (is.null(value)) labs$label[i] else as.character(value)
            },
            character(1)
          )

          base_names <- gqr_compatible_names(requested, unique = FALSE)

          # `ID` is the respondent identifier used by the current graphical
          # workflow. Keep that reserved column stable even when the other
          # dataframe columns are renamed.
          id_position <- match("ID", old_names)
          if (!is.na(id_position)) {
            base_names[id_position] <- "ID"
            order_for_unique <- c(id_position, setdiff(seq_along(base_names), id_position))
            unique_ordered <- make.unique(base_names[order_for_unique], sep = ".")
            new_names <- character(length(base_names))
            new_names[order_for_unique] <- unique_ordered
          } else {
            new_names <- make.unique(base_names, sep = ".")
          }

          if (length(new_names) != length(old_names)) {
            stop("The number of column names changed unexpectedly.", call. = FALSE)
          }

          name_map <- stats::setNames(new_names, old_names)
          map_values <- function(x) {
            if (is.null(x)) return(NULL)
            x <- as.character(x)
            mapped <- unname(name_map[x])
            missing <- is.na(mapped)
            mapped[missing] <- x[missing]
            mapped
          }

          # Remap all internal state before changing `raw_data()`. Updating
          # `raw_data()` rebuilds the role selectors; without freezing those
          # inputs, Shiny can briefly send their old values back to the server
          # and overwrite the correctly remapped covariate/analysis state.
          mapped_analysis <- map_values(role_analysis())
          mapped_covariates <- map_values(role_covariates())
          mapped_factors <- map_values(factor_columns())

          gm <- group_mapping()
          if (nrow(gm) > 0L && "variable" %in% names(gm)) {
            assigned <- !is.na(gm$variable)
            gm$variable[assigned] <- map_values(gm$variable[assigned])
          }

          shiny::freezeReactiveValue(input, "analysis_cols")
          shiny::freezeReactiveValue(input, "covariate_cols")

          names(df) <- new_names
          role_analysis(mapped_analysis)
          role_covariates(mapped_covariates)
          factor_columns(mapped_factors)
          group_mapping(gm)
          labels(tibble::tibble(name = new_names, label = new_names))
          raw_data(df)

          changed_by_cleanup <- requested != new_names
          if (!is.na(id_position) && requested[id_position] != "ID") {
            shiny::showNotification(
              "The reserved respondent identifier column remains named ID.",
              type = "warning",
              duration = 8
            )
          }
          if (any(changed_by_cleanup)) {
            examples <- paste0(
              requested[changed_by_cleanup], " -> ", new_names[changed_by_cleanup]
            )
            shiny::showNotification(
              paste0(
                "Column names were saved as R-compatible names. ",
                paste(utils::head(examples, 4L), collapse = "; "),
                if (length(examples) > 4L) "; ..." else ""
              ),
              type = "message",
              duration = 8
            )
          } else {
            shiny::showNotification("Column names saved.", type = "message")
          }
        },
        error = function(e) {
          shiny::showNotification(
            paste0("Column names were not changed: ", conditionMessage(e)),
            type = "error",
            duration = NULL
          )
        }
      )
    }, ignoreInit = TRUE)

    # ---- column roles ----
    output$col_roles_ui <- shiny::renderUI({
      shiny::req(raw_data())
      df <- raw_data()
      cols <- names(df)
      factor_cols <- intersect(factor_columns(), cols)
      numeric_cols <- cols[vapply(df, is.numeric, logical(1))]
      numeric_cols <- setdiff(numeric_cols, factor_cols)

      default_analysis <- numeric_cols
      default_covariates <- setdiff(cols, numeric_cols)

      # Keep the built-in examples synchronised with the package-level role
      # definitions used by non-GUI analyses. Uploaded datasets retain the
      # generic numeric/non-numeric defaults.
      if (identical(source_type(), "example") && !is.null(input$example_dataset)) {
        dataset_key <- switch(
          input$example_dataset,
          "Dummy data" = "dummy_data",
          "Gardening" = "gardening",
          NULL
        )

        if (!is.null(dataset_key)) {
          roles <- GQR::gqr_example_roles(dataset_key)
          default_analysis <- intersect(roles$analysis_cols, cols)
          default_covariates <- intersect(roles$covariate_cols, cols)
        }
      }

      selected_analysis <- role_analysis()
      selected_covariates <- role_covariates()
      if (is.null(selected_analysis)) selected_analysis <- default_analysis
      if (is.null(selected_covariates)) selected_covariates <- default_covariates

      selected_analysis <- intersect(setdiff(selected_analysis, factor_cols), cols)
      selected_covariates <- intersect(setdiff(selected_covariates, selected_analysis), cols)

      # A column already assigned to one role is deliberately omitted from the
      # other selector. To change a role, deselect it first; it then becomes
      # available in the other list. Factor-marked columns are not offered as
      # analysis variables because the GQR evaluation matrix must be numeric.
      analysis_choices <- setdiff(setdiff(cols, selected_covariates), factor_cols)
      covariate_choices <- setdiff(cols, selected_analysis)

      shiny::tagList(
        shiny::h4("Assign roles"),
        shiny::selectInput(
          ns("analysis_cols"),
          "Columns for analysis",
          choices = analysis_choices,
          selected = intersect(selected_analysis, analysis_choices),
          multiple = TRUE
        ),
        shiny::selectInput(
          ns("covariate_cols"),
          "Covariate columns",
          choices = covariate_choices,
          selected = intersect(selected_covariates, covariate_choices),
          multiple = TRUE
        )
      )
    })

    shiny::observeEvent(input$analysis_cols, {
      if (is.null(input$analysis_cols) || is.null(raw_data())) return(NULL)

      value <- as.character(input$analysis_cols)
      current_names <- names(raw_data())
      value <- setdiff(value, factor_columns())
      value <- setdiff(value, if (is.null(role_covariates())) character() else role_covariates())

      # A selectInput can momentarily report its pre-rename values while the
      # UI is being rebuilt. Such values cannot have been selected by the user
      # from the current control, so ignore the stale event rather than wiping
      # the already-remapped role state.
      if (length(value) > 0L && any(!value %in% current_names)) return(NULL)

      role_analysis(value)
    }, ignoreInit = FALSE)

    shiny::observeEvent(input$covariate_cols, {
      if (is.null(input$covariate_cols) || is.null(raw_data())) return(NULL)

      value <- as.character(input$covariate_cols)
      current_names <- names(raw_data())
      analysis_now <- role_analysis()
      if (is.null(analysis_now)) analysis_now <- character()
      value <- setdiff(value, analysis_now)

      if (length(value) > 0L && any(!value %in% current_names)) return(NULL)

      role_covariates(value)
    }, ignoreInit = FALSE)

    # ---- transformation (local to Data tab; frozen only on Next) ----
    typed_data <- shiny::reactive({
      df <- raw_data()
      shiny::req(df)

      factor_cols <- intersect(factor_columns(), names(df))
      for (v in factor_cols) {
        if (!is.factor(df[[v]])) {
          df[[v]] <- factor(df[[v]])
        }
      }
      df
    })

    transformed_data <- shiny::reactive({
      shiny::req(typed_data(), role_analysis())

      df_raw <- typed_data()
      cols <- setdiff(role_analysis(), factor_columns())

      shiny::req(all(cols %in% colnames(df_raw)))

      method <- input$transform_method
      allowed <- c("standardise", "normalise", "relative", "entropy")

      if (length(method) != 1L || is.na(method) || method == "" || method == "none") {
        return(df_raw)
      }

      shiny::validate(
        shiny::need(method %in% allowed, "Unknown transformation method.")
      )

      gqr_transform_columns(
        df_raw,
        cols = cols,
        method = method
      )
    })

    data_current <- shiny::reactive({
      df_tr <- transformed_data()
      if (is.null(df_tr)) typed_data() else df_tr
    })

    # ---- groups ----

    shiny::observeEvent(input$group_file, {
      shiny::req(input$group_file)

      tryCatch(
        {
          gf <- gqr_read_upload(input$group_file$datapath, input$group_file$name)
          if (!all(c("group", "variable") %in% names(gf))) {
            stop(
              "The grouping file must contain columns named `group` and `variable`.",
              call. = FALSE
            )
          }

          gf$group <- as.character(gf$group)
          gf$variable <- gqr_compatible_names(gf$variable, unique = FALSE)

          # If the data columns have subsequently been renamed in the GUI, map
          # grouping-file references from the imported names to the current names.
          imported <- imported_names()
          current <- if (is.null(raw_data())) character() else names(raw_data())
          if (length(imported) == length(current) && length(imported) > 0L) {
            import_map <- stats::setNames(current, imported)
            mapped <- unname(import_map[gf$variable])
            use_mapped <- !is.na(mapped)
            gf$variable[use_mapped] <- mapped[use_mapped]
          }

          unknown <- setdiff(unique(gf$variable), names(raw_data()))
          if (length(unknown) > 0L) {
            stop(
              "Grouping file contains variables not present in the current dataset: ",
              paste(unknown, collapse = ", "),
              call. = FALSE
            )
          }

          group_mapping(
            gf |>
              dplyr::distinct(.data$group, .data$variable)
          )
        },
        error = function(e) {
          shiny::showNotification(
            paste0("Grouping file was not loaded: ", conditionMessage(e)),
            type = "error",
            duration = NULL
          )
        }
      )
    }, ignoreInit = TRUE)

    group_control_id <- function(prefix, group_name) {
      bytes <- as.integer(charToRaw(enc2utf8(as.character(group_name))))
      suffix <- paste(sprintf("%02x", bytes), collapse = "")
      paste0(prefix, "_", suffix)
    }

    add_group_fn <- function() {
      gname <- trimws(input$new_group_name)
      if (!nzchar(gname)) return(NULL)

      gm <- group_mapping()
      if (gname %in% gm$group) return(NULL)

      gm <- dplyr::bind_rows(
        gm,
        tibble::tibble(group = gname, variable = NA_character_)
      )
      group_mapping(gm)
      shiny::updateTextInput(session, "new_group_name", value = "")
    }

    shiny::observeEvent(input$add_group, {
      add_group_fn()
    }, ignoreInit = TRUE)

    shiny::observe({
      gm <- group_mapping()
      gnames <- unique(gm$group)

      lapply(gnames, function(g) {
        btn_id <- group_control_id("delete_group", g)
        shiny::observeEvent(input[[btn_id]], {
          gm_now <- group_mapping() |>
            dplyr::filter(.data$group != g)

          if (nrow(gm_now) == 0) {
            gm_now <- tibble::tibble(group = character(), variable = character())
          }

          group_mapping(gm_now)
        }, ignoreInit = TRUE)
      })
    })

    shiny::observeEvent(input$clear_grouping, {
      group_mapping(tibble::tibble(group = character(), variable = character()))
    }, ignoreInit = TRUE)

    output$group_assign_ui <- shiny::renderUI({
      shiny::req(raw_data())
      gm <- group_mapping()

      if (nrow(gm) == 0 || !all(c("group", "variable") %in% names(gm))) {
        return(shiny::p("Create a group first, then you can assign columns."))
      }

      gnames <- unique(gm$group)
      if (length(gnames) == 0) {
        return(shiny::p("Create a group first, then you can assign columns."))
      }

      analysis_cols <- safe_analysis_cols()
      shiny::req(analysis_cols)

      controls <- lapply(gnames, function(g) {
        current_vars <- gm |>
          dplyr::filter(.data$group == g, !is.na(.data$variable)) |>
          dplyr::pull(.data$variable)

        used_elsewhere <- gm |>
          dplyr::filter(.data$group != g, !is.na(.data$variable)) |>
          dplyr::pull(.data$variable) |>
          unique()

        choices_g <- sort(unique(c(current_vars, setdiff(analysis_cols, used_elsewhere))))

        shiny::fluidRow(
          shiny::column(
            10,
            shiny::selectInput(
              ns(group_control_id("group_vars", g)),
              label = paste("Columns in group:", g),
              choices = choices_g,
              selected = intersect(current_vars, choices_g),
              multiple = TRUE,
              selectize = TRUE
            )
          ),
          shiny::column(
            2,
            style = "margin-top: 25px; text-align: right;",
            shiny::actionButton(
              ns(group_control_id("delete_group", g)),
              "Delete",
              class = "btn btn-danger btn-sm"
            )
          )
        )
      })

      shiny::tagList(
        controls,
        shiny::hr(),
        shiny::actionButton(
          ns("clear_grouping"),
          "Clear grouping",
          class = "btn btn-danger"
        )
      )
    })

    group_selections <- shiny::reactive({
      gm <- group_mapping()
      gnames <- unique(gm$group)
      if (length(gnames) == 0) return(NULL)

      vals <- purrr::set_names(
        lapply(gnames, function(g) input[[group_control_id("group_vars", g)]]),
        gnames
      )

      if (all(vapply(vals, is.null, logical(1)))) {
        return(NULL)
      }

      vals
    })

    shiny::observeEvent(group_selections(), {
      if (isTRUE(input$use_random_groups)) return(NULL)
      shiny::req(raw_data())

      sel_list <- group_selections()
      shiny::req(sel_list)

      new_map <- purrr::map_dfr(names(sel_list), function(g) {
        sel <- sel_list[[g]]
        if (is.null(sel) || length(sel) == 0) {
          tibble::tibble(group = g, variable = NA_character_)
        } else {
          tibble::tibble(group = g, variable = sel)
        }
      })

      group_mapping(
        dplyr::distinct(new_map, .data$group, .data$variable)
      )
    }, ignoreInit = TRUE)

    safe_analysis_cols <- shiny::reactive({
      shiny::req(raw_data(), role_analysis())
      setdiff(
        intersect(role_analysis(), colnames(raw_data())),
        factor_columns()
      )
    })

    safe_covariate_cols <- shiny::reactive({
      shiny::req(raw_data())
      covs <- role_covariates()
      if (is.null(covs)) return(character())
      intersect(covs, colnames(raw_data()))
    })

    shiny::observeEvent(
      list(input$use_random_groups, input$n_random_groups, role_analysis()),
      {
        shiny::req(safe_analysis_cols())
        if (!isTRUE(input$use_random_groups)) return(NULL)

        vars <- safe_analysis_cols()
        k <- min(as.integer(input$n_random_groups), length(vars))
        shiny::req(!is.na(k), k >= 1)

        gnames <- paste("Group", seq_len(k))
        shuffled_vars <- sample(vars, length(vars), replace = FALSE)
        group_assignments <- rep(gnames, length.out = length(shuffled_vars))

        gm <- tibble::tibble(
          group = group_assignments,
          variable = shuffled_vars
        ) |>
          dplyr::arrange(.data$group, .data$variable)

        empty_groups <- setdiff(gnames, gm$group)
        if (length(empty_groups) > 0) {
          gm <- dplyr::bind_rows(
            gm,
            tibble::tibble(group = empty_groups, variable = NA_character_)
          )
        }

        group_mapping(dplyr::distinct(gm, .data$group, .data$variable))
      },
      ignoreInit = TRUE
    )

    current_valid_groups <- shiny::reactive({
      shiny::req(raw_data())
      gm <- group_mapping()
      vars <- safe_analysis_cols()

      if (nrow(gm) == 0 || !all(c("group", "variable") %in% names(gm))) {
        return(NULL)
      }

      gm_assigned <- gm |>
        dplyr::distinct(.data$group, .data$variable) |>
        dplyr::filter(!is.na(.data$variable), .data$variable %in% vars)

      if (nrow(gm_assigned) > 0) gm_assigned else NULL
    })

    # ---- data viewer ----
    shiny::observeEvent(input$open_data_view, {
      shiny::req(raw_data())
      shiny::showModal(
        shiny::modalDialog(
          title = "Data viewer",
          size = "l",
          easyClose = TRUE,
          footer = NULL,
          DT::DTOutput(ns("data_table"))
        )
      )
    })

    output$data_table <- DT::renderDT({
      shiny::req(raw_data())
      df <- data_current()
      DT::datatable(
        df,
        options = list(pageLength = 10, scrollX = TRUE),
        filter = "top"
      )
    })

    # ---- freeze snapshot on Next ----
    freeze_snapshot_and_move <- function() {
      df_trans <- data_current()
      vars <- safe_analysis_cols()
      covs <- safe_covariate_cols()
      groups_cur <- current_valid_groups()
      labs <- labels()

      shiny::req(df_trans, vars)

      raw <- typed_data()

      data_source <- if (identical(source_type(), "file")) {
        shiny::req(input$file)
        provenance <- GQR::gqr_file_provenance(
          path = input$file$datapath,
          original_name = input$file$name,
          role = "dataset",
          data = raw
        )
        list(
          type = "file",
          name = provenance$original_name,
          md5 = provenance$md5,
          size_bytes = provenance$size_bytes,
          n_rows = provenance$n_rows,
          n_cols = provenance$n_cols
        )
      } else {
        dataset_key <- switch(
          input$example_dataset,
          "Dummy data" = "dummy_data",
          "Gardening" = "gardening",
          NULL
        )
        source <- list(
          type = "example",
          dataset = dataset_key,
          n_rows = nrow(raw),
          n_cols = ncol(raw)
        )
        if (identical(dataset_key, "dummy_data")) {
          source$rename_id_from <- "Respondent"
          source$rename_id_to <- "ID"
        }
        source
      }

      group_source <- NULL
      if (!isTRUE(input$use_random_groups) &&
          !is.null(groups_cur) &&
          !is.null(last_group_upload())) {
        group_source <- last_group_upload()
      }

      history <- upload_history()
      if (length(history) > 0L) {
        for (i in seq_along(history)) {
          history[[i]]$status <- "uploaded, not used in current analysis"

          if (identical(history[[i]]$role, "dataset") &&
              identical(data_source$type, "file") &&
              identical(history[[i]]$original_name, data_source$name) &&
              identical(history[[i]]$md5, data_source$md5)) {
            history[[i]]$status <- "used in current analysis"
            history[[i]]$n_rows <- data_source$n_rows
            history[[i]]$n_cols <- data_source$n_cols
          }

          if (!is.null(group_source) &&
              identical(history[[i]]$role, "groups") &&
              identical(history[[i]]$original_name, group_source$original_name) &&
              identical(history[[i]]$md5, group_source$md5)) {
            history[[i]]$status <- "grouping source; final GUI grouping recorded explicitly"
          }
        }
      }

      imported <- imported_names()
      current <- names(raw)
      column_renames <- NULL
      if (length(imported) == length(current) && length(imported) > 0L) {
        changed <- imported != current
        if (any(changed)) {
          column_renames <- data.frame(
            from = imported[changed],
            to = current[changed],
            stringsAsFactors = FALSE
          )
        }
      }

      data_snapshot(
        list(
          data_raw = raw,
          data_trans = df_trans,
          analysis_cols = vars,
          covariate_cols = covs,
          factor_cols = intersect(factor_columns(), names(raw)),
          labels = labs,
          column_renames = column_renames,
          groups = groups_cur,
          transform_method = input$transform_method,
          data_source = data_source,
          group_source = group_source,
          upload_history = history
        )
      )

      shiny::updateNavbarPage(
        session = session$rootScope(),
        inputId = "main_tabs",
        selected = "Dummies"
      )
    }

    shiny::observeEvent(input$next_to_dummies, {
      df_trans <- data_current()
      vars <- safe_analysis_cols()
      groups_cur <- current_valid_groups()
      shiny::req(df_trans, vars)

      # Estimation is deliberately performed before D or W is allocated.
      design <- if (is.null(groups_cur)) {
        GQR::gqr_estimate_design(
          variables = vars,
          mode = "all",
          n_respondents = nrow(df_trans)
        )
      } else {
        GQR::gqr_estimate_design(
          variables = vars,
          mode = "group_one_per",
          groups = groups_cur,
          n_respondents = nrow(df_trans),
          allow_ungrouped = TRUE
        )
      }

      warn_ungrouped <- is.null(groups_cur) && (
        is.finite(design$patterns) && design$patterns >= 4096 ||
          is.finite(design$w_memory_mb) && design$w_memory_mb >= 128
      )

      if (isTRUE(warn_ungrouped)) {
        shiny::showModal(
          shiny::modalDialog(
            title = "Proceed without variable groups?",
            easyClose = FALSE,
            footer = shiny::tagList(
              shiny::modalButton("Stay on Data tab"),
              shiny::actionButton(
                ns("confirm_ungrouped"),
                "Proceed without groups",
                class = "btn-warning"
              )
            ),
            shiny::p(
              "No variable groups are currently defined. GQR will therefore use the full binary dummy design."
            ),
            shiny::tags$ul(
              shiny::tags$li(
                sprintf(
                  "%s analysis variables produce %s synthetic combinations.",
                  length(vars),
                  format(design$patterns, big.mark = ",", scientific = FALSE)
                )
              ),
              shiny::tags$li(
                sprintf(
                  "A fully materialised W matrix would require about %.1f MiB before temporary copies and PCA objects.",
                  design$w_memory_mb
                )
              )
            ),
            shiny::p(
              "This may be substantially slower than a grouped design. GQR uses a compact PCA algorithm where possible, but generating and displaying very large designs can still take time and memory."
            ),
            shiny::p(
              "You can cancel the subsequent dummy or PCA calculation from its progress panel without terminating R."
            )
          )
        )
      } else {
        freeze_snapshot_and_move()
      }
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$confirm_ungrouped, {
      shiny::removeModal()
      freeze_snapshot_and_move()
    }, ignoreInit = TRUE)

    list(
      data_raw = raw_data,
      data_trans = data_current,
      analysis_cols = safe_analysis_cols,
      covariate_cols = safe_covariate_cols,
      factor_cols = factor_columns,
      labels = labels,
      groups = current_valid_groups,
      upload_history = upload_history,
      snapshot = data_snapshot
    )
  })
}
