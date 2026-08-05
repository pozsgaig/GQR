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
            )
          )
        )
      ),

      # 2. Column names / labels
      shiny::div(
        class = "panel panel-primary",
        shiny::div(
          class = "panel-heading",
          shiny::tags$div(
            class = "q-panel-header",
            `data-toggle` = "collapse",
            `data-target` = paste0("#", ns("sec_labels")),
            shiny::HTML(" 2. Column names / labels (click to expand/collapse)")
          )
        ),
        shiny::div(
          id = ns("sec_labels"),
          class = "panel-collapse collapse in",
          shiny::div(
            class = "panel-body q-panel",
            shiny::p("Internal names are taken from the data frame. Edit human-readable labels below."),
            shiny::uiOutput(ns("label_edit_ui")),
            shiny::actionButton(ns("save_labels"), "Save labels")
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

    check_loaded_data <- function(df) {
      if (!is.data.frame(df)) {
        stop("The selected object is not a data frame.", call. = FALSE)
      }

      if (ncol(df) == 1L && grepl("[,;\t]", names(df)[1L])) {
        stop(
          "The data were read as one column. Check the file delimiter or reload the bundled RDA example.",
          call. = FALSE
        )
      }

      df
    }

    raw_data <- shiny::reactive({
      if (!is.null(input$file)) {
        check_loaded_data(gqr_read_upload(input$file$datapath, input$file$name))
      } else if (isTRUE(input$use_example)) {
        ds <- input$example_dataset
        shiny::req(built_in_data)
        shiny::req(ds)
        check_loaded_data(built_in_data[[ds]])
      } else {
        NULL
      }
    })

    output$example_dataset_note <- shiny::renderUI({
      if (!isTRUE(input$use_example) || !identical(input$example_dataset, "Gardening")) {
        return(NULL)
      }

      shiny::div(
        class = "help-block",
        "This example is a selected-column extract from the gardening dataset published in Scientific Data (2026) and analysed in Urban Forestry & Urban Greening (2024, 2025). Full references are provided on the Home tab and in ?gardening."
      )
    })

    # ---- labels ----
    labels <- shiny::reactiveVal(NULL)

    shiny::observeEvent(raw_data(), {
      df <- raw_data()
      shiny::req(df)
      labels(
        tibble::tibble(
          name  = names(df),
          label = names(df)
        )
      )
    }, ignoreNULL = TRUE)

    output$label_edit_ui <- shiny::renderUI({
      shiny::req(labels())
      labs <- labels()

      n <- nrow(labs)
      per_row <- 3
      rows <- split(seq_len(n), ceiling(seq_len(n) / per_row))

      shiny::tagList(
        lapply(rows, function(idx_vec) {
          shiny::fluidRow(
            lapply(idx_vec, function(i) {
              shiny::column(
                width = 12 / per_row,
                shiny::textInput(
                  ns(paste0("lab_", i)),
                  label = labs$name[i],
                  value = labs$label[i]
                )
              )
            })
          )
        })
      )
    })

    shiny::observeEvent(input$save_labels, {
      shiny::req(labels())
      labs <- labels()
      for (i in seq_len(nrow(labs))) {
        id <- paste0("lab_", i)
        val <- input[[id]]
        if (!is.null(val)) labs$label[i] <- val
      }
      labels(labs)
    }, ignoreInit = TRUE)

    # ---- column roles ----
    output$col_roles_ui <- shiny::renderUI({
      shiny::req(raw_data())
      df <- raw_data()
      cols <- names(df)
      numeric_cols <- cols[vapply(df, is.numeric, logical(1))]

      shiny::tagList(
        shiny::h4("Assign roles"),
        shiny::selectInput(
          ns("analysis_cols"),
          "Columns for analysis",
          choices  = cols,
          selected = numeric_cols,
          multiple = TRUE
        ),
        shiny::selectInput(
          ns("covariate_cols"),
          "Covariate columns",
          choices  = cols,
          selected = setdiff(cols, numeric_cols),
          multiple = TRUE
        )
      )
    })

    # ---- transformation (local to Data tab; frozen only on Next) ----
    transformed_data <- shiny::reactive({
      shiny::req(raw_data(), input$analysis_cols)

      df_raw <- raw_data()
      cols <- input$analysis_cols

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
      if (is.null(df_tr)) raw_data() else df_tr
    })

    # ---- groups ----

    shiny::observeEvent(input$group_file, {
      shiny::req(input$group_file)
      gf <- readr::read_csv(input$group_file$datapath, show_col_types = FALSE)
      if (all(c("group", "variable") %in% names(gf))) {
        group_mapping(
          gf |>
            dplyr::distinct(.data$group, .data$variable)
        )
      }
    })

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
        btn_id <- paste0("delete_group_", make.names(g))
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

      analysis_cols <- input$analysis_cols
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
              ns(paste0("group_vars_", g)),
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
              ns(paste0("delete_group_", make.names(g))),
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
        lapply(gnames, function(g) input[[paste0("group_vars_", g)]]),
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
      shiny::req(raw_data(), input$analysis_cols)
      intersect(input$analysis_cols, colnames(raw_data()))
    })

    safe_covariate_cols <- shiny::reactive({
      shiny::req(raw_data(), input$covariate_cols)
      intersect(input$covariate_cols, colnames(raw_data()))
    })

    shiny::observeEvent(
      list(input$use_random_groups, input$n_random_groups, input$analysis_cols),
      {
        shiny::req(input$analysis_cols)
        if (!isTRUE(input$use_random_groups)) return(NULL)

        vars <- input$analysis_cols
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
    shiny::observeEvent(input$next_to_dummies, {
      df_trans <- data_current()
      vars <- safe_analysis_cols()
      covs <- safe_covariate_cols()
      groups_cur <- current_valid_groups()
      labs <- labels()

      shiny::req(df_trans, vars)

      data_snapshot(
        list(
          data_raw = raw_data(),
          data_trans = df_trans,
          analysis_cols = vars,
          covariate_cols = covs,
          labels = labs,
          groups = groups_cur,
          transform_method = input$transform_method
        )
      )

      shiny::updateNavbarPage(
        session = session$rootScope(),
        inputId = "main_tabs",
        selected = "Dummies"
      )
    }, ignoreInit = TRUE)

    list(
      data_raw = raw_data,
      data_trans = data_current,
      analysis_cols = safe_analysis_cols,
      covariate_cols = safe_covariate_cols,
      labels = labels,
      groups = current_valid_groups,
      snapshot = data_snapshot
    )
  })
}
