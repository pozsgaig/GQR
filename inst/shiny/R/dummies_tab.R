# src/dummies_tab.R

dummiesTabUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tabPanel(
    "Dummies",
    shiny::div(
      class = "q-container",
      shiny::h2("Dummy combinations and W matrix"),
      shiny::div(
        class = "alert alert-info",
        shiny::h4("What are dummies?"),
        shiny::p(
          "Here, dummy means a binary (0/1) indicator used to construct synthetic combined statements. Each column of the dummy matrix D represents one original analysis statement, and each row represents one possible or sampled combination of statements."
        ),
        shiny::tags$ul(
          shiny::tags$li(shiny::strong("1"), " = the statement is included in the synthetic combination"),
          shiny::tags$li(shiny::strong("0"), " = the statement is not included")
        ),
        shiny::p(
          "For example, for statements A, B, and C, the row (1, 0, 1) represents a synthetic statement containing A and C but not B. These indicators are not artificial respondents and do not represent missing data."
        ),
        shiny::p(
          "In a full design, D contains all permitted 0/1 patterns. In a grouped design, exactly one statement is selected from each group. In a random design, a requested number of patterns is sampled."
        )
      ),
      shiny::p(
        "W is constructed as W = D %*% t(V). Because D contains 0/1 indicators, each value in W is the sum of a respondent's scores for the statements marked 1 in the corresponding row of D."
      ),
      shiny::uiOutput(ns("dummy_info")),
      shiny::verbatimTextOutput(ns("dummy_summary")),

      # ---- filters (collapsible) ----
      shiny::div(
        class = "panel panel-primary",
        shiny::div(
          class = "panel-heading",
          shiny::tags$div(
            class = "q-panel-header",
            `data-toggle` = "collapse",
            `data-target` = paste0("#", ns("sec_filters")),
            shiny::HTML(" Respondent filters (click to expand/collapse)")
          )
        ),
        shiny::div(
          id = ns("sec_filters"),
          class = "panel-collapse collapse",
          shiny::div(
            class = "panel-body q-panel",
            shiny::fluidRow(
              shiny::column(
                6,
                shiny::uiOutput(ns("covariate_filters_ui"))
              ),
              shiny::column(
                6,
                shiny::h4("Filter by IDs"),
                shiny::textInput(
                  ns("id_filter"),
                  "Comma-separated list of IDs to keep (optional)",
                  value = ""
                ),
                shiny::helpText(
                  "If non-empty, only respondents whose IDs are in this list ",
                  "will be retained (after applying covariate filters)."
                )
              )
            )
          )
        )
      ),

      # ---- heatmap ----
      shiny::fluidRow(
        shiny::column(
          6,
          shiny::radioButtons(
            ns("dummy_var_order"),
            "Order variables in dummy plot:",
            choices = c(
              "Alphabetical"   = "alphabetical",
              "As in Data tab" = "data",
              "By group size"  = "group_size"
            ),
            selected = "data",
            inline = TRUE
          )
        ),
        shiny::column(
          6,
          shiny::uiOutput(ns("slider_ui"))
        )
      ),
      shiny::br(),
      shiny::h4("Dummy matrix D preview"),
      shiny::plotOutput(ns("D_plot"), height = "450px"),
      shiny::hr(),
      shiny::h4("W matrix preview"),
      shiny::checkboxInput(ns("show_W_values"), "Show numeric values in W", value = FALSE),
      shiny::plotOutput(ns("W_heatmap"), height = "600px"),

      shiny::div(
        style = "text-align: right; margin-top: 10px;",
        shiny::actionButton(ns("proceed_calc"), "Proceed to calculations \u2192")
      )
    )
  )
}

dummiesTabServer <- function(id, data_state, active_tab) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    can_calc <- shiny::reactiveVal(FALSE)

    snapshot <- shiny::reactive({
      data_state$snapshot()
    })

    snapshot_ready <- shiny::reactive({
      !is.null(snapshot())
    })

    data_trans_frozen <- shiny::reactive({
      s <- snapshot()
      shiny::req(s)
      s$data_trans
    })

    analysis_cols_frozen <- shiny::reactive({
      s <- snapshot()
      shiny::req(s)
      s$analysis_cols
    })

    covariate_cols_frozen <- shiny::reactive({
      s <- snapshot()
      shiny::req(s)
      s$covariate_cols %||% character(0)
    })

    groups_frozen <- shiny::reactive({
      s <- snapshot()
      shiny::req(s)
      s$groups
    })

    shiny::observeEvent(snapshot(), {
      can_calc(FALSE)
    }, ignoreInit = TRUE)

    dummy_mode_used <- shiny::reactive({
      g <- groups_frozen()
      has_assigned <- !is.null(g) &&
        nrow(g |> dplyr::filter(!is.na(.data$variable))) > 0
      if (isTRUE(has_assigned)) "group_one_per" else "all"
    })

    D_full <- shiny::reactive({
      shiny::req(snapshot_ready())

      df <- data_trans_frozen()
      vars <- analysis_cols_frozen()
      shiny::req(df, vars)

      mode <- dummy_mode_used()

      D <- if (mode == "group_one_per") {
        groups <- groups_frozen()
        shiny::req(groups, nrow(groups) > 0)

        gqr_generate_all_dummies(
          nvars = length(vars),
          varnames = vars,
          groups = groups,
          mode = "group_one_per"
        )
      } else {
        gqr_generate_all_dummies(
          nvars = length(vars),
          varnames = vars,
          mode = "all"
        )
      }

      rownames(D) <- paste0("S", seq_len(nrow(D)))
      D
    })

    W_full <- shiny::reactive({
      shiny::req(snapshot_ready())

      df <- data_trans_frozen()
      vars <- analysis_cols_frozen()
      D <- D_full()

      shiny::req(df, vars, D)

      V <- as.matrix(df[, vars, drop = FALSE])
      W <- D %*% t(V)

      rownames(W) <- rownames(D)

      if ("ID" %in% colnames(df)) {
        colnames(W) <- as.character(df$ID)
      } else {
        num_digits <- nchar(as.character(nrow(df)))
        colnames(W) <- sprintf(paste0("R%0", num_digits, "d"), seq_len(nrow(df)))
      }

      W
    })

    dummy_count <- shiny::reactive({
      D <- D_full()
      shiny::req(D)
      nrow(D)
    })

    output$dummy_summary <- shiny::renderPrint({
      if (!isTRUE(snapshot_ready())) {
        cat("No frozen data available yet. Set the data on the Data tab and click 'Next'.\n")
        return(invisible(NULL))
      }

      n <- dummy_count()
      cat("Total dummy combinations (after current grouping):", n, "\n")
    })

    output$slider_ui <- shiny::renderUI({
      if (!isTRUE(snapshot_ready())) {
        return(
          shiny::helpText("Set the data on the Data tab and click 'Next' to build D and W.")
        )
      }

      W <- W_full()
      shiny::req(W)

      nmax <- nrow(W)
      min_val <- min(10, nmax)
      if (min_val < 1) min_val <- 1

      shiny::sliderInput(
        ns("n_rows_plot"),
        "Number of combinations (rows of W) to show",
        min = min_val,
        max = nmax,
        value = nmax,
        step = if (nmax <= 50) 1 else 10
      )
    })

    data_covariates <- shiny::reactive({
      shiny::req(snapshot_ready())

      df <- data_trans_frozen()
      covs <- covariate_cols_frozen()

      shiny::req(df)

      if (is.null(covs)) covs <- character(0)
      covs <- setdiff(covs, "ID")

      df[, covs, drop = FALSE]
    })

    output$covariate_filters_ui <- shiny::renderUI({
      if (!isTRUE(snapshot_ready())) {
        return(
          shiny::helpText("No snapshot yet. Click 'Next' on the Data tab first.")
        )
      }

      cov_df <- data_covariates()
      shiny::req(cov_df)

      cov_names <- colnames(cov_df)

      if (length(cov_names) == 0) {
        return(
          shiny::helpText(
            "No covariate filters. ",
            "If you need them, select covariate columns on the Data tab before clicking 'Next'."
          )
        )
      }

      controls <- lapply(cov_names, function(v) {
        col <- cov_df[[v]]
        id <- ns(paste0("filter_", v))

        if (is.character(col) || is.factor(col)) {
          vals <- sort(unique(col))
          shiny::selectInput(
            inputId = id,
            label = v,
            choices = vals,
            selected = vals,
            multiple = TRUE
          )
        } else if (is.numeric(col)) {
          rng <- range(col, na.rm = TRUE)
          shiny::sliderInput(
            inputId = id,
            label = v,
            min = rng[1],
            max = rng[2],
            value = rng,
            step = (rng[2] - rng[1]) / 100
          )
        } else {
          NULL
        }
      })

      do.call(shiny::tagList, controls)
    })

    filtered_indices <- shiny::reactive({
      shiny::req(snapshot_ready())

      df <- data_trans_frozen()
      cov_df <- data_covariates()
      shiny::req(df, cov_df)

      idx <- seq_len(nrow(df))

      for (v in colnames(cov_df)) {
        col <- cov_df[[v]]
        input_id <- paste0("filter_", v)
        val <- input[[input_id]]

        if (is.null(val)) next

        if (is.character(col) || is.factor(col)) {
          idx <- idx[col[idx] %in% val]
        } else if (is.numeric(col)) {
          rng <- val
          idx <- idx[col[idx] >= rng[1] & col[idx] <= rng[2]]
        }
      }

      id_filter_raw <- trimws(input$id_filter)
      if (nzchar(id_filter_raw)) {
        id_vec <- trimws(unlist(strsplit(id_filter_raw, ",")))
        if ("ID" %in% colnames(df)) {
          idx <- idx[df$ID[idx] %in% id_vec]
        }
      }

      idx
    })

    filtered_W <- shiny::reactive({
      W <- W_full()
      shiny::req(W)

      idx <- filtered_indices()

      validate(
        shiny::need(length(idx) > 0, "No respondents remain after filtering.")
      )

      idx_cols <- idx[idx <= ncol(W)]

      validate(
        shiny::need(length(idx_cols) > 0, "No matching respondent columns remain in W.")
      )

      W[, idx_cols, drop = FALSE]
    })


    output$dummy_info <- shiny::renderUI({
      if (!isTRUE(snapshot_ready())) {
        return(
          shiny::tagList(
            shiny::p("No frozen data have been received yet."),
            shiny::helpText("Complete the settings on the Data tab and click 'Next'.")
          )
        )
      }

      mode <- dummy_mode_used()
      g <- groups_frozen()

      msg_ui <- NULL
      if (mode == "all" && is.null(g)) {
        msg_ui <- shiny::helpText(
          "No groups were frozen from the Data tab, so all binary combinations are used."
        )
      }

      W_try <- tryCatch(W_full(), error = function(e) e)

      if (inherits(W_try, "error")) {
        msg <- conditionMessage(W_try)
        if (!nzchar(msg)) {
          msg <- "W could not be built from the frozen snapshot."
        }

        return(
          shiny::tagList(
            msg_ui,
            shiny::p(sprintf("Dummy mode: %s", mode)),
            shiny::p(paste("W error:", msg))
          )
        )
      }

      W <- W_try
      idx <- filtered_indices()

      shiny::tagList(
        msg_ui,
        shiny::p(sprintf("Dummy mode: %s", mode)),
        shiny::p(sprintf("Total combinations (rows in W): %d", nrow(W))),
        shiny::p(sprintf("Total respondents: %d", ncol(W))),
        shiny::p(sprintf("Respondents after filters: %d", length(idx)))
      )
    })

    output$D_plot <- shiny::renderPlot({
      shiny::req(snapshot_ready())

      D <- D_full()
      shiny::req(D)

      nplot <- input$n_rows_plot
      if (is.null(nplot)) nplot <- nrow(D)
      nr <- min(nplot, nrow(D))
      Dsub <- D[seq_len(nr), , drop = FALSE]

      var_order <- input$dummy_var_order
      if (is.null(var_order)) var_order <- "data"

      vars <- colnames(Dsub)
      gm <- groups_frozen()

      if (!is.null(gm) && nrow(gm) > 0) {
        gm <- gm |>
          dplyr::filter(!is.na(.data$variable)) |>
          dplyr::distinct(.data$group, .data$variable) |>
          dplyr::filter(.data$variable %in% colnames(Dsub))

        if (nrow(gm) > 0) {
          group_sizes <- gm |>
            dplyr::count(.data$group, name = "group_n")

          ordered_groups <-
            if (identical(var_order, "data")) {
              unique(gm$group)
            } else if (identical(var_order, "group_size")) {
              group_sizes |>
                dplyr::arrange(dplyr::desc(.data$group_n), .data$group) |>
                dplyr::pull(.data$group)
            } else {
              sort(unique(gm$group))
            }

          gm_alpha <- gm |>
            dplyr::mutate(group = factor(.data$group, levels = ordered_groups)) |>
            dplyr::arrange(.data$group, .data$variable)

          vars_grouped <- gm_alpha$variable
          vars_ungrouped <- setdiff(colnames(Dsub), vars_grouped)
          vars <- c(vars_grouped, sort(vars_ungrouped))
        } else {
          if (identical(var_order, "alphabetical")) {
            vars <- sort(colnames(Dsub))
          } else if (identical(var_order, "data")) {
            vars <- colnames(Dsub)
          } else {
            vars <- sort(colnames(Dsub))
          }
        }
      } else {
        if (identical(var_order, "alphabetical")) {
          vars <- sort(colnames(Dsub))
        } else if (identical(var_order, "data")) {
          vars <- colnames(Dsub)
        } else {
          vars <- sort(colnames(Dsub))
        }
      }

      Dsub <- Dsub[, vars, drop = FALSE]

      label_map <- stats::setNames(
        abbreviate_dummy_names(colnames(Dsub), max_n = 10),
        colnames(Dsub)
      )

      dlong <- as.data.frame(Dsub) |>
        dplyr::mutate(pattern = dplyr::row_number()) |>
        tidyr::pivot_longer(
          cols = -pattern,
          names_to = "variable",
          values_to = "value"
        ) |>
        dplyr::mutate(
          variable = factor(.data$variable, levels = colnames(Dsub))
        )

      ggplot2::ggplot(
        dlong,
        ggplot2::aes(x = .data$variable, y = .data$pattern, fill = factor(.data$value))
      ) +
        ggplot2::geom_tile(colour = "grey30") +
        ggplot2::scale_fill_manual(values = c("0" = "#CC0000", "1" = "#009933"), name = "Dummy") +
        ggplot2::scale_y_reverse() +
        ggplot2::scale_x_discrete(labels = label_map) +
        ggplot2::labs(x = "Variable", y = "Pattern index") +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 11),
          axis.ticks.x = ggplot2::element_blank()
        )
    })

    output$W_heatmap <- shiny::renderPlot({
      shiny::req(snapshot_ready())

      W <- W_full()
      shiny::req(W)

      idx <- filtered_indices()
      if (length(idx) == 0) return(NULL)

      n_cols <- ncol(W)
      idx_cols <- idx[idx <= n_cols]
      if (length(idx_cols) == 0) return(NULL)

      n_plot <- input$n_rows_plot
      if (is.null(n_plot)) n_plot <- nrow(W)
      n_r <- min(n_plot, nrow(W))

      W_sub <- W[seq_len(n_r), idx_cols, drop = FALSE]

      df_long <- as.data.frame(W_sub) |>
        dplyr::mutate(Combination = dplyr::row_number()) |>
        tidyr::pivot_longer(
          cols = -"Combination",
          names_to = "Respondent",
          values_to = "Value"
        ) |>
        dplyr::mutate(
          Respondent = factor(.data$Respondent, levels = colnames(W_sub))
        )

      p <- ggplot2::ggplot(
        df_long,
        ggplot2::aes(x = .data$Respondent, y = .data$Combination, fill = .data$Value)
      ) +
        ggplot2::geom_tile() +
        ggplot2::scale_fill_viridis_c(option = "C", direction = 1) +
        ggplot2::scale_y_reverse() +
        ggplot2::labs(
          x = "Respondent",
          y = "Combination index (S_i)",
          fill = "W value"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          axis.text.x = ggplot2::element_blank(),
          axis.ticks.x = ggplot2::element_blank()
        )

      if (isTRUE(input$show_W_values)) {
        p <- p + ggplot2::geom_text(
          ggplot2::aes(label = sprintf("%.2f", .data$Value)),
          size = 4
        )
      }

      p
    })

    shiny::observeEvent(input$proceed_calc, {
      shiny::req(snapshot_ready())
      n <- dummy_count()
      shiny::req(n)

      if (n > 1000) {
        shiny::showModal(
          shiny::modalDialog(
            title = "Large number of dummy combinations",
            easyClose = FALSE,
            footer = shiny::tagList(
              shiny::modalButton("Cancel"),
              shiny::actionButton(ns("confirm_calc"), "Proceed anyway")
            ),
            paste0(
              "The current frozen selection produces ", n, " dummy combinations. ",
              "Running PCA on this many combinations may be slow.\n\n",
              "You can go back to the Data tab and click 'Next' again after changing the setup, ",
              "or click 'Proceed anyway' to continue."
            )
          )
        )
      } else {
        can_calc(TRUE)
        shiny::updateNavbarPage(
          session = session$rootScope(),
          inputId = "main_tabs",
          selected = "PCA"
        )
      }
    })

    shiny::observeEvent(active_tab(), {
      if (identical(active_tab(), "Dummies")) {
        can_calc(FALSE)
      }
    }, ignoreInit = TRUE)


    shiny::observeEvent(input$confirm_calc, {
      can_calc(TRUE)
      shiny::removeModal()
      shiny::updateNavbarPage(
        session = session$rootScope(),
        inputId = "main_tabs",
        selected = "PCA"
      )
    }, ignoreInit = TRUE)

    list(
      W = W_full,           # unfiltered for reference
      W_filtered = filtered_W,  # filtered by user
      D = D_full,
      can_calc = can_calc,
      dummy_count = dummy_count,
      snapshot = snapshot
    )
  })
}
