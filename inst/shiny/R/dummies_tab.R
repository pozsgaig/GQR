# src/dummies_tab.R

dummiesTabUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tabPanel(
    "Dummies",
    shiny::div(
      class = "q-container",
      shiny::h2("Dummy combinations and W matrix"),
      gqr_info_box(
        "What are dummies?",
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
        ),
        open = TRUE
      ),
      shiny::p(
        "W is constructed as W = D %*% t(V). Because D contains 0/1 indicators, each value in W is the sum of a respondent's scores for the statements marked 1 in the corresponding row of D."
      ),
      shiny::uiOutput(ns("dummy_info")),
      shiny::verbatimTextOutput(ns("dummy_summary")),
      shiny::uiOutput(ns("dummy_progress")),

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

    D_value <- shiny::reactiveVal(NULL)
    D_task <- shiny::reactiveVal(NULL)
    D_progress <- shiny::reactiveVal(list(value = 0, message = "Waiting"))
    D_error <- shiny::reactiveVal(NULL)

    dummy_mode_used <- shiny::reactive({
      g <- groups_frozen()
      has_assigned <- !is.null(g) &&
        nrow(g |> dplyr::filter(!is.na(.data$variable))) > 0
      if (isTRUE(has_assigned)) "group_one_per" else "all"
    })

    design_estimate <- shiny::reactive({
      shiny::req(snapshot_ready())
      vars <- analysis_cols_frozen()
      df <- data_trans_frozen()
      mode <- dummy_mode_used()

      if (mode == "group_one_per") {
        GQR::gqr_estimate_design(
          variables = vars,
          mode = mode,
          groups = groups_frozen(),
          n_respondents = nrow(df),
          allow_ungrouped = TRUE
        )
      } else {
        GQR::gqr_estimate_design(
          variables = vars,
          mode = mode,
          n_respondents = nrow(df)
        )
      }
    })

    start_dummy_task <- function() {
      shiny::req(snapshot_ready())
      vars <- analysis_cols_frozen()
      mode <- dummy_mode_used()

      old_task <- D_task()
      if (!is.null(old_task)) {
        gqr_app_stop_background(old_task)
      }

      D_value(NULL)
      D_error(NULL)
      D_progress(list(value = 0, message = "Starting dummy design"))

      args <- list(
        variables = vars,
        mode = mode,
        include_empty = TRUE,
        max_patterns = 1000000L
      )
      if (mode == "group_one_per") {
        args$groups <- groups_frozen()
        args$allow_ungrouped <- TRUE
      }

      D_task(
        gqr_app_start_background(
          task = "dummies",
          args = args
        )
      )
    }

    shiny::observeEvent(snapshot(), {
      can_calc(FALSE)
      start_dummy_task()
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    shiny::observe({
      shiny::invalidateLater(250, session)
      task <- D_task()
      if (is.null(task)) return(invisible(NULL))

      D_progress(gqr_app_read_progress(task$status_file))

      if (!isTRUE(task$process$is_alive())) {
        status <- task$process$get_exit_status()

        if (identical(status, 0L)) {
          result <- tryCatch(
            task$process$get_result(),
            error = function(e) e
          )

          if (inherits(result, "error")) {
            D_error(conditionMessage(result))
            D_value(NULL)
          } else {
            D_value(result)
            D_error(NULL)
            D_progress(list(value = 1, message = "Dummy design ready"))
          }
        } else {
          err <- tryCatch(
            {
              task$process$get_result()
              ""
            },
            error = function(e) conditionMessage(e)
          )
          if (!nzchar(err)) {
            err <- "The dummy calculation stopped before completion."
          }
          D_error(err)
          D_value(NULL)
        }

        if (!is.null(task$status_file) && file.exists(task$status_file)) {
          unlink(task$status_file)
        }
        D_task(NULL)
      }
    })

    shiny::observeEvent(input$cancel_dummies, {
      task <- D_task()
      if (!is.null(task)) {
        gqr_app_stop_background(task)
      }
      D_task(NULL)
      D_value(NULL)
      D_error("Dummy calculation cancelled. You can restart it without restarting R.")
      D_progress(list(value = 0, message = "Cancelled"))
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$retry_dummies, {
      start_dummy_task()
    }, ignoreInit = TRUE)

    output$dummy_progress <- shiny::renderUI({
      if (!isTRUE(snapshot_ready())) return(NULL)

      task <- D_task()
      err <- D_error()
      ready <- !is.null(D_value())

      if (!is.null(task)) {
        p <- D_progress()
        value <- round(100 * (p$value %||% 0))
        message <- p$message %||% "Working..."

        return(
          shiny::div(
            class = "alert alert-info",
            shiny::strong("Building dummy design: "),
            shiny::span(message),
            shiny::div(
              class = "progress",
              style = "margin-top:8px; margin-bottom:8px;",
              shiny::div(
                class = "progress-bar progress-bar-striped active",
                role = "progressbar",
                style = sprintf("width:%d%%", value),
                sprintf("%d%%", value)
              )
            ),
            shiny::actionButton(
              ns("cancel_dummies"),
              "Stop calculation",
              class = "btn-danger btn-sm"
            )
          )
        )
      }

      if (!is.null(err)) {
        return(
          shiny::div(
            class = "alert alert-warning",
            shiny::strong(err),
            shiny::br(),
            shiny::actionButton(
              ns("retry_dummies"),
              "Restart dummy calculation",
              class = "btn-warning btn-sm",
              style = "margin-top:8px;"
            )
          )
        )
      }

      if (ready) {
        return(
          shiny::div(
            class = "alert alert-success",
            "Dummy design ready."
          )
        )
      }

      NULL
    })

    D_full <- shiny::reactive({
      shiny::req(snapshot_ready())
      D <- D_value()
      shiny::validate(
        shiny::need(!is.null(D), "The dummy design is still being calculated.")
      )
      D
    })

    dummy_count <- shiny::reactive({
      estimate <- design_estimate()
      shiny::req(estimate)
      as.double(estimate$patterns)
    })

    output$dummy_summary <- shiny::renderPrint({
      if (!isTRUE(snapshot_ready())) {
        cat("No frozen data available yet. Set the data on the Data tab and click 'Next'.\n")
        return(invisible(NULL))
      }

      estimate <- design_estimate()
      cat(
        "Total dummy combinations (after current grouping):",
        format(estimate$patterns, big.mark = ",", scientific = FALSE),
        "\n"
      )
      cat(
        "Estimated full W size:",
        sprintf("%.1f MiB", estimate$w_memory_mb),
        "\n"
      )
      cat(
        "The app does not materialise the complete W matrix for PCA when the compact design algorithm can be used.\n"
      )
    })

    output$slider_ui <- shiny::renderUI({
      if (!isTRUE(snapshot_ready())) {
        return(
          shiny::helpText("Set the data on the Data tab and click 'Next' to build the dummy design.")
        )
      }

      nmax <- as.integer(min(dummy_count(), .Machine$integer.max))
      max_plot <- min(nmax, 1000L)
      min_val <- min(10L, max_plot)
      if (min_val < 1L) min_val <- 1L

      shiny::tagList(
        shiny::sliderInput(
          ns("n_rows_plot"),
          "Number of combinations to show in the plots",
          min = min_val,
          max = max_plot,
          value = min(100L, max_plot),
          step = if (max_plot <= 50L) 1L else 10L
        ),
        if (nmax > max_plot) {
          shiny::helpText(
            sprintf(
              "Plots are capped at %s combinations for responsiveness; the analysis still uses all %s combinations.",
              format(max_plot, big.mark = ","),
              format(nmax, big.mark = ",")
            )
          )
        }
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

    filtered_data <- shiny::reactive({
      df <- data_trans_frozen()
      idx <- filtered_indices()
      shiny::req(df)

      shiny::validate(
        shiny::need(length(idx) > 0, "No respondents remain after filtering.")
      )

      df[idx, , drop = FALSE]
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
      estimate <- design_estimate()
      idx <- filtered_indices()

      msg_ui <- NULL
      if (mode == "all") {
        msg_ui <- shiny::helpText(
          "No groups were frozen from the Data tab, so all binary combinations are used. Full binary designs grow exponentially with the number of variables."
        )
      }

      shiny::tagList(
        msg_ui,
        shiny::p(sprintf("Dummy mode: %s", mode)),
        shiny::p(
          sprintf(
            "Total combinations: %s",
            format(estimate$patterns, big.mark = ",", scientific = FALSE)
          )
        ),
        shiny::p(sprintf("Total respondents: %d", nrow(data_trans_frozen()))),
        shiny::p(sprintf("Respondents after filters: %d", length(idx))),
        shiny::p(
          sprintf(
            "Estimated size of a fully materialised W matrix: %.1f MiB.",
            estimate$w_memory_mb
          )
        )
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

      D <- D_full()
      df <- filtered_data()
      vars <- analysis_cols_frozen()
      shiny::req(D, df, vars)

      n_plot <- input$n_rows_plot
      if (is.null(n_plot)) n_plot <- min(100L, nrow(D))
      n_r <- min(as.integer(n_plot), nrow(D))

      # W is created only for the rows needed by the plot. This avoids
      # materialising a potentially hundreds-of-MiB matrix simply to draw a
      # preview.
      W_sub <- GQR::gqr_make_w(
        data = df,
        analysis_cols = vars,
        D = D,
        id_col = if ("ID" %in% names(df)) "ID" else NULL,
        rows = seq_len(n_r),
        algorithm = "matmul"
      )

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

      if (isTRUE(input$show_W_values) && ncol(W_sub) <= 100L && nrow(W_sub) <= 200L) {
        p <- p + ggplot2::geom_text(
          ggplot2::aes(label = sprintf("%.2f", .data$Value)),
          size = 4
        )
      }

      p
    })

    shiny::observeEvent(input$proceed_calc, {
      shiny::req(snapshot_ready())

      if (is.null(D_value())) {
        shiny::showNotification(
          "The dummy design is not ready yet. Wait for it to finish, or restart the calculation if it was cancelled.",
          type = "warning",
          duration = 6
        )
        return(invisible(NULL))
      }

      n <- dummy_count()
      shiny::req(n)

      if (n > 100000) {
        shiny::showModal(
          shiny::modalDialog(
            title = "Large number of dummy combinations",
            easyClose = FALSE,
            footer = shiny::tagList(
              shiny::modalButton("Cancel"),
              shiny::actionButton(ns("confirm_calc"), "Proceed anyway")
            ),
            paste0(
              "The current frozen selection produces ", format(n, big.mark = ","), " dummy combinations. ",
              "GQR will use the compact statement-space PCA algorithm where possible, ",
              "but reconstructing scores for this many combinations may still take time.\n\n",
              "The PCA runs in a separate R process and can be stopped from the PCA tab without terminating R. ",
              "You can also go back to the Data tab and reduce the design with groups."
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
      D = D_full,
      can_calc = can_calc,
      dummy_count = dummy_count,
      design_estimate = design_estimate,
      filtered_indices = filtered_indices,
      filtered_data = filtered_data,
      snapshot = snapshot
    )
  })
}
