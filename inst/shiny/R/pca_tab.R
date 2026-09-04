# src/pca_tab.R

pcaTabUI <- function(id) {
  ns <- NS(id)

  tabPanel(
    "PCA",

    tags$head(
      tags$style(HTML("
.pca-detail-summary {
  display: block;
  font-weight: 700;
  font-size: 1.05em;
  color: #FFFFFF;
  background-color: #404F69;
  padding: 10px 14px;
  border-radius: 6px;
  cursor: pointer;
  margin-bottom: 6px;
}
details.pca-detail-box {
  margin-bottom: 10px;
  border: 1px solid #D9D9D9;
  border-radius: 6px;
  padding: 0;
  background: #FAFAFA;
}
details.pca-detail-box > div {
  padding: 12px 14px;
}
.pca-modal .modal-body {
  max-height: 70vh;
  overflow-y: auto;
}
.pca-modal .modal-dialog {
  width: 88vw;
  max-width: 88vw;
}
")),
      tags$script(HTML("
$(document).on('shown.bs.modal', '.pca-modal', function () {
  var $dlg = $(this).find('.modal-dialog');
  if (!$dlg.data('ui-ready')) {
    $dlg.draggable({ handle: '.modal-header' });
    $dlg.resizable({
      minHeight: 300,
      minWidth: 700
    });
    $dlg.data('ui-ready', true);
  }
});
"))
    ),

    div(
      class = "q-container",
      h2("Principal Component Analysis"),
      p("Extract latent respondent patterns from the Generalised Q synthetic-statement matrix."),

      gqr_info_box(
        "What is calculated on this tab?",
        p(
          "GQR analyses the W matrix with synthetic statement combinations in rows and respondents in columns. PCA scores therefore describe the synthetic combinations, while PCA loadings describe respondents."
        ),
        p(
          "Ordinary PCA uses the standard centred/scaled PCA solution. For large designs, GQR can compute the same PCA exactly in the smaller statement space without materialising the complete W matrix."
        ),
        p(
          "SPSS-style mode instead uses the respondent correlation matrix, smooths it when necessary, and applies psych::principal() with regression-type component scores. This preserves compatibility with the original GQR implementation and SPSS-oriented calculations."
        ),
        p(
          "Varimax is an orthogonal rotation used to make retained components easier to interpret. Rotation can change component order and orientation, but not the retained multidimensional subspace."
        ),
        open = TRUE
      ),

      fluidRow(
        column(
          8,
          h4("Explained variance and scree"),
          downloadButton(ns("download_scree_png"), "Download PNG"),
          downloadButton(ns("download_scree_pdf"), "Download PDF"),
          br(), br(),
          plotOutput(ns("scree_plot"), height = "360px")
        ),
        column(
          4,
          numericInput(
            ns("n_comp"),
            "Number of components to compute and show:",
            value = 5, min = 1, max = 50, step = 1
          ),
          selectInput(
            ns("rotation"),
            "Rotation:",
            choices = c("None" = "none", "Varimax" = "varimax"),
            selected = "none"
          ),
          checkboxInput(ns("center"), "Centre variables", value = TRUE),
          checkboxInput(ns("scale"), "Scale variables", value = TRUE),
          checkboxInput(ns("SPSS"), "SPSS-style (correlation-based) scoring", value = FALSE),
          checkboxInput(ns("impute_mean"), "Mean-impute missing values (SPSS mode)", value = TRUE),
          br(),
          actionButton(ns("run_pca"), "Run / update PCA", class = "btn-primary"),
          br(), br(),
          uiOutput(ns("pca_progress")),
          actionButton(ns("show_scored_W"), "Show W matrix with PCA scores")
        )
      ),

      br(),

      h4("PCA loadings tables"),
      actionButton(ns("show_unrotated_loadings"), "Show unrotated loadings table"),
      actionButton(ns("show_rotated_loadings"), "Show rotated loadings table"),
      br(), br(),

      h4("PCA results"),

      tags$details(
        class = "pca-detail-box",
        open = TRUE,
        tags$summary(
          span(class = "pca-detail-summary", HTML("▸ Overview (click to open/close)"))
        ),
        div(verbatimTextOutput(ns("pca_overview")))
      ),

      tags$details(
        class = "pca-detail-box",
        tags$summary(
          span(class = "pca-detail-summary", HTML("▸ Eigenvalues (click to open/close)"))
        ),
        div(verbatimTextOutput(ns("pca_eigen")))
      ),

      tags$details(
        class = "pca-detail-box",
        tags$summary(
          span(class = "pca-detail-summary", HTML("▸ Explained variance (click to open/close)"))
        ),
        div(verbatimTextOutput(ns("pca_var")))
      ),

      br()
    )
  )
}

pcaTabServer <- function(id, data_state, dummies_state = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    pca_snapshot <- reactive({
      if (!is.null(dummies_state) && !is.null(dummies_state$snapshot)) {
        dummies_state$snapshot()
      } else {
        NULL
      }
    })

    pca_data_trans <- reactive({
      s <- pca_snapshot()
      if (!is.null(s)) {
        s$data_trans
      } else {
        data_state$data_trans()
      }
    })

    pca_covariate_cols <- reactive({
      s <- pca_snapshot()
      if (!is.null(s)) {
        s$covariate_cols %||% character(0)
      } else {
        data_state$covariate_cols() %||% character(0)
      }
    })



    # ---------- background PCA calculation ----------
    pca_value <- reactiveVal(NULL)
    pca_task <- reactiveVal(NULL)
    pca_progress <- reactiveVal(list(value = 0, message = "Waiting"))
    pca_error <- reactiveVal(NULL)
    pca_settings <- reactiveVal(NULL)

    pca_input_data <- reactive({
      if (!is.null(dummies_state) && !is.null(dummies_state$filtered_data)) {
        dummies_state$filtered_data()
      } else {
        pca_data_trans()
      }
    })

    start_pca_task <- function() {
      req(!is.null(dummies_state), dummies_state$D())
      df <- pca_input_data()
      D <- dummies_state$D()
      req(df, D)

      validate(
        need(nrow(df) > 1, "Not enough respondents remain for PCA.")
      )

      old_task <- pca_task()
      if (!is.null(old_task)) {
        gqr_app_stop_background(old_task)
      }

      pca_value(NULL)
      pca_error(NULL)
      pca_progress(list(value = 0, message = "Starting PCA"))

      id_col <- if ("ID" %in% names(df)) "ID" else NULL

      settings <- list(
        n_components = as.integer(input$n_comp),
        rotation = input$rotation,
        center = isTRUE(input$center),
        scale = isTRUE(input$scale),
        SPSS = isTRUE(input$SPSS),
        impute_mean = isTRUE(input$impute_mean),
        engine = if (isTRUE(input$SPSS)) "matrix" else "design",
        method = if (isTRUE(input$SPSS)) "correlation" else "prcomp",
        id_col = id_col,
        data_filter = if (!is.null(dummies_state$filter_spec)) dummies_state$filter_spec() else NULL,
        n_respondents = nrow(df)
      )
      pca_settings(settings)

      if (!isTRUE(input$SPSS)) {
        task <- gqr_app_start_background(
          task = "pca_design",
          args = list(
            data = df,
            D = D,
            analysis_cols = colnames(D),
            id_col = id_col,
            n_components = settings$n_components,
            rotation = settings$rotation,
            center = settings$center,
            scale = settings$scale,
            na_action = "error"
          )
        )
      } else {
        task <- gqr_app_start_background(
          task = "pca_matrix",
          args = list(
            data = if (inherits(df, "gqr_prepared_data")) df else
              GQR::gqr_prepare_data(
                data = df,
                analysis_cols = colnames(D),
                id_col = id_col
              ),
            D = D,
            na_action = if (settings$impute_mean) "mean" else "error",
            pca_args = list(
              n_components = settings$n_components,
              rotation = settings$rotation,
              center = settings$center,
              scale = settings$scale,
              method = "correlation",
              impute = if (settings$impute_mean) "mean" else "none"
            )
          )
        )
      }

      pca_task(task)
    }

    observeEvent(
      if (!is.null(dummies_state) && !is.null(dummies_state$can_calc)) {
        dummies_state$can_calc()
      } else {
        FALSE
      },
      {
        if (isTRUE(dummies_state$can_calc())) {
          start_pca_task()
        }
      },
      ignoreInit = TRUE
    )

    observeEvent(input$run_pca, {
      if (!is.null(dummies_state) &&
          !is.null(dummies_state$can_calc) &&
          !isTRUE(dummies_state$can_calc())) {
        showNotification(
          "Return to the Dummies tab and click 'Proceed to calculations' first.",
          type = "warning",
          duration = 5
        )
        return(invisible(NULL))
      }
      start_pca_task()
    }, ignoreInit = TRUE)

    observe({
      invalidateLater(250, session)
      task <- pca_task()
      if (is.null(task)) return(invisible(NULL))

      pca_progress(gqr_app_read_progress(task$status_file))

      if (!isTRUE(task$process$is_alive())) {
        status <- task$process$get_exit_status()

        if (identical(status, 0L)) {
          result <- tryCatch(
            task$process$get_result(),
            error = function(e) e
          )

          if (inherits(result, "error")) {
            pca_error(conditionMessage(result))
            pca_value(NULL)
          } else {
            pca_value(result)
            pca_error(NULL)
            pca_progress(list(value = 1, message = "PCA ready"))
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
            err <- "The PCA calculation stopped before completion."
          }
          pca_error(err)
          pca_value(NULL)
        }

        if (!is.null(task$status_file) && file.exists(task$status_file)) {
          unlink(task$status_file)
        }
        pca_task(NULL)
      }
    })

    observeEvent(input$cancel_pca, {
      task <- pca_task()
      if (!is.null(task)) {
        gqr_app_stop_background(task)
      }
      pca_task(NULL)
      pca_value(NULL)
      pca_error("PCA calculation cancelled. Change the settings if needed, then click 'Run / update PCA'.")
      pca_progress(list(value = 0, message = "Cancelled"))
    }, ignoreInit = TRUE)

    output$pca_progress <- renderUI({
      task <- pca_task()
      err <- pca_error()
      result <- pca_value()

      if (!is.null(task)) {
        p <- pca_progress()
        value <- round(100 * (p$value %||% 0))
        message <- p$message %||% "Working..."

        return(
          div(
            class = "alert alert-info",
            strong(if (isTRUE(input$SPSS)) {
              "Running full-W correlation PCA: "
            } else {
              "Running compact PCA: "
            }),
            span(message),
            div(
              class = "progress",
              style = "margin-top:8px; margin-bottom:8px;",
              div(
                class = "progress-bar progress-bar-striped active",
                role = "progressbar",
                style = sprintf("width:%d%%", value),
                sprintf("%d%%", value)
              )
            ),
            actionButton(
              ns("cancel_pca"),
              "Stop calculation",
              class = "btn-danger btn-sm"
            )
          )
        )
      }

      if (!is.null(err)) {
        return(
          div(
            class = "alert alert-warning",
            err
          )
        )
      }

      if (!is.null(result)) {
        settings <- pca_settings()
        return(
          div(
            class = "alert alert-success",
            if (isTRUE(settings$SPSS)) {
              "PCA ready (full-W correlation mode)."
            } else {
              "PCA ready (compact exact design algorithm; full W was not required)."
            }
          )
        )
      }

      helpText("Click 'Run / update PCA' after changing PCA settings.")
    })

    # The background process computes one PCA only. Both unrotated and selected
    # results are retained in the same object, avoiding the previous duplicate
    # PCA calculation.
    pca_unrotated <- reactive({
      pr <- pca_value()
      req(pr)

      list(
        eigenvalues = unname(pr$eigenvalues),
        var_expl = unname(pr$variance_explained),
        loadings_rot = pr$loadings_unrotated,
        scores = pr$scores_unrotated,
        sdev = sqrt(unname(pr$eigenvalues))
      )
    })

    pca_selected <- reactive({
      pr <- pca_value()
      req(pr)

      list(
        eigenvalues = unname(pr$eigenvalues),
        var_expl = unname(pr$variance_explained),
        loadings_rot = pr$loadings,
        scores = pr$scores,
        sdev = sqrt(unname(pr$eigenvalues))
      )
    })

    # ---------- textual outputs ----------
    output$pca_overview <- renderPrint({
      pr <- pca_selected()
      req(pr)

      settings <- pca_settings()
      cat("Number of components:", ncol(pr$loadings_rot), "\n")
      cat("Rotation:", settings$rotation, "\n")
      cat("SPSS mode:", isTRUE(settings$SPSS), "\n")
      cat("Centred:", isTRUE(settings$center), "\n")
      cat("Scaled:", isTRUE(settings$scale), "\n")
      cat(
        "Computation:",
        if (isTRUE(settings$SPSS)) "full W / correlation mode" else "compact exact design PCA",
        "\n"
      )
    })

    output$pca_eigen <- renderPrint({
      pr <- pca_unrotated()
      req(pr)
      print(round(pr$eigenvalues, 4))
    })

    output$pca_var <- renderPrint({
      pr <- pca_unrotated()
      req(pr)

      k <- length(pr$var_expl)

      out <- data.frame(
        Component = paste0("PC", seq_len(k)),
        Variance_Explained_Percent = round(pr$var_expl[seq_len(k)], 2),
        Cumulative_Percent = round(cumsum(pr$var_expl[seq_len(k)]), 2)
      )

      print(out, row.names = FALSE)
    })

    # ---------- scree plot ----------
    scree_plot_obj <- reactive({
      pr <- pca_unrotated()
      req(pr)

      k <- length(pr$eigenvalues)

      df <- data.frame(
        Component = seq_len(k),
        PC = factor(paste0("PC", seq_len(k)),
                    levels = paste0("PC", seq_len(k))),
        Eigenvalue = pr$eigenvalues[seq_len(k)],
        Variance = pr$var_expl[seq_len(k)]
      )

      scale_fac <- max(df$Variance, na.rm = TRUE) /
        max(df$Eigenvalue, na.rm = TRUE)

      ggplot2::ggplot(df, ggplot2::aes(x = PC)) +
        ggplot2::geom_col(
          ggplot2::aes(y = Variance),
          fill = "#404F69",
          alpha = 0.85
        ) +
        ggplot2::geom_line(
          ggplot2::aes(y = Eigenvalue * scale_fac, group = 1),
          colour = "#B22222",
          linewidth = 1.2
        ) +
        ggplot2::geom_point(
          ggplot2::aes(y = Eigenvalue * scale_fac),
          colour = "#B22222",
          size = 3
        ) +
        ggplot2::labs(
          x = "Component",
          y = "Explained variance (%)",
          title = "Explained Variance and Scree Plot"
        ) +
        ggplot2::scale_y_continuous(
          sec.axis = ggplot2::sec_axis(
            ~ . / scale_fac,
            name = "Eigenvalue"
          )
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(size = 14, face = "bold"),
          axis.text.y = ggplot2::element_text(size = 14, face = "bold"),
          axis.title.x = ggplot2::element_text(size = 16, face = "bold"),
          axis.title.y = ggplot2::element_text(size = 16, face = "bold"),
          plot.title = ggplot2::element_text(size = 18, face = "bold")
        )
    })

    output$scree_plot <- renderPlot({
      scree_plot_obj()
    })

    # ---------- respondent IDs and covariates for PCA display tables ----------
    # The package-level PCA functions do not require an explicit ID column.
    # Reproduce the same ID rules here so uploaded datasets without `ID` can
    # still use the loadings and W display tools.
    pca_display_ids <- function(df) {
      if ("ID" %in% colnames(df)) {
        ids <- as.character(df$ID)
        validate(need(!anyNA(ids) && all(nzchar(ids)),
                      "The ID column contains missing or empty values."))
        validate(need(!anyDuplicated(ids),
                      "The ID column must contain unique values."))
        return(ids)
      }

      row_ids <- rownames(df)
      if (!is.null(row_ids) &&
          length(row_ids) == nrow(df) &&
          !identical(row_ids, as.character(seq_len(nrow(df)))) &&
          !anyDuplicated(row_ids)) {
        return(as.character(row_ids))
      }

      width <- nchar(as.character(nrow(df)))
      sprintf(paste0("R%0", width, "d"), seq_len(nrow(df)))
    }

    covariates_for_tables <- reactive({
      # Loadings correspond to the exact respondent set entering PCA, after
      # any Dummies-tab respondent filters have been applied.
      df <- pca_input_data()
      covs <- setdiff(pca_covariate_cols(), "ID")
      req(df)

      covs <- intersect(covs, colnames(df))
      out <- df[, covs, drop = FALSE]
      out$.gqr_id <- pca_display_ids(df)
      out[, c(".gqr_id", covs), drop = FALSE]
    })

    append_covariates_to_loadings <- function(L_df) {
      cov_df <- covariates_for_tables()
      if (is.null(cov_df)) return(L_df)

      L_df$Variable <- as.character(L_df$Variable)
      dplyr::left_join(L_df, cov_df, by = c("Variable" = ".gqr_id"))
    }


    # ---------- loadings data frames ----------
    loadings_unrot_df <- reactive({
      pr <- pca_unrotated()
      req(pr)

      L <- as.matrix(pr$loadings_rot)
      k <- ncol(L)
      L <- L[, seq_len(k), drop = FALSE]

      df <- as.data.frame(L)
      df <- tibble::rownames_to_column(df, "Variable")

      append_covariates_to_loadings(df)
    })

    loadings_rot_df <- reactive({
      if (!identical(pca_settings()$rotation, "varimax")) {
        return(loadings_unrot_df())
      }

      pr <- pca_selected()
      req(pr)

      L <- as.matrix(pr$loadings_rot)
      k <- ncol(L)
      L <- L[, seq_len(k), drop = FALSE]

      df <- as.data.frame(L)
      df <- tibble::rownames_to_column(df, "Variable")

      append_covariates_to_loadings(df)
    })

    # ---------- DT tables for loadings ----------
    # Pagination only; no PCA effect.
    make_loadings_dt <- function(df) {
      num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
      pc_cols <- grep("^PC[0-9]+$", num_cols, value = TRUE)

      dt <- DT::datatable(
        df,
        filter = "top",
        rownames = FALSE,
        options = list(
          paging = TRUE,
          pageLength = 1000,
          lengthMenu = list(c(100, 500, 1000, -1), c("100", "500", "1000", "All")),
          scrollX = TRUE,
          autoWidth = TRUE,
          searching = TRUE,
          ordering = TRUE,
          dom = "ftip"
        )
      )

      pal <- rev(RColorBrewer::brewer.pal(11, "Spectral"))

      for (col in pc_cols) {
        rng <- range(df[[col]], na.rm = TRUE)

        if (is.finite(rng[1]) && is.finite(rng[2]) && rng[1] < rng[2]) {
          cuts <- seq(rng[1], rng[2], length.out = length(pal) + 1)

          dt <- dt |>
            DT::formatRound(columns = col, digits = 3) |>
            DT::formatStyle(
              columns = col,
              backgroundColor = DT::styleInterval(
                cuts[-c(1, length(cuts))],
                pal
              )
            )
        } else {
          dt <- dt |>
            DT::formatRound(columns = col, digits = 3)
        }
      }

      dt
    }




    output$unrotated_loadings_table <- DT::renderDT({
      df <- loadings_unrot_df()
      req(df)
      make_loadings_dt(df)
    }, server = FALSE)

    output$rotated_loadings_table <- DT::renderDT({
      df <- loadings_rot_df()
      req(df)
      make_loadings_dt(df)
    }, server = FALSE)


    observeEvent(input$show_unrotated_loadings, {
      showModal(
        modalDialog(
          title = "Unrotated PCA loadings",
          size = "l",
          easyClose = TRUE,
          footer = modalButton("Close"),
          DT::DTOutput(ns("unrotated_loadings_table")),
          class = "pca-modal"
        )
      )
    })

    observeEvent(input$show_rotated_loadings, {
      showModal(
        modalDialog(
          title = if (identical(pca_settings()$rotation, "varimax")) {
            "Rotated PCA loadings"
          } else {
            "PCA loadings (no rotation applied)"
          },
          size = "l",
          easyClose = TRUE,
          footer = modalButton("Close"),
          DT::DTOutput(ns("rotated_loadings_table")),
          class = "pca-modal"
        )
      )
    })

    # ---------- downloads for scree ----------
    output$download_scree_png <- downloadHandler(
      filename = function() {
        paste0("pca_variance_scree_", pca_settings()$rotation, ".png")
      },
      content = function(file) {
        ggplot2::ggsave(
          filename = file,
          plot = scree_plot_obj(),
          device = "png",
          width = 10,
          height = 6,
          units = "in",
          dpi = 300
        )
      }
    )

    output$download_scree_pdf <- downloadHandler(
      filename = function() {
        paste0("pca_variance_scree_", pca_settings()$rotation, ".pdf")
      },
      content = function(file) {
        ggplot2::ggsave(
          filename = file,
          plot = scree_plot_obj(),
          device = "pdf",
          width = 10,
          height = 6,
          units = "in"
        )
      }
    )

    # ---------- W with scores ----------
    # PCA regressions only need the component scores, so keep this object small.
    # A W preview is materialised only when the user opens the table.
    scored_W <- reactive({
      pr <- pca_value()
      req(pr)
      out <- as.data.frame(pr$scores, check.names = FALSE)
      out$Combination <- rownames(pr$scores)
      out[, c("Combination", setdiff(names(out), "Combination")), drop = FALSE]
    })

    scored_W_covariates <- reactive({
      df <- pca_input_data()
      covs <- setdiff(pca_covariate_cols(), "ID")
      req(df)

      covs <- intersect(covs, colnames(df))
      out <- df[, covs, drop = FALSE]
      out$ID <- pca_display_ids(df)
      out[, c("ID", covs), drop = FALSE]
    })

    filtered_scored_W_ids <- reactive({
      cov_df <- scored_W_covariates()
      req(cov_df)

      keep <- rep(TRUE, nrow(cov_df))

      for (v in colnames(cov_df)) {
        val <- input[[paste0("score_filter_", v)]]
        col <- cov_df[[v]]

        if (identical(v, "ID")) {
          id_raw <- trimws(if (is.null(val)) "" else val)
          if (nzchar(id_raw)) {
            ids <- trimws(unlist(strsplit(id_raw, ",")))
            keep <- keep & cov_df$ID %in% ids
          }
        } else if (!is.null(val)) {
          if (is.character(col) || is.factor(col)) {
            keep <- keep & col %in% val
          } else if (is.numeric(col)) {
            keep <- keep & col >= val[1] & col <= val[2]
          }
        }
      }

      cov_df$ID[keep]
    })

    display_scored_W <- reactive({
      pr <- pca_value()
      D <- dummies_state$D()
      df <- pca_input_data()
      ids <- filtered_scored_W_ids()
      req(pr, D, df, ids)

      ids <- intersect(ids, pr$respondents_used)
      mode <- input$score_sample_mode %||% "first"

      if (length(ids) > 200) {
        if (mode == "last") {
          ids <- tail(ids, 200)
        } else if (mode == "random") {
          ids <- sample(ids, 200)
        } else {
          ids <- head(ids, 200)
        }
      }

      validate(need(length(ids) > 0, "No respondents match the display filters."))

      source_ids <- pca_display_ids(df)
      keep <- match(ids, source_ids)
      keep <- keep[!is.na(keep)]
      df_sub <- df[keep, , drop = FALSE]
      selected_ids <- source_ids[keep]

      # Give the temporary W preview an explicit internal identifier. This
      # preserves the PCA respondent names even when the uploaded dataset had
      # no ID column and only a subset of respondents is displayed.
      internal_id <- ".gqr_display_id"
      while (internal_id %in% names(df_sub)) {
        internal_id <- paste0(internal_id, "_")
      }
      df_sub[[internal_id]] <- selected_ids

      # Keep the interactive table bounded. The PCA itself still uses every
      # combination.
      row_limit <- min(nrow(D), 10000L)
      rows <- seq_len(row_limit)

      W_display <- GQR::gqr_make_w(
        data = df_sub,
        analysis_cols = colnames(D),
        D = D,
        id_col = internal_id,
        rows = rows,
        algorithm = "matmul"
      )

      scores <- as.data.frame(pr$scores[rows, , drop = FALSE], check.names = FALSE)
      cbind(as.data.frame(W_display, check.names = FALSE), scores)
    })


    output$scored_W_filters <- renderUI({
      cov_df <- scored_W_covariates()

      if (is.null(cov_df)) {
        return(
          tagList(
            helpText("No covariate filters; define covariates on the Data tab if needed."),
            hr(),
            radioButtons(
              ns("score_sample_mode"),
              "Which respondents to show (max. 200):",
              choices = c("First 200" = "first", "Last 200" = "last", "Random 200" = "random"),
              selected = "first",
              inline = TRUE
            )
          )
        )
      }

      cov_names <- colnames(cov_df)

      controls <- lapply(cov_names, function(v) {
        col <- cov_df[[v]]
        id <- ns(paste0("score_filter_", v))

        if (identical(v, "ID")) {
          textInput(
            inputId = id,
            label = "Filter by IDs (comma-separated, optional)",
            value = ""
          )
        } else if (is.character(col) || is.factor(col)) {
          vals <- sort(unique(col))
          selectInput(
            inputId = id,
            label = v,
            choices = vals,
            selected = vals,
            multiple = TRUE
          )
        } else if (is.numeric(col)) {
          rng <- range(col, na.rm = TRUE)
          sliderInput(
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

      tagList(
        controls,
        hr(),
        radioButtons(
          ns("score_sample_mode"),
          "Which respondents to show (max. 200):",
          choices = c("First 200" = "first", "Last 200" = "last", "Random 200" = "random"),
          selected = "first",
          inline = TRUE
        )
      )
    })

    output$scored_W_table <- DT::renderDT({
      df <- display_scored_W()
      req(df)

      dt <- DT::datatable(
        df,
        filter = "top",
        rownames = TRUE,
        extensions = c("Buttons"),
        options = list(
          dom = "Bfrtip",
          buttons = c("copy", "csv", "excel"),
          paging = TRUE,
          pageLength = 1000,
          lengthMenu = c(20, 50, 100, 200),
          scrollX = TRUE,
          scrollY = "60vh",
          scrollCollapse = TRUE
        )
      )

      num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
      pal <- rev(RColorBrewer::brewer.pal(11, "Spectral"))

      for (col in num_cols) {
        rng <- range(df[[col]], na.rm = TRUE)

        if (is.finite(rng[1]) && is.finite(rng[2]) && rng[1] < rng[2]) {
          cuts <- seq(rng[1], rng[2], length.out = length(pal) + 1)

          dt <- dt |>
            DT::formatRound(columns = col, digits = 3) |>
            DT::formatStyle(
              columns = col,
              backgroundColor = DT::styleInterval(
                cuts[-c(1, length(cuts))],
                pal
              )
            )
        } else {
          dt <- dt |>
            DT::formatRound(columns = col, digits = 3)
        }
      }

      dt
    }, server = TRUE)

    observeEvent(input$show_scored_W, {
      showModal(
        modalDialog(
          title = "W matrix with PCA scores (display only; max. 200 respondents and 10,000 combinations shown)",
          size = "l",
          easyClose = TRUE,
          footer = modalButton("Close"),
          fluidRow(
            column(3, uiOutput(ns("scored_W_filters"))),
            column(9, DT::DTOutput(ns("scored_W_table")))
          ),
          class = "pca-modal"
        )
      )
    })

    scores_reactive <- reactive({
      pr <- pca_selected()
      req(pr)
      as.data.frame(pr$scores)
    })

    # ---------- expose state to Component–Covariate Regression tab ----------
    # Component–Covariate Regression remains independent of the PCA-table filters.
    list(
      # Keep the full package result available so downstream GUI modules can
      # delegate analysis to the same exported functions used in scripts.
      result = pca_value,
      scores = scores_reactive,
      scored_W = scored_W,
      loadings_unrot = reactive(as.data.frame(pca_unrotated()$loadings_rot)),
      loadings_rot = reactive(as.data.frame(pca_selected()$loadings_rot)),
      rotation = reactive({ req(pca_settings()); pca_settings()$rotation }),
      settings = reactive(pca_settings())
    )
  })
}
