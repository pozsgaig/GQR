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
      h2("PCA on W matrix"),
      p("Principal component analysis of the Generalised Q W matrix (combinations × respondents)."),

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



    # ---------- W matrix used for PCA ----------
    # This remains independent of the PCA-table display filters.
    W_for_pca <- reactive({
      W <- NULL

      if (!is.null(dummies_state) && !is.null(dummies_state$W_filtered)) {
        W <- dummies_state$W_filtered()
      } else if (!is.null(dummies_state) && !is.null(dummies_state$W)) {
        W <- dummies_state$W()
      } else {
        W <- make_W(data_state)
      }

      req(W)

      validate(
        need(ncol(W) > 1, "Not enough respondents remain for PCA.")
      )

      W
    })


    # ---------- remove zero-variance respondents ----------
    W_clean <- reactive({
      W <- W_for_pca()
      req(W)

      vars_sd <- apply(W, 2, sd, na.rm = TRUE)
      keep <- vars_sd > 0
      W_use <- W[, keep, drop = FALSE]

      validate(
        need(ncol(W_use) > 1, "Not enough respondent variance for PCA.")
      )

      W_use
    })

    # ---------- PCA fits ----------
    pca_unrotated <- reactive({
      W_use <- W_clean()
      req(W_use)

      gqr_app_pca(
        W = W_use,
        scale. = isTRUE(input$scale),
        center = isTRUE(input$center),
        rotate = "none",
        naxis = input$n_comp,
        SPSS = isTRUE(input$SPSS),
        add_scores = TRUE,
        impute_mean = isTRUE(input$impute_mean)
      )
    })

    pca_selected <- reactive({
      W_use <- W_clean()
      req(W_use)

      gqr_app_pca(
        W = W_use,
        scale. = isTRUE(input$scale),
        center = isTRUE(input$center),
        rotate = input$rotation,
        naxis = input$n_comp,
        SPSS = isTRUE(input$SPSS),
        add_scores = TRUE,
        impute_mean = isTRUE(input$impute_mean)
      )
    })

    # ---------- textual outputs ----------
    output$pca_overview <- renderPrint({
      pr <- pca_selected()
      req(pr)

      cat("Number of components:", ncol(pr$loadings_rot), "\n")
      cat("Rotation:", input$rotation, "\n")
      cat("SPSS mode:", isTRUE(input$SPSS), "\n")
      cat("Centred:", isTRUE(input$center), "\n")
      cat("Scaled:", isTRUE(input$scale), "\n")
    })

    output$pca_eigen <- renderPrint({
      pr <- pca_unrotated()
      req(pr)
      print(round(pr$eigenvalues, 4))
    })

    output$pca_var <- renderPrint({
      pr <- pca_unrotated()
      req(pr)

      k <- min(as.integer(input$n_comp), length(pr$var_expl))

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

      k <- min(as.integer(input$n_comp), length(pr$eigenvalues))

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

    # ---------- covariates to append to loadings tables ----------
    covariates_for_tables <- reactive({
      df <- pca_data_trans()
      covs <- setdiff(pca_covariate_cols(), "ID")
      req(df)
      validate(need("ID" %in% colnames(df), "An ID column is required for loadings metadata."))

      keep_cols <- intersect(c("ID", covs), colnames(df))
      out <- df[, keep_cols, drop = FALSE]
      out$ID <- as.character(out$ID)
      out
    })

    append_covariates_to_loadings <- function(L_df) {
      cov_df <- covariates_for_tables()
      if (is.null(cov_df)) return(L_df)

      L_df$Variable <- as.character(L_df$Variable)
      dplyr::left_join(L_df, cov_df, by = c("Variable" = "ID"))
    }


    # ---------- loadings data frames ----------
    loadings_unrot_df <- reactive({
      pr <- pca_unrotated()
      req(pr)

      L <- as.matrix(pr$loadings_rot)
      k <- min(as.integer(input$n_comp), ncol(L))
      L <- L[, seq_len(k), drop = FALSE]

      df <- as.data.frame(L)
      df <- tibble::rownames_to_column(df, "Variable")

      append_covariates_to_loadings(df)
    })

    loadings_rot_df <- reactive({
      if (!identical(input$rotation, "varimax")) {
        return(loadings_unrot_df())
      }

      pr <- pca_selected()
      req(pr)

      L <- as.matrix(pr$loadings_rot)
      k <- min(as.integer(input$n_comp), ncol(L))
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
          title = if (identical(input$rotation, "varimax")) {
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
        paste0("pca_variance_scree_", input$rotation, ".png")
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
        paste0("pca_variance_scree_", input$rotation, ".pdf")
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
    # Full PCA result object; display filters do not feed back into PCA.
    scored_W <- reactive({
      pr <- pca_selected()
      req(pr)
      as.data.frame(pr$data_with_scores)
    })

    scored_W_covariates <- reactive({
      df <- pca_data_trans()
      covs <- pca_covariate_cols()
      req(df)

      keep_cols <- union("ID", covs)
      keep_cols <- intersect(keep_cols, colnames(df))
      validate(need("ID" %in% keep_cols, "An ID column is required for W-column filtering."))

      out <- df[, keep_cols, drop = FALSE]
      out$ID <- as.character(out$ID)
      out
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
      df <- scored_W()
      ids <- filtered_scored_W_ids()
      req(df, ids)

      resp_cols <- intersect(ids, colnames(df))
      pc_cols <- grep("^PC[0-9]+$", colnames(df), value = TRUE)

      mode <- input$score_sample_mode %||% "first"

      if (length(resp_cols) > 200) {
        if (mode == "last") {
          resp_cols <- tail(resp_cols, 200)
        } else if (mode == "random") {
          resp_cols <- sort(sample(resp_cols, 200))
        } else {
          resp_cols <- head(resp_cols, 200)
        }
      }

      df[, c(resp_cols, pc_cols), drop = FALSE]
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
          title = "W matrix with PCA scores (display filters only; max. 200 respondents shown)",
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

    # ---------- expose state to Output tab ----------
    # Output tab remains independent of the PCA-table filters.
    list(
      scores = scores_reactive,
      scored_W = scored_W,
      loadings_unrot = reactive(as.data.frame(pca_unrotated()$loadings_rot)),
      loadings_rot = reactive(as.data.frame(pca_selected()$loadings_rot)),
      rotation = reactive(input$rotation)
    )
  })
}
