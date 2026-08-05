PCAregressionsTabUI <- function(id) {
  ns <- NS(id)

  tabPanel(
    "PCA Regressions",

    tags$head(
      tags$style(HTML("
        .pca-reg-modal .modal-body {
          max-height: 70vh;
          overflow-y: auto;
        }
        .pca-reg-modal .modal-dialog {
          width: 85vw;
          max-width: 85vw;
        }
      ")),
      tags$script(HTML("
        $(document).on('shown.bs.modal', '.pca-reg-modal', function () {
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
      h2("Component-Covariate Regressions"),
      p("Linear regressions of PCA component scores on the dummy variables used to generate W."),
      fluidRow(
        column(
          12,
          uiOutput(ns("component_select_ui")),
          br(),
          radioButtons(
            ns("display_mode"),
            "Display:",
            choices = c(
              "Standardised coefficients (beta)" = "beta",
              "Unstandardised coefficients" = "raw"
            ),
            selected = "beta",
            inline = TRUE
          ),
          actionButton(ns("show_coeff_popup"), "Open coefficient matrix"),
          br(), br(),
          h4("Regression Summary"),
          verbatimTextOutput(ns("reg_details")),
          br(),
          h4("Coefficient Heatmap"),
          plotOutput(ns("coeff_heatmap"), height = "700px"),
          downloadButton(ns("download_heatmap_png"), "Download heatmap PNG"),
          downloadButton(ns("download_heatmap_pdf"), "Download heatmap PDF")
        )
      )
    )
  )
}


PCAregressionsTabServer <- function(id, data_state, pca_state, dummies_state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    group_map <- reactive({
      if (!is.null(data_state$groups)) {
        data_state$groups()
      } else {
        NULL
      }
    })

    scored_W_data <- reactive({
      sc1 <- pca_state$scored_W()
      if (!is.null(sc1)) {
        return(sc1)
      }

      sc2 <- pca_state$scores()
      req(sc2)
      sc2
    })

    augmented_W <- reactive({
      D <- dummies_state$D()
      W_scored <- scored_W_data()
      req(D, W_scored)

      Ddf <- as.data.frame(D)
      Ddf$Combination <- rownames(D)

      Wdf <- as.data.frame(W_scored)
      if (!"Combination" %in% names(Wdf)) {
        Wdf$Combination <- rownames(W_scored)
      }

      dplyr::left_join(Ddf, Wdf, by = "Combination")
    })


    dummy_vars <- reactive({
      D <- dummies_state$D()
      req(D)
      colnames(D)
    })

    predictor_vars <- reactive({
      get_regression_predictors(
        dummy_vars = dummy_vars(),
        groups = group_map()
      )
    })

    pc_vars <- reactive({
      sc <- scored_W_data()
      req(sc)
      grep("^PC[0-9]", colnames(sc), value = TRUE)
    })

    output$component_select_ui <- renderUI({
      pcs <- pc_vars()
      req(length(pcs) > 0)

      selectInput(
        ns("component"),
        "PCA Component:",
        choices = pcs,
        selected = pcs[1]
      )
    })

    reg_models <- reactive({
      dat <- augmented_W()
      preds <- predictor_vars()
      pcs <- pc_vars()
      req(dat, preds, pcs)
      req(length(preds) > 0)

      stats::setNames(
        lapply(pcs, function(pc) {
          form <- stats::as.formula(
            paste(pc, "~", paste(preds, collapse = " + "))
          )
          stats::lm(form, data = dat)
        }),
        pcs
      )
    })

    beta_models <- reactive({
      dat <- augmented_W()
      preds <- predictor_vars()
      pcs <- pc_vars()
      req(dat, preds, pcs)
      req(length(preds) > 0)

      lapply(pcs, function(pc) {
        standardiseandfit(
          pc_name = pc,
          data = dat,
          dummy_vars = preds
        )
      }) |>
        stats::setNames(pcs)
    })

    output$reg_details <- renderPrint({
      pc <- input$component
      mods <- reg_models()
      req(pc, mods[[pc]])

      model <- mods[[pc]]
      residual_scale <- max(abs(stats::residuals(model)), na.rm = TRUE)
      response_scale <- max(abs(stats::model.response(stats::model.frame(model))), na.rm = TRUE)

      if (is.finite(residual_scale) &&
          residual_scale <= sqrt(.Machine$double.eps) * max(1, response_scale)) {
        cat(
          "Note: this model is essentially a perfect fit. ",
          "Coefficient standard errors and p-values may be unreliable.\n\n"
        )
      }

      print(suppressWarnings(summary(model)))
    })

    coeff_matrix <- reactive({
      preds <- predictor_vars()
      req(length(preds) > 0)

      if (identical(input$display_mode, "beta")) {
        mods <- beta_models()
        req(mods)

        out <- lapply(names(mods), function(pc) {
          suppressWarnings(broom::tidy(mods[[pc]])) |>
            dplyr::filter(.data$term != "(Intercept)") |>
            dplyr::select(term, estimate) |>
            dplyr::rename(!!pc := estimate)
        })

        mat <- Reduce(
          function(x, y) dplyr::full_join(x, y, by = "term"),
          out
        ) |>
          dplyr::rename(Variable = term) |>
          dplyr::slice(match(preds, .data$Variable)) |>
          dplyr::filter(!is.na(.data$Variable))

        return(mat)
      }

      mods <- reg_models()
      req(mods)

      out <- lapply(names(mods), function(pc) {
        suppressWarnings(broom::tidy(mods[[pc]])) |>
          dplyr::filter(.data$term != "(Intercept)") |>
          dplyr::select(term, estimate) |>
          dplyr::rename(!!pc := estimate)
      })

      Reduce(
        function(x, y) dplyr::full_join(x, y, by = "term"),
        out
      ) |>
        dplyr::rename(Variable = term) |>
        dplyr::slice(match(preds, .data$Variable)) |>
        dplyr::filter(!is.na(.data$Variable))
    })

    coeff_heatmap_plot <- reactive({
      preds <- predictor_vars()

      df_plot <- coeff_matrix() |>
        tidyr::pivot_longer(
          cols = -Variable,
          names_to = "Component",
          values_to = "Estimate"
        ) |>
        dplyr::mutate(
          Component = factor(.data$Component, levels = pc_vars()),
          Variable = factor(.data$Variable, levels = rev(preds))
        )

      ggplot2::ggplot(
        df_plot,
        ggplot2::aes(x = Component, y = Variable, fill = Estimate)
      ) +
        ggplot2::geom_tile(colour = "white") +
        ggplot2::geom_text(
          ggplot2::aes(label = sprintf("%.2f", Estimate)),
          size = 5.5,
          fontface = "bold"
        ) +
        ggplot2::scale_fill_distiller(
          palette = "Spectral",
          direction = -1
        ) +
        ggplot2::labs(
          x = "PCA Component",
          y = "Generated Variable",
          fill = if (identical(input$display_mode, "beta")) {
            "Std. beta"
          } else {
            "Coefficient"
          },
          title = if (identical(input$display_mode, "beta")) {
            "Standardised Regression Coefficients"
          } else {
            "Unstandardised Regression Coefficients"
          }
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(
            angle = 45,
            hjust = 1,
            size = 15,
            face = "bold"
          ),
          axis.text.y = ggplot2::element_text(
            size = 15,
            face = "bold"
          ),
          axis.title.x = ggplot2::element_text(
            size = 17,
            face = "bold"
          ),
          axis.title.y = ggplot2::element_text(
            size = 17,
            face = "bold"
          ),
          plot.title = ggplot2::element_text(
            size = 19,
            face = "bold"
          ),
          legend.title = ggplot2::element_text(
            size = 14,
            face = "bold"
          ),
          legend.text = ggplot2::element_text(
            size = 12,
            face = "bold"
          )
        )
    })

    output$coeff_heatmap <- renderPlot({
      coeff_heatmap_plot()
    })

    observeEvent(input$show_coeff_popup, {
      showModal(
        modalDialog(
          title = if (identical(input$display_mode, "beta")) {
            "Standardised coefficient matrix"
          } else {
            "Unstandardised coefficient matrix"
          },
          DT::DTOutput(ns("matrix_popup_table")),
          easyClose = TRUE,
          footer = modalButton("Close"),
          size = "l",
          class = "pca-reg-modal"
        )
      )
    })

    output$matrix_popup_table <- DT::renderDT({
      mat <- coeff_matrix()
      req(mat)

      DT::datatable(
        mat,
        rownames = FALSE,
        filter = "top",
        extensions = "Buttons",
        options = list(
          dom = "Bfrtip",
          buttons = c("copy", "csv", "excel"),
          paging = FALSE,
          scrollY = "60vh",
          scrollCollapse = TRUE,
          scrollX = TRUE
        )
      ) |>
        DT::formatRound(columns = 2:ncol(mat), digits = 4)
    }, server = FALSE)

    output$download_heatmap_png <- downloadHandler(
      filename = function() {
        paste0("pca_regression_heatmap_", input$display_mode, ".png")
      },
      content = function(file) {
        ggplot2::ggsave(
          filename = file,
          plot = coeff_heatmap_plot(),
          device = "png",
          width = 11,
          height = 8.5,
          units = "in",
          dpi = 300
        )
      }
    )

    output$download_heatmap_pdf <- downloadHandler(
      filename = function() {
        paste0("pca_regression_heatmap_", input$display_mode, ".pdf")
      },
      content = function(file) {
        ggplot2::ggsave(
          filename = file,
          plot = coeff_heatmap_plot(),
          device = cairo_pdf,
          width = 11,
          height = 8.5,
          units = "in"
        )
      }
    )
  })
}



