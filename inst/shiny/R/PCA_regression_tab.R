PCAregressionsTabUI <- function(id) {
  ns <- NS(id)

  tabPanel(
    "Statement–Component Regression",

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
      h2("Statement–Component Regression"),
      p("Use the original statement dummies to identify which statements characterise each PCA component."),

      gqr_info_box(
        "How should these regressions be interpreted?",
        p(
          "For each retained PCA component, GQR regresses the component scores of the synthetic statement combinations on the dummy variables in D. The coefficients therefore show how strongly inclusion of each original statement is associated with higher or lower scores on that component."
        ),
        p(
          "In grouped one-per-group designs, one statement per group must be omitted from the fitted model as a reference category because the dummies within each group sum to one. These reference statements are retained in the heatmap and displayed as 0, as are dummy variables that are constant or all zero."
        ),
        p(
          "Standardised coefficients are useful for comparing the relative contribution of statements within and across components. The regression is intended as an aid to component interpretation and naming; it does not imply a causal relationship."
        ),
        open = TRUE
      ),

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
      if (!is.null(dummies_state) && !is.null(dummies_state$snapshot)) {
        s <- dummies_state$snapshot()
        if (!is.null(s)) return(s$groups)
      }
      if (!is.null(data_state$groups)) data_state$groups() else NULL
    })

    scored_W_data <- reactive({
      sc <- pca_state$scores()
      req(sc)
      sc
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

    pca_stub <- reactive({
      sc <- scored_W_data()
      pcs <- pc_vars()
      req(sc, pcs)

      mat <- as.matrix(sc[, pcs, drop = FALSE])
      rownames(mat) <- rownames(sc)
      list(scores = mat)
    })

    raw_regression <- reactive({
      GQR::gqr_regress_statements(
        pca = pca_stub(),
        D = dummies_state$D(),
        groups = group_map(),
        components = pc_vars(),
        standardise = FALSE
      )
    })

    beta_regression <- reactive({
      GQR::gqr_regress_statements(
        pca = pca_stub(),
        D = dummies_state$D(),
        groups = group_map(),
        components = pc_vars(),
        standardise = TRUE
      )
    })

    predictor_vars <- reactive({
      raw_regression()$predictors
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
      raw_regression()$models
    })

    beta_models <- reactive({
      beta_regression()$models
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

      omitted <- raw_regression()$baselines
      constants <- raw_regression()$constants
      if (length(omitted) > 0L) {
        cat(
          "Reference variables shown as 0 in the heatmap:",
          paste(omitted, collapse = ", "),
          "\n"
        )
      }
      if (length(constants) > 0L) {
        cat(
          "Constant/all-zero variables shown as 0 in the heatmap:",
          paste(constants, collapse = ", "),
          "\n"
        )
      }
      if (length(omitted) + length(constants) > 0L) cat("\n")

      print(suppressWarnings(summary(model)))
    })

    heatmap_layout <- reactive({
      vars <- dummy_vars()
      gm <- group_map()
      req(vars)

      if (is.null(gm) || nrow(gm) == 0L) {
        return(
          data.frame(
            Variable = vars,
            Group = NA_character_,
            stringsAsFactors = FALSE
          )
        )
      }

      gm <- gm |>
        dplyr::filter(
          !is.na(.data$group),
          !is.na(.data$variable),
          .data$variable %in% vars
        ) |>
        dplyr::distinct(.data$group, .data$variable)

      if (nrow(gm) == 0L) {
        return(
          data.frame(
            Variable = vars,
            Group = NA_character_,
            stringsAsFactors = FALSE
          )
        )
      }

      group_order <- unique(gm$group)
      gm <- gm |>
        dplyr::mutate(
          .group_order = match(.data$group, group_order),
          .variable_order = match(.data$variable, vars)
        ) |>
        dplyr::arrange(.data$.group_order, .data$.variable_order)

      grouped_vars <- gm$variable
      ungrouped_vars <- setdiff(vars, grouped_vars)

      data.frame(
        Variable = c(grouped_vars, ungrouped_vars),
        Group = c(
          as.character(gm$group),
          rep("Ungrouped", length(ungrouped_vars))
        ),
        stringsAsFactors = FALSE
      )
    })

    coeff_matrix <- reactive({
      result <- if (identical(input$display_mode, "beta")) {
        beta_regression()
      } else {
        raw_regression()
      }

      vars <- dummy_vars()
      pcs <- pc_vars()
      req(result, vars, pcs)

      long <- result$coefficients |>
        dplyr::filter(.data$term != "(Intercept)") |>
        dplyr::select(
          Component = .data$component,
          Variable = .data$term,
          Estimate = .data$estimate
        )

      wide <- tidyr::pivot_wider(
        long,
        names_from = "Component",
        values_from = "Estimate"
      )

      out <- data.frame(
        Variable = vars,
        stringsAsFactors = FALSE
      ) |>
        dplyr::left_join(wide, by = "Variable")

      for (pc in pcs) {
        if (!pc %in% names(out)) out[[pc]] <- NA_real_
      }

      zero_reference <- union(result$baselines, result$constants)
      if (length(zero_reference) > 0L) {
        for (pc in pcs) {
          out[[pc]][out$Variable %in% zero_reference] <- 0
        }
      }

      layout <- heatmap_layout()
      out |>
        dplyr::slice(match(layout$Variable, .data$Variable)) |>
        dplyr::select(Variable, dplyr::all_of(pcs))
    })

    coeff_heatmap_plot <- reactive({
      layout <- heatmap_layout()
      pcs <- pc_vars()
      req(layout, pcs)

      component_index <- stats::setNames(seq_along(pcs), pcs)

      df_plot <- coeff_matrix() |>
        tidyr::pivot_longer(
          cols = -Variable,
          names_to = "Component",
          values_to = "Estimate"
        ) |>
        dplyr::mutate(
          Component = factor(.data$Component, levels = pcs),
          ComponentIndex = unname(component_index[as.character(.data$Component)]),
          Variable = factor(
            .data$Variable,
            levels = rev(layout$Variable)
          ),
          Label = ifelse(
            is.na(.data$Estimate),
            "",
            sprintf("%.2f", .data$Estimate)
          )
        )

      # The y-axis factor is reversed so the first grouped variable is shown at
      # the top. Group boundaries are calculated in that displayed order.
      y_variables <- rev(layout$Variable)
      y_groups <- layout$Group[match(y_variables, layout$Variable)]
      transitions <- if (length(y_groups) > 1L) {
        which(y_groups[-1L] != y_groups[-length(y_groups)]) + 0.5
      } else {
        numeric(0)
      }

      label_df <- NULL
      if (any(!is.na(layout$Group))) {
        label_df <- layout |>
          dplyr::filter(!is.na(.data$Group)) |>
          dplyr::group_by(.data$Group) |>
          dplyr::summarise(
            Variable = .data$Variable[ceiling(dplyr::n() / 2)],
            .groups = "drop"
          ) |>
          dplyr::mutate(
            Variable = factor(
              .data$Variable,
              levels = rev(layout$Variable)
            ),
            # Reserve a separate blank strip to the right of the last PC.
            X = length(pcs) + 0.75
          )
      }

      x_max <- length(pcs) + if (!is.null(label_df) && nrow(label_df) > 0L) 2.2 else 0.5

      p <- ggplot2::ggplot(
        df_plot,
        ggplot2::aes(x = .data$ComponentIndex, y = .data$Variable, fill = .data$Estimate)
      ) +
        ggplot2::geom_tile(width = 1, height = 1, colour = "white") +
        ggplot2::geom_text(
          ggplot2::aes(label = .data$Label),
          size = 5.5,
          fontface = "bold"
        ) +
        ggplot2::scale_fill_distiller(
          palette = "Spectral",
          direction = -1,
          na.value = "grey90"
        ) +
        ggplot2::scale_x_continuous(
          breaks = seq_along(pcs),
          labels = pcs,
          limits = c(0.5, x_max),
          expand = c(0, 0)
        ) +
        ggplot2::labs(
          x = "PCA Component",
          y = "Original statement",
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
          legend.position = "bottom",
          legend.title = ggplot2::element_text(
            size = 14,
            face = "bold"
          ),
          legend.text = ggplot2::element_text(
            size = 12,
            face = "bold"
          ),
          plot.margin = ggplot2::margin(5.5, 15, 5.5, 5.5)
        )

      if (length(transitions) > 0L) {
        p <- p + ggplot2::geom_hline(
          yintercept = transitions,
          linewidth = 0.7,
          colour = "grey30"
        )
      }

      if (!is.null(label_df) && nrow(label_df) > 0L) {
        p <- p + ggplot2::geom_vline(
          xintercept = length(pcs) + 0.5,
          linewidth = 0.6,
          colour = "grey55"
        )

        p <- p + ggplot2::geom_text(
          data = label_df,
          ggplot2::aes(
            x = .data$X,
            y = .data$Variable,
            label = .data$Group
          ),
          inherit.aes = FALSE,
          hjust = 0,
          fontface = "bold",
          size = 4.5
        )
      }

      p
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



