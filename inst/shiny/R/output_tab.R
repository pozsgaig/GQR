# src/output_tab.R

outputTabUI <- function(id) {
  ns <- NS(id)

  tabPanel(
    "Output",
    div(
      class = "q-container",
      h2("Component–covariate relationships"),
      p("Regression and visualisation of respondent loadings on selected covariates."),

      h4("Respondent filters"),
      div(
        class = "panel panel-primary",
        div(
          class = "panel-heading",
          tags$div(
            class = "q-panel-header",
            `data-toggle` = "collapse",
            `data-target` = paste0("#", ns("sec_filters_out")),
            HTML(" Respondent filters (click to expand/collapse)")
          )
        ),
        div(
          id = ns("sec_filters_out"),
          class = "panel-collapse collapse",
          div(
            class = "panel-body q-panel",
            fluidRow(
              column(
                6,
                uiOutput(ns("covariate_filters_out_ui"))
              ),
              column(
                6,
                h4("Filter by IDs"),
                textInput(
                  ns("id_filter_out"),
                  "Comma-separated list of IDs to keep (optional)",
                  value = ""
                ),
                helpText(
                  "If non-empty, only respondents whose IDs are in this list ",
                  "will be retained (after applying covariate filters)."
                ),
                sliderInput(
                  ns("max_respondents_out"),
                  "Maximum number of respondents:",
                  min = 1,
                  max = 1,
                  value = 1,
                  step = 1
                )
              )
            )
          )
        )
      ),

      br(),

      fluidRow(
        column(
          4,
          uiOutput(ns("component_select_ui")),
          uiOutput(ns("covariate_select_ui")),
          actionButton(ns("run_models"), "Run models")
        )
      ),

      br(),

      h4("Respondent loading map"),
      uiOutput(ns("pc_scatter_controls")),
      plotOutput(ns("pc_scatter_plot"), height = "500px"),

      br(),

      h4("Regression results"),
      tableOutput(ns("reg_table")),

      br(),
      h4("Individual regression summary"),
      uiOutput(ns("reg_summary_component_ui")),
      verbatimTextOutput(ns("reg_summary")),

      br(),

      h4("Component loadings by covariate"),
      uiOutput(ns("plot_controls_ui")),
      plotOutput(ns("cov_plot"), height = "420px")
    )
  )
}

outputTabServer <- function(id, data_state, pca_state, dummies_state = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    `%||%` <- function(x, y) {
      if (is.null(x)) y else x
    }

    output_snapshot <- reactive({
      if (!is.null(dummies_state) && !is.null(dummies_state$snapshot)) {
        dummies_state$snapshot()
      } else {
        NULL
      }
    })

    output_data_trans <- reactive({
      s <- output_snapshot()
      if (!is.null(s)) {
        s$data_trans
      } else {
        data_state$data_trans()
      }
    })

    output_covariate_cols <- reactive({
      s <- output_snapshot()
      if (!is.null(s)) {
        s$covariate_cols %||% character(0)
      } else {
        data_state$covariate_cols() %||% character(0)
      }
    })

    current_loadings <- reactive({
      L <- if (identical(pca_state$rotation(), "varimax")) {
        pca_state$loadings_rot()
      } else {
        pca_state$loadings_unrot()
      }

      req(L)

      L_df <- as.data.frame(L)
      validate(need(nrow(L_df) > 0, "No respondent loadings available."))

      respondent_ids <- rownames(L_df)
      if (is.null(respondent_ids) || length(respondent_ids) != nrow(L_df)) {
        source_data <- output_data_trans()
        if (!is.null(source_data) &&
            "ID" %in% names(source_data) &&
            nrow(source_data) == nrow(L_df)) {
          respondent_ids <- as.character(source_data$ID)
        } else {
          respondent_ids <- paste0("R", seq_len(nrow(L_df)))
        }
      }
      L_df$ID <- as.character(respondent_ids)

      pc_cols <- grep("^PC[0-9]+$", colnames(L_df), value = TRUE)
      validate(need(length(pc_cols) > 0, "No component loadings available."))

      L_df[, c("ID", pc_cols), drop = FALSE]
    })

    respondent_metadata <- reactive({
      df <- output_data_trans()
      covs <- output_covariate_cols()
      req(df)

      validate(need("ID" %in% colnames(df), "An ID column is required for the Output tab."))

      covs <- setdiff(covs %||% character(0), "ID")
      keep_cols <- intersect(c("ID", covs), colnames(df))

      out <- df[, keep_cols, drop = FALSE]
      out$ID <- as.character(out$ID)

      out |>
        dplyr::distinct(.data$ID, .keep_all = TRUE)
    })

    respondent_loadings_all <- reactive({
      L_df <- current_loadings()
      meta_df <- respondent_metadata()
      req(L_df, meta_df)

      dplyr::left_join(L_df, meta_df, by = "ID")
    })

    component_names <- reactive({
      grep("^PC[0-9]+$", colnames(current_loadings()), value = TRUE)
    })

    all_covariate_names <- reactive({
      setdiff(colnames(respondent_loadings_all()), c("ID", component_names()))
    })

    numeric_covariate_names <- reactive({
      dat <- respondent_loadings_all()
      covs <- all_covariate_names()

      covs[vapply(dat[, covs, drop = FALSE], is.numeric, logical(1))]
    })

    categorical_covariate_names <- reactive({
      dat <- respondent_loadings_all()
      covs <- all_covariate_names()

      covs[vapply(
        dat[, covs, drop = FALSE],
        function(x) is.character(x) || is.factor(x) || is.logical(x),
        logical(1)
      )]
    })

    data_covariates_out <- reactive({
      dat <- respondent_loadings_all()
      covs <- all_covariate_names()
      dat[, covs, drop = FALSE]
    })

    output$covariate_filters_out_ui <- renderUI({
      cov_df <- data_covariates_out()
      req(cov_df)

      cov_names <- colnames(cov_df)

      if (length(cov_names) == 0) {
        return(
          helpText(
            "No covariate filters. ",
            "If you need them, select covariate columns on the Data tab."
          )
        )
      }

      controls <- lapply(cov_names, function(v) {
        col <- cov_df[[v]]
        input_id <- ns(paste0("out_filter_", v))

        if (is.character(col) || is.factor(col) || is.logical(col)) {
          vals <- sort(unique(as.character(col[!is.na(col)])))
          selectInput(
            inputId = input_id,
            label = v,
            choices = vals,
            selected = vals,
            multiple = TRUE
          )
        } else if (is.numeric(col)) {
          rng <- range(col, na.rm = TRUE)

          if (!all(is.finite(rng))) {
            return(NULL)
          }

          sliderInput(
            inputId = input_id,
            label = v,
            min = rng[1],
            max = rng[2],
            value = rng,
            step = if (diff(rng) == 0) 1 else diff(rng) / 100
          )
        } else {
          NULL
        }
      })

      do.call(tagList, controls)
    })

    respondent_loadings_filtered_uncapped <- reactive({
      dat <- respondent_loadings_all()
      req(dat)

      keep <- rep(TRUE, nrow(dat))

      for (v in all_covariate_names()) {
        val <- input[[paste0("out_filter_", v)]]
        col <- dat[[v]]

        if (is.null(val)) next

        if (is.character(col) || is.factor(col) || is.logical(col)) {
          all_levels <- sort(unique(as.character(col[!is.na(col)])))
          if (length(val) == length(all_levels) && all(val %in% all_levels)) {
            next
          }
          keep <- keep & as.character(col) %in% val
        } else if (is.numeric(col)) {
          rng_all <- range(col, na.rm = TRUE)
          if (all(is.finite(rng_all)) && isTRUE(all.equal(as.numeric(val), rng_all))) {
            next
          }
          keep <- keep & col >= val[1] & col <= val[2]
        }
      }

      id_filter_raw <- trimws(input$id_filter_out %||% "")
      if (nzchar(id_filter_raw)) {
        id_vec <- trimws(unlist(strsplit(id_filter_raw, ",")))
        keep <- keep & dat$ID %in% id_vec
      }

      out <- dat[keep, , drop = FALSE]
      validate(need(nrow(out) > 0, "No respondents left after filters."))

      out
    })

    observe({
      dat <- respondent_loadings_filtered_uncapped()
      req(dat)

      n_resp <- nrow(dat)

      updateSliderInput(
        session = session,
        inputId = "max_respondents_out",
        min = 1,
        max = n_resp,
        value = n_resp,
        step = max(1L, floor(n_resp / 20))
      )
    })


    respondent_loadings_filtered <- reactive({
      dat <- respondent_loadings_filtered_uncapped()
      req(dat)

      max_resp <- input$max_respondents_out
      if (is.null(max_resp) || !is.finite(max_resp) || nrow(dat) <= max_resp) {
        return(dat)
      }

      dat[seq_len(max_resp), , drop = FALSE]
    })

    output$component_select_ui <- renderUI({
      comps <- component_names()
      req(length(comps) > 0)

      selectInput(
        ns("components"),
        "Components (dependent variables):",
        choices = comps,
        selected = comps[1:min(3, length(comps))],
        multiple = TRUE
      )
    })

    output$covariate_select_ui <- renderUI({
      covs <- all_covariate_names()

      if (length(covs) == 0L) {
        return(
          shiny::div(
            class = "alert alert-warning",
            "No selected covariates. Select covariate columns on the Data tab and click Next again."
          )
        )
      }

      selectInput(
        ns("covariates"),
        "Covariates:",
        choices = covs,
        selected = covs,
        multiple = TRUE
      )
    })

    reg_models <- eventReactive(input$run_models, {
      dat <- respondent_loadings_filtered()
      comps <- input$components
      covs <- input$covariates
      req(dat, comps, covs)
      req(length(comps) > 0, length(covs) > 0)

      valid_covs <- covs[vapply(covs, function(v) {
        x <- dat[[v]]

        if (is.numeric(x)) {
          length(unique(x[is.finite(x)])) >= 2
        } else {
          length(unique(as.character(x[!is.na(x)]))) >= 2
        }
      }, logical(1))]

      validate(
        need(length(valid_covs) > 0,
             "No selected covariates have enough variation after filtering to fit a regression model.")
      )

      fits <- lapply(comps, function(pc) {
        form <- stats::as.formula(
          paste(pc, "~", paste(valid_covs, collapse = " + "))
        )
        stats::lm(form, data = dat)
      })

      stats::setNames(fits, comps)
    })


    reg_results <- reactive({
      fits <- reg_models()
      req(fits)

      purrr::imap_dfr(fits, function(fit, pc) {
        suppressWarnings(broom::tidy(fit)) |>
          dplyr::mutate(component = pc)
      })
    })


    output$reg_summary_component_ui <- renderUI({
      if (length(all_covariate_names()) == 0L) {
        return(NULL)
      }

      fits <- reg_models()
      req(fits)

      selectInput(
        ns("reg_summary_component"),
        "Component summary to display:",
        choices = names(fits),
        selected = names(fits)[1]
      )
    })

    output$reg_summary <- renderPrint({
      if (length(all_covariate_names()) == 0L) {
        cat("No selected covariates. Select covariate columns on the Data tab and click Next again.\n")
        return(invisible(NULL))
      }

      fits <- reg_models()
      pc <- input$reg_summary_component
      req(fits, pc)

      model <- fits[[pc]]
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


    output$reg_table <- renderTable({
      if (length(all_covariate_names()) == 0L) {
        return(NULL)
      }

      res <- reg_results()
      req(res)

      res |>
        dplyr::filter(.data$term != "(Intercept)") |>
        dplyr::select(.data$component, .data$term, .data$estimate,
                      .data$std.error, .data$statistic, .data$p.value)
    }, digits = 3)

    output$plot_controls_ui <- renderUI({
      dat <- respondent_loadings_all()
      comps <- component_names()
      covs <- all_covariate_names()
      req(dat)

      if (length(covs) == 0L) {
        return(
          shiny::div(
            class = "alert alert-warning",
            "No selected covariates. Select covariate columns on the Data tab and click Next again."
          )
        )
      }

      req(length(comps) > 0)

      tagList(
        selectInput(
          ns("plot_component"),
          "Component to plot:",
          choices = comps,
          selected = comps[1]
        ),
        selectInput(
          ns("plot_covariate"),
          "Covariate for grouping:",
          choices = covs,
          selected = covs[1]
        )
      )
    })

    output$cov_plot <- renderPlot({
      if (length(all_covariate_names()) == 0L) {
        return(invisible(NULL))
      }

      dat <- respondent_loadings_filtered()
      dat_all <- respondent_loadings_all()
      req(dat, dat_all)

      pc <- input$plot_component
      cov <- input$plot_covariate
      req(pc, cov)

      if (is.numeric(dat[[cov]])) {
        ggplot2::ggplot(
          dat,
          ggplot2::aes(x = .data[[cov]], y = .data[[pc]])
        ) +
          ggplot2::geom_point(alpha = 0.45, colour = "#404F69", na.rm = TRUE) +
          ggplot2::geom_smooth(
            method = "loess",
            se = FALSE,
            colour = "#B22222",
            na.rm = TRUE
          ) +
          ggplot2::theme_minimal() +
          ggplot2::labs(x = cov, y = pc)
      } else {
        full_levels <- sort(unique(as.character(dat_all[[cov]])))
        dat_plot <- dat
        dat_plot[[cov]] <- factor(as.character(dat_plot[[cov]]), levels = full_levels)

        ggplot2::ggplot(
          dat_plot,
          ggplot2::aes(x = .data[[cov]], y = .data[[pc]])
        ) +
          ggplot2::geom_boxplot(
            ggplot2::aes(fill = .data[[cov]]),
            alpha = 0.65,
            na.rm = TRUE
          ) +
          ggplot2::scale_x_discrete(drop = FALSE) +
          ggplot2::scale_fill_viridis_d(option = "D", end = 0.9, na.value = "grey85") +
          ggplot2::theme_minimal(base_size = 18) +
          ggplot2::theme(
            axis.title = ggplot2::element_text(size = 20, face = "bold"),
            axis.text = ggplot2::element_text(size = 17),
            legend.title = ggplot2::element_text(size = 18, face = "bold"),
            legend.text = ggplot2::element_text(size = 16),
            axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
          ) +
          ggplot2::labs(x = cov, y = pc)
      }
    })

    output$pc_scatter_controls <- renderUI({
      comps <- component_names()
      all_covs <- all_covariate_names()
      num_covs <- numeric_covariate_names()
      cat_covs <- categorical_covariate_names()
      req(length(comps) > 0)

      tagList(
        fluidRow(
          column(
            4,
            selectInput(
              ns("sc_xcomp"),
              "X-axis component:",
              choices = comps,
              selected = comps[1]
            )
          ),
          column(
            4,
            selectInput(
              ns("sc_ycomp"),
              "Y-axis component:",
              choices = comps,
              selected = comps[min(2, length(comps))]
            )
          ),
          column(
            4,
            selectInput(
              ns("sc_colour"),
              "Colour by covariate:",
              choices = c("None", all_covs),
              selected = "None"
            )
          )
        ),
        fluidRow(
          column(
            4,
            selectInput(
              ns("sc_size"),
              "Point size by covariate:",
              choices = c("None", all_covs),
              selected = "None"
            )
          ),
          column(
            4,
            selectInput(
              ns("sc_hull"),
              "Convex hull group:",
              choices = c("None", cat_covs),
              selected = "None"
            )
          ),
          column(
            4,
            selectInput(
              ns("sc_ellipse"),
              "Ellipse group:",
              choices = c("None", cat_covs),
              selected = "None"
            )
          )
        ),
        fluidRow(
          column(
            4,
            checkboxInput(
              ns("sc_show_arrows"),
              "Show selected loadings as arrows",
              value = FALSE
            )
          ),
          column(
            8,
            uiOutput(ns("sc_arrow_vars_ui"))
          )
        ),
        fluidRow(
          column(
            6,
            checkboxGroupInput(
              ns("sc_ell_levels"),
              "Ellipse confidence levels:",
              choices = c("95%" = 0.95, "75%" = 0.75),
              selected = 0.95
            )
          )
        )
      )
    })

    output$sc_arrow_vars_ui <- renderUI({
      vars <- numeric_covariate_names()
      req(length(vars) > 0)

      if (!isTRUE(input$sc_show_arrows)) {
        return(NULL)
      }

      selectizeInput(
        ns("sc_arrow_vars"),
        "Numeric covariates to show as arrows:",
        choices = vars,
        selected = head(vars, min(5, length(vars))),
        multiple = TRUE,
        options = list(
          plugins = list("remove_button")
        )
      )
    })


    make_hull_df <- function(dat, xvar, yvar, groupvar) {
      dat2 <- dat |>
        dplyr::filter(
          !is.na(.data[[groupvar]]),
          is.finite(.data[[xvar]]),
          is.finite(.data[[yvar]])
        )

      if (nrow(dat2) < 3) {
        return(NULL)
      }

      split_dat <- split(dat2, as.character(dat2[[groupvar]]))

      hull_list <- lapply(names(split_dat), function(g) {
        gdf <- split_dat[[g]]

        if (nrow(gdf) < 3) {
          return(NULL)
        }

        uniq_n <- nrow(unique(gdf[, c(xvar, yvar), drop = FALSE]))
        if (uniq_n < 3) {
          return(NULL)
        }

        h <- chull(gdf[[xvar]], gdf[[yvar]])
        out <- gdf[h, c(groupvar, xvar, yvar), drop = FALSE]
        out$.group <- g
        out
      })

      hull_list <- hull_list[!vapply(hull_list, is.null, logical(1))]

      if (length(hull_list) == 0) {
        return(NULL)
      }

      dplyr::bind_rows(hull_list)
    }

    output$pc_scatter_plot <- renderPlot({
      dat <- respondent_loadings_filtered()
      dat_all <- respondent_loadings_all()
      req(dat, dat_all)

      xcomp <- input$sc_xcomp
      ycomp <- input$sc_ycomp
      colvar <- input$sc_colour
      sizevar <- input$sc_size
      hullvar <- input$sc_hull
      ellvar <- input$sc_ellipse
      ell_lvls <- suppressWarnings(as.numeric(input$sc_ell_levels))

      req(xcomp, ycomp)

      dat_plot <- dat |>
        dplyr::filter(
          is.finite(.data[[xcomp]]),
          is.finite(.data[[ycomp]])
        )

      validate(need(nrow(dat_plot) > 0, "No respondents available for this plot."))

      drop_bright_cols <- function(cols) {
        rgb <- grDevices::col2rgb(cols)
        lum <- 0.2126 * rgb[1, ] + 0.7152 * rgb[2, ] + 0.0722 * rgb[3, ]
        keep <- lum < 235 & !(rgb[1, ] > 245 & rgb[2, ] > 245 & rgb[3, ] > 245)
        cols[keep]
      }

      plot_pal <- function(n, var_key = "", seed = 1) {
        drop_bright_cols <- function(cols) {
          rgb <- grDevices::col2rgb(cols)
          lum <- 0.2126 * rgb[1, ] + 0.7152 * rgb[2, ] + 0.0722 * rgb[3, ]
          keep <- lum < 225 & !(rgb[1, ] > 240 & rgb[2, ] > 240 & rgb[3, ] > 240)
          unname(cols[keep])
        }

        base_pool <- unique(c(
          Polychrome::glasbey.colors(32),
          Polychrome::palette36.colors(36)
        ))
        base_pool <- drop_bright_cols(base_pool)

        if (n > length(base_pool)) {
          if (!is.null(seed)) set.seed(seed)
          extra_cols <- drop_bright_cols(
            Polychrome::createPalette(
              N = n - length(base_pool) + 20,
              seedcolors = c("#2A3B8E", "#E85D04", "#1B9E77"),
              range = c(20, 75)
            )
          )
          base_pool <- c(base_pool, extra_cols)
        }

        key_sum <- sum(utf8ToInt(var_key))
        start <- (key_sum %% length(base_pool)) + 1L
        pal <- c(base_pool[start:length(base_pool)], base_pool[seq_len(start - 1L)])

        pal[seq_len(n)]
      }

      get_group_cols <- function(var, levels_now, seed = 1) {
        key <- as.character(var)

        if (is.null(colour_cache[[key]])) {
          all_levels <- sort(unique(as.character(dat_all[[var]])))
          all_levels <- all_levels[!is.na(all_levels)]

          colour_cache[[key]] <<- stats::setNames(
            plot_pal(length(all_levels), var_key = key, seed = seed),
            all_levels
          )
        }

        full_map <- colour_cache[[key]]
        stats::setNames(full_map[levels_now], levels_now)
      }

      colour_cache <- list()

      size_breaks <- ggplot2::waiver()
      size_labels <- ggplot2::waiver()

      if (!is.null(sizevar) && !identical(sizevar, "None")) {
        if (is.numeric(dat_plot[[sizevar]])) {
          dat_plot$.size_map <- dat_plot[[sizevar]]
        } else {
          levs <- sort(unique(as.character(dat_plot[[sizevar]])))
          dat_plot$.size_group <- factor(as.character(dat_plot[[sizevar]]), levels = levs)
          dat_plot$.size_map <- as.numeric(dat_plot$.size_group)
          size_breaks <- seq_along(levs)
          size_labels <- levs
        }
      }

      p <- ggplot2::ggplot(
        dat_plot,
        ggplot2::aes(x = .data[[xcomp]], y = .data[[ycomp]])
      )

      # 1. points ---------------------------------------------------------------
      if (!is.null(colvar) && !identical(colvar, "None")) {
        point_levels <- sort(unique(as.character(dat_plot[[colvar]])))
        point_cols <- get_group_cols(colvar, point_levels, seed = 101)

        p <- p +
          ggplot2::geom_point(
            mapping = ggplot2::aes(
              colour = .data[[colvar]],
              size = if (!is.null(sizevar) && !identical(sizevar, "None")) .data$.size_map else NULL
            ),
            alpha = 0.85,
            na.rm = TRUE,
            key_glyph = "point"
          ) +
          ggplot2::scale_colour_manual(
            name = colvar,
            values = point_cols,
            na.value = "grey75",
            guide = ggplot2::guide_legend(
              order = 1,
              override.aes = list(
                shape = 16,
                size = 5,
                alpha = 1,
                linetype = 0,
                linewidth = 0,
                fill = NA,
                colour = unname(point_cols)
              )
            )
          )
      } else {
        p <- p +
          ggplot2::geom_point(
            mapping = ggplot2::aes(
              size = if (!is.null(sizevar) && !identical(sizevar, "None")) .data$.size_map else NULL
            ),
            colour = "#3A3A3A",
            alpha = 0.85,
            na.rm = TRUE,
            key_glyph = "point"
          )
      }

      # point size guide
      if (!is.null(sizevar) && !identical(sizevar, "None")) {
        if (is.numeric(dat_plot[[sizevar]])) {
          p <- p +
            ggplot2::scale_size_continuous(
              name = sizevar,
              guide = ggplot2::guide_legend(
                order = 2,
                override.aes = list(
                  shape = 16,
                  colour = "#4D4D4D",
                  fill = NA,
                  alpha = 1,
                  linetype = 0,
                  linewidth = 0
                )
              )
            )
        } else {
          p <- p +
            ggplot2::scale_size_continuous(
              name = sizevar,
              breaks = size_breaks,
              labels = size_labels,
              guide = ggplot2::guide_legend(
                order = 2,
                override.aes = list(
                  shape = 16,
                  colour = "#4D4D4D",
                  fill = NA,
                  alpha = 1,
                  linetype = 0,
                  linewidth = 0
                )
              )
            )
        }
      }

      # 2. hulls ----------------------------------------------------------------
      if (!is.null(hullvar) && !identical(hullvar, "None")) {
        hull_df <- make_hull_df(dat_plot, xcomp, ycomp, hullvar)

        if (!is.null(hull_df) && nrow(hull_df) > 0) {
          hull_levels <- sort(unique(as.character(hull_df[[hullvar]])))
          hull_cols <- get_group_cols(hullvar, hull_levels, seed = 202)

          hull_key_df <- tibble::tibble(
            .key_x = NA_real_,
            .key_y = NA_real_,
            .key_group = factor(hull_levels, levels = hull_levels)
          )

          hull_border_cols <- unname(hull_cols[as.character(hull_df[[hullvar]])])

          p <- p +
            ggnewscale::new_scale_fill() +
            ggplot2::geom_polygon(
              data = hull_df,
              mapping = ggplot2::aes(
                x = .data[[xcomp]],
                y = .data[[ycomp]],
                group = .data$.group,
                fill = .data[[hullvar]]
              ),
              colour = hull_border_cols,
              alpha = 0.20,
              linewidth = 1.1,
              inherit.aes = FALSE,
              show.legend = FALSE
            ) +
            ggplot2::geom_point(
              data = hull_key_df,
              mapping = ggplot2::aes(
                x = .data$.key_x,
                y = .data$.key_y,
                fill = .data$.key_group
              ),
              inherit.aes = FALSE,
              shape = 22,
              size = 5.5,
              stroke = 1.1,
              colour = NA,
              alpha = 1,
              na.rm = TRUE,
              show.legend = TRUE
            ) +
            ggplot2::scale_fill_manual(
              name = paste(hullvar, "(hull)"),
              values = stats::setNames(
                scales::alpha(unname(hull_cols), 0.20),
                names(hull_cols)
              ),
              limits = hull_levels,
              drop = FALSE,
              guide = ggplot2::guide_legend(
                order = 3,
                override.aes = list(
                  shape = 22,
                  size = 5.5,
                  stroke = 1.1,
                  colour = unname(hull_cols),
                  fill = unname(scales::alpha(hull_cols, 0.20)),
                  alpha = 1,
                  linetype = 0,
                  linewidth = 0
                )
              )
            )
        }
      }
      # 3. ellipses -------------------------------------------------------------
      if (!is.null(ellvar) && !identical(ellvar, "None") && length(ell_lvls) > 0) {
        dat_ell <- dat_plot |>
          dplyr::filter(!is.na(.data[[ellvar]]))

        valid_groups <- dat_ell |>
          dplyr::count(.data[[ellvar]], name = "n") |>
          dplyr::filter(.data$n >= 3) |>
          dplyr::pull(1)

        dat_ell <- dat_ell |>
          dplyr::filter(.data[[ellvar]] %in% valid_groups)

        if (nrow(dat_ell) >= 3 && length(valid_groups) > 0) {
          ell_lvls <- ell_lvls[is.finite(ell_lvls)]

          if (length(ell_lvls) > 0) {
            ell_levels <- sort(unique(as.character(dat_ell[[ellvar]])))
            ell_cols <- get_group_cols(ellvar, ell_levels, seed = 303)

            p <- p + ggnewscale::new_scale_colour()

            for (lev in ell_lvls) {
              p <- p +
                ggplot2::stat_ellipse(
                  data = dat_ell,
                  mapping = ggplot2::aes(
                    x = .data[[xcomp]],
                    y = .data[[ycomp]],
                    colour = .data[[ellvar]],
                    group = .data[[ellvar]]
                  ),
                  level = lev,
                  linetype = if (lev == max(ell_lvls)) "solid" else "dashed",
                  linewidth = 1.1,
                  inherit.aes = FALSE,
                  show.legend = (lev == max(ell_lvls)),
                  key_glyph = "path"
                )
            }

            p <- p +
              ggplot2::scale_colour_manual(
                name = paste(ellvar, "(ellipse)"),
                values = ell_cols,
                na.value = "grey75",
                guide = ggplot2::guide_legend(
                  order = 4,
                  override.aes = list(
                    shape = NA,
                    fill = NA,
                    linewidth = 1.2,
                    linetype = 1,
                    alpha = 1
                  )
                )
              )
          }
        }
      }

      # 4. arrows ---------------------------------------------------------------
      if (isTRUE(input$sc_show_arrows)) {
        arrow_vars <- input$sc_arrow_vars %||% character(0)

        if (length(arrow_vars) > 0) {
          arrow_df <- tibble::tibble(
            Variable = arrow_vars,
            xend = vapply(
              arrow_vars,
              function(v) stats::cor(dat_plot[[v]], dat_plot[[xcomp]], use = "pairwise.complete.obs"),
              numeric(1)
            ),
            yend = vapply(
              arrow_vars,
              function(v) stats::cor(dat_plot[[v]], dat_plot[[ycomp]], use = "pairwise.complete.obs"),
              numeric(1)
            )
          ) |>
            dplyr::filter(
              is.finite(.data$xend),
              is.finite(.data$yend)
            )

          if (nrow(arrow_df) > 0) {
            x_scale <- max(abs(dat_plot[[xcomp]]), na.rm = TRUE)
            y_scale <- max(abs(dat_plot[[ycomp]]), na.rm = TRUE)
            ax_scale <- max(abs(arrow_df$xend), na.rm = TRUE)
            ay_scale <- max(abs(arrow_df$yend), na.rm = TRUE)

            scale_fac <- 0.8 * min(
              if (is.finite(ax_scale) && ax_scale > 0) x_scale / ax_scale else 1,
              if (is.finite(ay_scale) && ay_scale > 0) y_scale / ay_scale else 1
            )

            arrow_df <- arrow_df |>
              dplyr::mutate(
                xend = .data$xend * scale_fac,
                yend = .data$yend * scale_fac
              )

            p <- p +
              ggplot2::geom_segment(
                data = arrow_df,
                ggplot2::aes(
                  x = 0,
                  y = 0,
                  xend = .data$xend,
                  yend = .data$yend
                ),
                arrow = ggplot2::arrow(length = grid::unit(0.02, "npc")),
                colour = "#B22222",
                linewidth = 0.7,
                inherit.aes = FALSE,
                show.legend = FALSE
              ) +
              ggplot2::geom_text(
                data = arrow_df,
                ggplot2::aes(
                  x = .data$xend,
                  y = .data$yend,
                  label = .data$Variable
                ),
                colour = "#B22222",
                hjust = 0.5,
                vjust = -0.35,
                size = 3.4,
                inherit.aes = FALSE,
                show.legend = FALSE
              )
          }
        }
      }

      p +
        ggplot2::labs(x = xcomp, y = ycomp) +
        ggplot2::theme_minimal(base_size = 18) +
        ggplot2::theme(
          axis.title = ggplot2::element_text(size = 20, face = "bold"),
          axis.text = ggplot2::element_text(size = 17),
          legend.title = ggplot2::element_text(size = 18, face = "bold"),
          legend.text = ggplot2::element_text(size = 16),
          legend.key.width = grid::unit(1.1, "lines"),
          legend.key.height = grid::unit(1.1, "lines")
        )
    })
  })
}
