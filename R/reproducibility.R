# Reproducibility helpers -------------------------------------------------

#' Record provenance for an input file
#'
#' @description
#' Creates a compact provenance record for a file used in a GQR analysis.
#' The record stores the original user-facing file name, file size, an MD5
#' checksum, and (when a data frame is supplied) its dimensions. This is used
#' by the Shiny application's reproducible-script tab, but can also be used in
#' scripted workflows.
#'
#' @param path Path to the file currently available to R. In a Shiny upload
#'   this is normally the temporary upload path.
#' @param original_name Original file name to report. For Shiny uploads this
#'   should be the browser-supplied file name rather than the temporary path.
#' @param role Short description of the file's role, such as `"dataset"` or
#'   `"groups"`.
#' @param data Optional data frame read from the file. When supplied, row and
#'   column counts are added to the provenance record.
#'
#' @return A named list of class `gqr_file_provenance`.
#'
#' @examples
#' path <- system.file("extdata", "dummy_data.RDA", package = "GQR")
#' if (nzchar(path)) {
#'   dat <- gqr_read(path)
#'   gqr_file_provenance(path, original_name = "dummy_data.RDA", data = dat)
#' }
#'
#' @export
gqr_file_provenance <- function(
    path,
    original_name = basename(path),
    role = "dataset",
    data = NULL) {

  if (length(path) != 1L || !file.exists(path)) {
    stop("`path` must identify an existing file.", call. = FALSE)
  }
  if (length(original_name) != 1L || is.na(original_name) || !nzchar(original_name)) {
    stop("`original_name` must be a non-empty string.", call. = FALSE)
  }
  if (length(role) != 1L || is.na(role) || !nzchar(role)) {
    stop("`role` must be a non-empty string.", call. = FALSE)
  }
  if (!is.null(data) && !is.data.frame(data)) {
    stop("`data` must be a data frame when supplied.", call. = FALSE)
  }

  info <- file.info(path)

  structure(
    list(
      role = as.character(role),
      original_name = as.character(original_name),
      size_bytes = as.numeric(info$size[1L]),
      md5 = unname(as.character(tools::md5sum(path)[1L])),
      n_rows = if (is.null(data)) NA_integer_ else nrow(data),
      n_cols = if (is.null(data)) NA_integer_ else ncol(data),
      recorded_at = Sys.time()
    ),
    class = "gqr_file_provenance"
  )
}

.gqr_script_quote <- function(x) {
  if (length(x) == 0L) return("character(0)")
  x <- as.character(x)
  vapply(
    x,
    function(value) {
      if (is.na(value)) "NA_character_" else encodeString(value, quote = '"')
    },
    character(1)
  )
}


.gqr_script_vector <- function(x) {
  if (is.null(x)) return("NULL")
  if (length(x) == 0L) {
    if (is.character(x)) return("character(0)")
    if (is.numeric(x)) return("numeric(0)")
    if (is.logical(x)) return("logical(0)")
    return("NULL")
  }

  if (is.character(x) || is.factor(x)) {
    values <- vapply(as.character(x), .gqr_script_quote, character(1))
  } else if (is.logical(x)) {
    values <- ifelse(is.na(x), "NA", ifelse(x, "TRUE", "FALSE"))
  } else if (is.integer(x)) {
    values <- ifelse(is.na(x), "NA_integer_", paste0(x, "L"))
  } else if (is.numeric(x)) {
    values <- ifelse(is.na(x), "NA_real_", format(x, digits = 17, scientific = FALSE, trim = TRUE))
  } else {
    values <- vapply(as.character(x), .gqr_script_quote, character(1))
  }

  if (length(values) == 1L) values else paste0("c(", paste(values, collapse = ", "), ")")
}

.gqr_script_comment <- function(x) {
  x <- as.character(x)
  x <- gsub("\r", " ", x, fixed = TRUE)
  x <- gsub("\n", " ", x, fixed = TRUE)
  paste0("# ", x)
}

.gqr_script_filters <- function(name, filters) {
  if (is.null(filters) || length(filters) == 0L) {
    return(character())
  }

  out <- paste0(name, " <- list()")
  for (nm in names(filters)) {
    out <- c(
      out,
      paste0(
        name,
        "[[", .gqr_script_quote(nm), "]] <- ",
        .gqr_script_vector(filters[[nm]])
      )
    )
  }
  out
}

.gqr_script_groups <- function(groups) {
  if (is.null(groups) || nrow(groups) == 0L) {
    return("groups <- NULL")
  }

  if (!all(c("group", "variable") %in% names(groups))) {
    stop("`state$groups` must contain `group` and `variable` columns.", call. = FALSE)
  }

  groups <- groups[, c("group", "variable"), drop = FALSE]
  c(
    "groups <- data.frame(",
    paste0("  group = ", .gqr_script_vector(as.character(groups$group)), ","),
    paste0("  variable = ", .gqr_script_vector(as.character(groups$variable)), ","),
    "  stringsAsFactors = FALSE",
    ")"
  )
}

.gqr_state_value <- function(x, name, default = NULL) {
  if (is.null(x) || is.null(x[[name]])) default else x[[name]]
}

.gqr_script_dummies_plots <- function(settings, has_groups = FALSE) {
  if (is.null(settings)) return(character())

  n_rows <- as.integer(.gqr_state_value(settings, "n_rows", 100L))
  if (!is.finite(n_rows) || n_rows < 1L) n_rows <- 100L
  variable_order <- .gqr_state_value(settings, "variable_order", "data")
  show_w_values <- isTRUE(.gqr_state_value(settings, "show_w_values", FALSE))

  out <- c(
    "",
    "# ----------------------------------------------------------------",
    "# Dummies-tab plots",
    "# ----------------------------------------------------------------",
    paste0("D_plot_data <- D[seq_len(min(", n_rows, "L, nrow(D))), , drop = FALSE]"),
    "plot_vars <- colnames(D_plot_data)"
  )

  if (isTRUE(has_groups)) {
    out <- c(
      out,
      "plot_groups <- groups[!is.na(groups$variable) & groups$variable %in% plot_vars, , drop = FALSE]"
    )

    if (identical(variable_order, "group_size")) {
      out <- c(
        out,
        "ordered_groups <- names(sort(table(plot_groups$group), decreasing = TRUE))"
      )
    } else if (identical(variable_order, "alphabetical")) {
      out <- c(
        out,
        "ordered_groups <- sort(unique(plot_groups$group))"
      )
    } else {
      out <- c(
        out,
        "ordered_groups <- unique(plot_groups$group)"
      )
    }

    out <- c(
      out,
      "plot_groups$.group_order <- match(plot_groups$group, ordered_groups)",
      "plot_groups <- plot_groups[order(plot_groups$.group_order, match(plot_groups$variable, plot_vars)), , drop = FALSE]",
      "grouped_vars <- unique(plot_groups$variable)",
      "plot_vars <- c(grouped_vars, setdiff(plot_vars, grouped_vars))"
    )
  } else if (!identical(variable_order, "data")) {
    out <- c(out, "plot_vars <- sort(plot_vars)")
  }

  out <- c(
    out,
    "D_plot_data <- D_plot_data[, plot_vars, drop = FALSE]",
    "dummy_labels <- stats::setNames(",
    "  vapply(colnames(D_plot_data), function(x) if (nchar(x) <= 10L) x else paste0(substr(x, 1L, 9L), \"...\"), character(1)),",
    "  colnames(D_plot_data)",
    ")",
    "D_plot_long <- as.data.frame(D_plot_data) |>",
    "  dplyr::mutate(pattern = dplyr::row_number()) |>",
    "  tidyr::pivot_longer(cols = -dplyr::all_of(\"pattern\"), names_to = \"variable\", values_to = \"value\") |>",
    "  dplyr::mutate(variable = factor(.data$variable, levels = colnames(D_plot_data)))",
    "plot_dummy_matrix <- ggplot2::ggplot(",
    "  D_plot_long, ggplot2::aes(x = .data$variable, y = .data$pattern, fill = factor(.data$value))",
    ") +",
    "  ggplot2::geom_tile(colour = \"grey30\") +",
    "  ggplot2::scale_fill_manual(values = c(\"0\" = \"#CC0000\", \"1\" = \"#009933\"), name = \"Dummy\") +",
    "  ggplot2::scale_y_reverse() +",
    "  ggplot2::scale_x_discrete(labels = dummy_labels) +",
    "  ggplot2::labs(x = \"Variable\", y = \"Pattern index\") +",
    "  ggplot2::theme_minimal() +",
    "  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 11), axis.ticks.x = ggplot2::element_blank())",
    "print(plot_dummy_matrix)",
    "",
    "W_plot_data <- GQR::gqr_make_w(",
    "  data = analysis_data,",
    "  analysis_cols = analysis_cols,",
    "  D = D,",
    "  id_col = if (\"ID\" %in% names(analysis_data)) \"ID\" else NULL,",
    paste0("  rows = seq_len(min(", n_rows, "L, nrow(D))),"),
    "  algorithm = \"matmul\"",
    ")",
    "W_plot_long <- as.data.frame(W_plot_data) |>",
    "  dplyr::mutate(Combination = dplyr::row_number()) |>",
    "  tidyr::pivot_longer(cols = -dplyr::all_of(\"Combination\"), names_to = \"Respondent\", values_to = \"Value\") |>",
    "  dplyr::mutate(Respondent = factor(.data$Respondent, levels = colnames(W_plot_data)))",
    "plot_w_matrix <- ggplot2::ggplot(",
    "  W_plot_long, ggplot2::aes(x = .data$Respondent, y = .data$Combination, fill = .data$Value)",
    ") +",
    "  ggplot2::geom_tile() +",
    "  ggplot2::scale_fill_viridis_c(option = \"C\", direction = 1) +",
    "  ggplot2::scale_y_reverse() +",
    "  ggplot2::labs(x = \"Respondent\", y = \"Combination index (S_i)\", fill = \"W value\") +",
    "  ggplot2::theme_minimal() +",
    "  ggplot2::theme(axis.text.x = ggplot2::element_blank(), axis.ticks.x = ggplot2::element_blank())"
  )

  if (show_w_values) {
    out <- c(
      out,
      "plot_w_matrix <- plot_w_matrix +",
      "  ggplot2::geom_text(",
      "    ggplot2::aes(label = sprintf(\"%.2f\", .data$Value)),",
      "    size = max(1.5, min(4, 14 / sqrt(max(nrow(W_plot_data), ncol(W_plot_data)))))",
      "  )"
    )
  }

  c(out, "print(plot_w_matrix)")
}

.gqr_script_pca_plot <- function() {
  c(
    "",
    "# ----------------------------------------------------------------",
    "# PCA scree plot",
    "# ----------------------------------------------------------------",
    "pca_plot_df <- data.frame(",
    "  Component = seq_along(pca$eigenvalues),",
    "  PC = factor(paste0(\"PC\", seq_along(pca$eigenvalues)), levels = paste0(\"PC\", seq_along(pca$eigenvalues))),",
    "  Eigenvalue = pca$eigenvalues,",
    "  Variance = pca$variance_explained",
    ")",
    "pca_scree_scale <- max(pca_plot_df$Variance, na.rm = TRUE) / max(pca_plot_df$Eigenvalue, na.rm = TRUE)",
    "plot_pca_scree <- ggplot2::ggplot(pca_plot_df, ggplot2::aes(x = .data$PC)) +",
    "  ggplot2::geom_col(ggplot2::aes(y = .data$Variance), fill = \"#404F69\", alpha = 0.85) +",
    "  ggplot2::geom_line(ggplot2::aes(y = .data$Eigenvalue * pca_scree_scale, group = 1), colour = \"#B22222\", linewidth = 1.2) +",
    "  ggplot2::geom_point(ggplot2::aes(y = .data$Eigenvalue * pca_scree_scale), colour = \"#B22222\", size = 3) +",
    "  ggplot2::labs(x = \"Component\", y = \"Explained variance (%)\", title = \"Explained Variance and Scree Plot\") +",
    "  ggplot2::scale_y_continuous(sec.axis = ggplot2::sec_axis(~ . / pca_scree_scale, name = \"Eigenvalue\")) +",
    "  ggplot2::theme_minimal() +",
    "  ggplot2::theme(",
    "    axis.text.x = ggplot2::element_text(size = 14, face = \"bold\"),",
    "    axis.text.y = ggplot2::element_text(size = 14, face = \"bold\"),",
    "    axis.title.x = ggplot2::element_text(size = 16, face = \"bold\"),",
    "    axis.title.y = ggplot2::element_text(size = 16, face = \"bold\"),",
    "    plot.title = ggplot2::element_text(size = 18, face = \"bold\")",
    "  )",
    "print(plot_pca_scree)"
  )
}

.gqr_script_statement_plot <- function(settings, groups = NULL, analysis_cols = character(0)) {
  if (is.null(settings)) return(character())

  mode <- .gqr_state_value(settings, "display_mode", "beta")
  use_beta <- identical(mode, "beta")

  statement_vars <- as.character(analysis_cols)
  if (length(statement_vars) == 0L) statement_vars <- character(0)

  has_groups <- !is.null(groups) && nrow(groups) > 0L &&
    all(c("group", "variable") %in% names(groups))

  if (has_groups) {
    g <- groups[
      !is.na(groups$group) &
        !is.na(groups$variable) &
        groups$variable %in% statement_vars,
      c("group", "variable"),
      drop = FALSE
    ]
    group_order <- unique(as.character(g$group))
    g$.group_order <- match(as.character(g$group), group_order)
    g$.variable_order <- match(as.character(g$variable), statement_vars)
    g <- g[order(g$.group_order, g$.variable_order), , drop = FALSE]
    grouped <- unique(as.character(g$variable))
    ungrouped <- setdiff(statement_vars, grouped)
    layout_vars <- c(grouped, ungrouped)
    layout_groups <- c(
      as.character(g$group[match(grouped, g$variable)]),
      rep("Ungrouped", length(ungrouped))
    )
  } else {
    layout_vars <- statement_vars
    layout_groups <- rep(NA_character_, length(layout_vars))
  }

  y_vars <- rev(layout_vars)
  y_groups <- layout_groups[match(y_vars, layout_vars)]
  transitions <- if (length(y_groups) > 1L) {
    which(y_groups[-1L] != y_groups[-length(y_groups)]) + 0.5
  } else {
    numeric(0)
  }

  label_groups <- character(0)
  label_vars <- character(0)
  if (has_groups && length(layout_vars) > 0L) {
    for (grp in unique(layout_groups[!is.na(layout_groups)])) {
      vars_now <- layout_vars[layout_groups == grp]
      if (length(vars_now) > 0L) {
        label_groups <- c(label_groups, grp)
        label_vars <- c(label_vars, vars_now[ceiling(length(vars_now) / 2)])
      }
    }
  }

  out <- c(
    "",
    "# ----------------------------------------------------------------",
    "# Statement-Component Regression heatmap",
    "# ----------------------------------------------------------------",
    paste0(
      "statement_plot_result <- ",
      if (use_beta) "statement_regression_standardised" else "statement_regression_raw"
    ),
    paste0("statement_plot_vars <- ", .gqr_script_vector(layout_vars)),
    "statement_plot_pcs <- colnames(pca$scores)",
    "statement_layout <- data.frame(",
    paste0("  Variable = ", .gqr_script_vector(layout_vars), ","),
    paste0("  Group = ", .gqr_script_vector(layout_groups), ","),
    "  stringsAsFactors = FALSE",
    ")",
    "statement_coeff <- statement_plot_result$coefficients |>",
    "  dplyr::filter(.data$term != \"(Intercept)\") |>",
    "  dplyr::transmute(Component = .data$component, Variable = .data$term, Estimate = .data$estimate)",
    "statement_plot_df <- expand.grid(",
    "  Variable = statement_layout$Variable,",
    "  Component = statement_plot_pcs,",
    "  stringsAsFactors = FALSE",
    ") |>",
    "  dplyr::left_join(statement_coeff, by = c(\"Variable\", \"Component\"))",
    "zero_reference <- union(statement_plot_result$baselines, statement_plot_result$constants)",
    "statement_plot_df$Estimate[statement_plot_df$Variable %in% zero_reference] <- 0",
    "statement_plot_df$ComponentIndex <- match(statement_plot_df$Component, statement_plot_pcs)",
    "statement_plot_df$Component <- factor(statement_plot_df$Component, levels = statement_plot_pcs)",
    "statement_plot_df$Variable <- factor(statement_plot_df$Variable, levels = rev(statement_layout$Variable))",
    "statement_plot_df$Label <- ifelse(is.na(statement_plot_df$Estimate), \"\", sprintf(\"%.2f\", statement_plot_df$Estimate))",
    paste0("statement_transitions <- ", .gqr_script_vector(transitions)),
    paste0(
      "statement_x_max <- length(statement_plot_pcs) + ",
      if (length(label_groups) > 0L) "2.2" else "0.5"
    )
  )

  if (length(label_groups) > 0L) {
    out <- c(
      out,
      "statement_label_df <- data.frame(",
      paste0("  Group = ", .gqr_script_vector(label_groups), ","),
      paste0("  Variable = ", .gqr_script_vector(label_vars), ","),
      "  stringsAsFactors = FALSE",
      ")",
      "statement_label_df$Variable <- factor(statement_label_df$Variable, levels = rev(statement_layout$Variable))",
      "statement_label_df$X <- length(statement_plot_pcs) + 0.75"
    )
  }

  out <- c(
    out,
    "plot_statement_coefficients <- ggplot2::ggplot(",
    "  statement_plot_df, ggplot2::aes(x = .data$ComponentIndex, y = .data$Variable, fill = .data$Estimate)",
    ") +",
    "  ggplot2::geom_tile(width = 1, height = 1, colour = \"white\") +",
    "  ggplot2::geom_text(ggplot2::aes(label = .data$Label), size = 5.5, fontface = \"bold\") +",
    "  ggplot2::scale_fill_distiller(palette = \"Spectral\", direction = -1, na.value = \"grey90\") +",
    "  ggplot2::scale_x_continuous(breaks = seq_along(statement_plot_pcs), labels = statement_plot_pcs, limits = c(0.5, statement_x_max), expand = c(0, 0)) +",
    paste0(
      "  ggplot2::labs(x = \"PCA Component\", y = \"Original statement\", fill = ",
      .gqr_script_quote(if (use_beta) "Std. beta" else "Coefficient"),
      ", title = ",
      .gqr_script_quote(if (use_beta) "Standardised Regression Coefficients" else "Unstandardised Regression Coefficients"),
      ") +"
    ),
    "  ggplot2::theme_minimal() +",
    "  ggplot2::theme(",
    "    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 15, face = \"bold\"),",
    "    axis.text.y = ggplot2::element_text(size = 15, face = \"bold\"),",
    "    axis.title.x = ggplot2::element_text(size = 17, face = \"bold\"),",
    "    axis.title.y = ggplot2::element_text(size = 17, face = \"bold\"),",
    "    plot.title = ggplot2::element_text(size = 19, face = \"bold\"),",
    "    legend.position = \"bottom\",",
    "    legend.title = ggplot2::element_text(size = 14, face = \"bold\"),",
    "    legend.text = ggplot2::element_text(size = 12, face = \"bold\"),",
    "    plot.margin = ggplot2::margin(5.5, 15, 5.5, 5.5)",
    "  )"
  )

  if (length(transitions) > 0L) {
    out <- c(
      out,
      "plot_statement_coefficients <- plot_statement_coefficients +",
      "  ggplot2::geom_hline(yintercept = statement_transitions, linewidth = 0.7, colour = \"grey30\")"
    )
  }

  if (length(label_groups) > 0L) {
    out <- c(
      out,
      "plot_statement_coefficients <- plot_statement_coefficients +",
      "  ggplot2::geom_vline(xintercept = length(statement_plot_pcs) + 0.5, linewidth = 0.6, colour = \"grey55\") +",
      "  ggplot2::geom_text(",
      "    data = statement_label_df,",
      "    ggplot2::aes(x = .data$X, y = .data$Variable, label = .data$Group),",
      "    inherit.aes = FALSE, hjust = 0, fontface = \"bold\", size = 4.5",
      "  )"
    )
  }

  c(out, "print(plot_statement_coefficients)")
}

.gqr_script_respondent_plots <- function(settings, covariate_cols) {
  if (is.null(settings)) return(character())

  relationship <- .gqr_state_value(settings, "relationship", list())
  scatter <- .gqr_state_value(settings, "scatter", list())
  filters <- .gqr_state_value(settings, "filters", list())

  rel_component <- .gqr_state_value(relationship, "component")
  rel_covariate <- .gqr_state_value(relationship, "covariate")
  rel_type <- .gqr_state_value(relationship, "type", "categorical")
  rel_levels <- as.character(.gqr_state_value(relationship, "levels", character(0)))
  rel_colours <- as.character(.gqr_state_value(relationship, "colours", character(0)))

  x_component <- .gqr_state_value(scatter, "x_component")
  y_component <- .gqr_state_value(scatter, "y_component")

  colour <- .gqr_state_value(scatter, "colour", "None")
  colour_type <- .gqr_state_value(
    scatter,
    "colour_type",
    if (identical(colour, "None")) "none" else "categorical"
  )
  colour_levels <- as.character(.gqr_state_value(scatter, "colour_levels", character(0)))
  colour_values <- as.character(.gqr_state_value(scatter, "colour_values", character(0)))

  size <- .gqr_state_value(scatter, "size", "None")
  size_type <- .gqr_state_value(
    scatter,
    "size_type",
    if (identical(size, "None")) "none" else "categorical"
  )
  size_levels <- as.character(.gqr_state_value(scatter, "size_levels", character(0)))

  hull <- .gqr_state_value(scatter, "hull", "None")
  hull_levels <- as.character(.gqr_state_value(scatter, "hull_levels", character(0)))
  hull_values <- as.character(.gqr_state_value(scatter, "hull_values", character(0)))

  ellipse <- .gqr_state_value(scatter, "ellipse", "None")
  ellipse_group_levels <- as.character(
    .gqr_state_value(scatter, "ellipse_group_levels", character(0))
  )
  ellipse_values <- as.character(.gqr_state_value(scatter, "ellipse_values", character(0)))
  ellipse_levels <- as.numeric(.gqr_state_value(scatter, "ellipse_levels", numeric(0)))
  ellipse_levels <- ellipse_levels[is.finite(ellipse_levels)]

  show_arrows <- isTRUE(.gqr_state_value(scatter, "show_arrows", FALSE))
  arrow_vars <- as.character(.gqr_state_value(scatter, "arrow_vars", character(0)))

  filter_values <- .gqr_state_value(filters, "filters")
  filter_ids <- .gqr_state_value(filters, "ids")
  filter_limit <- .gqr_state_value(filters, "limit")

  out <- c(
    "",
    "# ----------------------------------------------------------------",
    "# Component-Covariate plots",
    "# ----------------------------------------------------------------",
    "respondent_plot_loadings <- as.data.frame(pca$loadings, check.names = FALSE)",
    "respondent_plot_loadings$ID <- rownames(pca$loadings)",
    paste0("respondent_plot_covariates <- ", .gqr_script_vector(as.character(covariate_cols))),
    "respondent_plot_meta <- analysis_data[, intersect(respondent_plot_covariates, names(analysis_data)), drop = FALSE]",
    "respondent_plot_meta$ID <- rownames(pca$loadings)",
    "respondent_plot_all <- dplyr::left_join(respondent_plot_loadings, respondent_plot_meta, by = \"ID\")",
    "respondent_plot_data <- respondent_plot_all"
  )

  if (length(filter_values) > 0L) {
    out <- c(out, .gqr_script_filters("respondent_plot_filters", filter_values))
  }
  if (length(filter_values) > 0L || length(filter_ids) > 0L) {
    out <- c(
      out,
      "respondent_plot_data <- GQR::gqr_filter_data(",
      "  respondent_plot_data,",
      "  id_col = \"ID\",",
      paste0("  ids = ", .gqr_script_vector(filter_ids), ","),
      paste0("  filters = ", if (length(filter_values) > 0L) "respondent_plot_filters" else "NULL"),
      ")"
    )
  }
  if (!is.null(filter_limit) && length(filter_limit) == 1L && is.finite(filter_limit)) {
    out <- c(out, paste0("respondent_plot_data <- head(respondent_plot_data, ", as.integer(filter_limit), "L)"))
  }

  if (!is.null(rel_component) && !is.null(rel_covariate)) {
    component_q <- .gqr_script_quote(rel_component)
    covariate_q <- .gqr_script_quote(rel_covariate)

    out <- c(out, "", "# Component-covariate relationship")

    if (identical(rel_type, "numeric")) {
      out <- c(
        out,
        "plot_component_covariate <- ggplot2::ggplot(",
        paste0("  respondent_plot_data, ggplot2::aes(x = .data[[", covariate_q, "]], y = .data[[", component_q, "]])"),
        ") +",
        "  ggplot2::geom_point(alpha = 0.45, colour = \"#404F69\", na.rm = TRUE) +",
        "  ggplot2::geom_smooth(method = \"loess\", se = FALSE, colour = \"#B22222\", na.rm = TRUE) +",
        "  ggplot2::theme_minimal() +",
        paste0("  ggplot2::labs(x = ", covariate_q, ", y = ", component_q, ")"),
        "print(plot_component_covariate)"
      )
    } else {
      out <- c(
        out,
        paste0("relationship_levels <- ", .gqr_script_vector(rel_levels)),
        paste0("relationship_cols <- stats::setNames(", .gqr_script_vector(rel_colours), ", relationship_levels)"),
        "relationship_data <- respondent_plot_data",
        paste0(
          "relationship_data[[", covariate_q, "]] <- factor(as.character(relationship_data[[",
          covariate_q, "]]), levels = relationship_levels)"
        ),
        "plot_component_covariate <- ggplot2::ggplot(",
        paste0("  relationship_data, ggplot2::aes(x = .data[[", covariate_q, "]], y = .data[[", component_q, "]])"),
        ") +",
        paste0("  ggplot2::geom_boxplot(ggplot2::aes(fill = .data[[", covariate_q, "]]), alpha = 0.65, na.rm = TRUE) +"),
        "  ggplot2::scale_x_discrete(drop = FALSE) +",
        "  ggplot2::scale_fill_manual(values = relationship_cols, limits = relationship_levels, drop = FALSE, na.value = \"grey85\") +",
        "  ggplot2::theme_minimal(base_size = 18) +",
        "  ggplot2::theme(",
        "    axis.title = ggplot2::element_text(size = 20, face = \"bold\"),",
        "    axis.text = ggplot2::element_text(size = 17),",
        "    legend.title = ggplot2::element_text(size = 18, face = \"bold\"),",
        "    legend.text = ggplot2::element_text(size = 16),",
        "    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)",
        "  ) +",
        paste0("  ggplot2::labs(x = ", covariate_q, ", y = ", component_q, ")"),
        "print(plot_component_covariate)"
      )
    }
  }

  if (!is.null(x_component) && !is.null(y_component)) {
    xq <- .gqr_script_quote(x_component)
    yq <- .gqr_script_quote(y_component)
    colour_q <- .gqr_script_quote(colour)
    size_q <- .gqr_script_quote(size)
    hull_q <- .gqr_script_quote(hull)
    ellipse_q <- .gqr_script_quote(ellipse)

    out <- c(
      out,
      "",
      "# Respondent component map",
      "scatter_data <- respondent_plot_data |>",
      paste0("  dplyr::filter(is.finite(.data[[", xq, "]]), is.finite(.data[[", yq, "]]))")
    )

    if (identical(size_type, "categorical")) {
      out <- c(
        out,
        paste0("size_levels <- ", .gqr_script_vector(size_levels)),
        paste0(
          "scatter_data$.size_group <- factor(as.character(scatter_data[[",
          size_q, "]]), levels = size_levels)"
        ),
        "scatter_data$.size_map <- as.numeric(scatter_data$.size_group)"
      )
    }

    point_aes <- character(0)
    if (!identical(colour_type, "none")) {
      point_aes <- c(point_aes, paste0("colour = .data[[", colour_q, "]]"))
    }
    if (identical(size_type, "numeric")) {
      point_aes <- c(point_aes, paste0("size = .data[[", size_q, "]]"))
    } else if (identical(size_type, "categorical")) {
      point_aes <- c(point_aes, "size = .data$.size_map")
    }

    out <- c(
      out,
      paste0(
        "plot_component_map <- ggplot2::ggplot(scatter_data, ggplot2::aes(x = .data[[",
        xq, "]], y = .data[[", yq, "]])) +"
      )
    )

    if (length(point_aes) > 0L) {
      out <- c(
        out,
        paste0(
          "  ggplot2::geom_point(ggplot2::aes(",
          paste(point_aes, collapse = ", "),
          "), alpha = 0.85, na.rm = TRUE)"
        )
      )
    } else {
      out <- c(
        out,
        "  ggplot2::geom_point(colour = \"#3A3A3A\", alpha = 0.85, na.rm = TRUE)"
      )
    }

    if (identical(colour_type, "numeric")) {
      out <- c(
        out,
        paste0(
          "plot_component_map <- plot_component_map + ggplot2::scale_colour_viridis_c(name = ",
          colour_q,
          ", option = \"D\", end = 0.90, na.value = \"grey75\", guide = ggplot2::guide_colourbar(order = 1))"
        )
      )
    } else if (identical(colour_type, "categorical")) {
      out <- c(
        out,
        paste0("colour_levels <- ", .gqr_script_vector(colour_levels)),
        paste0("point_cols <- stats::setNames(", .gqr_script_vector(colour_values), ", colour_levels)"),
        paste0(
          "plot_component_map <- plot_component_map + ggplot2::scale_colour_manual(name = ",
          colour_q,
          ", values = point_cols, limits = colour_levels, drop = FALSE, na.value = \"grey75\", guide = ggplot2::guide_legend(order = 1))"
        )
      )
    }

    if (identical(size_type, "numeric")) {
      out <- c(
        out,
        paste0(
          "plot_component_map <- plot_component_map + ggplot2::scale_size_continuous(name = ",
          size_q,
          ", guide = ggplot2::guide_legend(order = 2))"
        )
      )
    } else if (identical(size_type, "categorical")) {
      out <- c(
        out,
        paste0(
          "plot_component_map <- plot_component_map + ggplot2::scale_size_continuous(name = ",
          size_q,
          ", breaks = seq_along(size_levels), labels = size_levels, guide = ggplot2::guide_legend(order = 2))"
        )
      )
    }

    if (!identical(hull, "None") && length(hull_levels) > 0L) {
      out <- c(
        out,
        "",
        "# Convex hulls",
        paste0("hull_levels <- ", .gqr_script_vector(hull_levels)),
        paste0("hull_cols <- stats::setNames(", .gqr_script_vector(hull_values), ", hull_levels)"),
        paste0(
          "hull_source <- scatter_data |> dplyr::filter(as.character(.data[[",
          hull_q, "]]) %in% hull_levels)"
        ),
        paste0("hull_source$.hull_group <- as.character(hull_source[[", hull_q, "]])"),
        "hull_data <- hull_source |>",
        "  dplyr::group_by(.data$.hull_group) |>",
        paste0(
          "  dplyr::group_modify(~ .x[chull(.x[[", xq, "]], .x[[", yq, "]]), , drop = FALSE]) |>"
        ),
        "  dplyr::ungroup()",
        "plot_component_map <- plot_component_map + ggnewscale::new_scale_fill() +",
        "  ggplot2::geom_polygon(",
        "    data = hull_data,",
        paste0(
          "    ggplot2::aes(x = .data[[", xq, "]], y = .data[[", yq,
          "]], group = .data$.hull_group, fill = .data[[", hull_q, "]]),"
        ),
        paste0(
          "    colour = unname(hull_cols[as.character(hull_data[[", hull_q,
          "]])]), alpha = 0.20, linewidth = 1.1, inherit.aes = FALSE"
        ),
        "  ) +",
        paste0(
          "  ggplot2::scale_fill_manual(name = ", .gqr_script_quote(paste0(hull, " (hull)")),
          ", values = scales::alpha(hull_cols, 0.20), limits = hull_levels, drop = FALSE)"
        )
      )
    }

    if (!identical(ellipse, "None") &&
        length(ellipse_group_levels) > 0L &&
        length(ellipse_levels) > 0L) {
      out <- c(
        out,
        "",
        "# Confidence ellipses",
        paste0("ellipse_groups <- ", .gqr_script_vector(ellipse_group_levels)),
        paste0("ellipse_cols <- stats::setNames(", .gqr_script_vector(ellipse_values), ", ellipse_groups)"),
        paste0(
          "ellipse_data <- scatter_data |> dplyr::filter(as.character(.data[[",
          ellipse_q, "]]) %in% ellipse_groups)"
        ),
        "plot_component_map <- plot_component_map + ggnewscale::new_scale_colour()"
      )

      max_level <- max(ellipse_levels)
      for (lev in ellipse_levels) {
        out <- c(
          out,
          "plot_component_map <- plot_component_map + ggplot2::stat_ellipse(",
          "  data = ellipse_data,",
          paste0(
            "  ggplot2::aes(x = .data[[", xq, "]], y = .data[[", yq,
            "]], colour = .data[[", ellipse_q, "]], group = .data[[", ellipse_q, "]]),"
          ),
          paste0("  level = ", format(lev, trim = TRUE, scientific = FALSE), ","),
          paste0(
            "  linetype = ",
            .gqr_script_quote(if (isTRUE(all.equal(lev, max_level))) "solid" else "dashed"),
            ","
          ),
          "  linewidth = 1.1, inherit.aes = FALSE,",
          paste0(
            "  show.legend = ",
            if (isTRUE(all.equal(lev, max_level))) "TRUE" else "FALSE"
          ),
          ")"
        )
      }

      out <- c(
        out,
        paste0(
          "plot_component_map <- plot_component_map + ggplot2::scale_colour_manual(name = ",
          .gqr_script_quote(paste0(ellipse, " (ellipse)")),
          ", values = ellipse_cols, limits = ellipse_groups, drop = FALSE, na.value = \"grey75\")"
        )
      )
    }

    if (show_arrows && length(arrow_vars) > 0L) {
      out <- c(
        out,
        "",
        "# Covariate arrows",
        paste0("arrow_vars <- ", .gqr_script_vector(arrow_vars)),
        "arrow_data <- data.frame(",
        "  Variable = arrow_vars,",
        paste0(
          "  xend = vapply(arrow_vars, function(v) stats::cor(scatter_data[[v]], scatter_data[[",
          xq, "]], use = \"pairwise.complete.obs\"), numeric(1)),"
        ),
        paste0(
          "  yend = vapply(arrow_vars, function(v) stats::cor(scatter_data[[v]], scatter_data[[",
          yq, "]], use = \"pairwise.complete.obs\"), numeric(1)),"
        ),
        "  stringsAsFactors = FALSE",
        ") |>",
        "  dplyr::filter(is.finite(.data$xend), is.finite(.data$yend))",
        paste0("x_scale <- max(abs(scatter_data[[", xq, "]]), na.rm = TRUE)"),
        paste0("y_scale <- max(abs(scatter_data[[", yq, "]]), na.rm = TRUE)"),
        "ax_scale <- max(abs(arrow_data$xend), na.rm = TRUE)",
        "ay_scale <- max(abs(arrow_data$yend), na.rm = TRUE)",
        "arrow_scale <- 0.8 * min(",
        "  x_scale / max(ax_scale, .Machine$double.eps),",
        "  y_scale / max(ay_scale, .Machine$double.eps)",
        ")",
        "arrow_data$xend <- arrow_data$xend * arrow_scale",
        "arrow_data$yend <- arrow_data$yend * arrow_scale",
        "plot_component_map <- plot_component_map +",
        "  ggplot2::geom_segment(",
        "    data = arrow_data,",
        "    ggplot2::aes(x = 0, y = 0, xend = .data$xend, yend = .data$yend),",
        "    arrow = grid::arrow(length = grid::unit(0.02, \"npc\")),",
        "    colour = \"#B22222\", linewidth = 0.7, inherit.aes = FALSE",
        "  ) +",
        "  ggplot2::geom_text(",
        "    data = arrow_data,",
        "    ggplot2::aes(x = .data$xend, y = .data$yend, label = .data$Variable),",
        "    colour = \"#B22222\", hjust = 0.5, vjust = -0.35, size = 3.4, inherit.aes = FALSE",
        "  )"
      )
    }

    out <- c(
      out,
      "plot_component_map <- plot_component_map +",
      paste0("  ggplot2::labs(x = ", xq, ", y = ", yq, ") +"),
      "  ggplot2::theme_minimal(base_size = 18) +",
      "  ggplot2::theme(",
      "    axis.title = ggplot2::element_text(size = 20, face = \"bold\"),",
      "    axis.text = ggplot2::element_text(size = 17),",
      "    legend.title = ggplot2::element_text(size = 18, face = \"bold\"),",
      "    legend.text = ggplot2::element_text(size = 16),",
      "    legend.key.width = grid::unit(1.1, \"lines\"),",
      "    legend.key.height = grid::unit(1.1, \"lines\")",
      "  )",
      "print(plot_component_map)"
    )
  }

  out
}

#' Generate executable R code from a GQR analysis state
#'
#' @description
#' Converts a structured GQR analysis state into an executable R script. The
#' Shiny application uses this function to expose the current analysis as code
#' that can be copied into RStudio or downloaded as an `.R` file.
#'
#' @param state Named list describing the analysis. The Shiny application
#'   constructs this automatically. At minimum it should contain `data`,
#'   `analysis_cols`, `dummy`, and, after PCA has been run, `pca`. See Details.
#' @param include_session_info Logical; append `sessionInfo()` to the generated
#'   script.
#' @param verify_files Logical; for uploaded files, include an MD5 checksum
#'   comparison that warns when the local file differs from the file used in
#'   Shiny.
#'
#' @return A single character string containing executable R code.
#'
#' @details
#' `state$data` must describe either a bundled example (`type = "example"`,
#' `dataset = "gardening"` or `"dummy_data"`) or an uploaded file
#' (`type = "file"`, `name`, and optionally `md5`, `n_rows`, and `n_cols`).
#' Uploaded grouping files are recorded as provenance, but the final grouping
#' is written explicitly into the generated script so that any edits made in
#' the graphical interface are reproduced exactly.
#'
#' The generated script records original upload names rather than Shiny's
#' temporary upload paths. Consequently, downloaded scripts expect those input
#' files to be present in the R working directory (or the user can edit the
#' generated path before running the script).
#'
#' This function is intentionally text-generating: it does not execute the
#' analysis or write any files.
#'
#' @examples
#' state <- list(
#'   data = list(type = "example", dataset = "dummy_data"),
#'   analysis_cols = paste0("Q", 1:4),
#'   covariate_cols = "Numeric_covariate",
#'   transform = "none",
#'   groups = NULL,
#'   dummy = list(mode = "all", include_empty = TRUE, max_patterns = 1000000L),
#'   pca = list(
#'     engine = "design", n_components = 2L, rotation = "none",
#'     center = TRUE, scale = TRUE, id_col = "ID",
#'     data_filter = list(filters = NULL, ids = NULL, id_col = "ID")
#'   )
#' )
#' cat(gqr_reproducible_script(state, include_session_info = FALSE))
#'
#' @export
gqr_reproducible_script <- function(
    state,
    include_session_info = TRUE,
    verify_files = TRUE) {

  if (!is.list(state) || is.null(state$data) || is.null(state$analysis_cols)) {
    stop("`state` must contain at least `data` and `analysis_cols`.", call. = FALSE)
  }

  data_state <- state$data
  data_type <- .gqr_state_value(data_state, "type")
  if (length(data_type) != 1L || is.na(data_type) || !data_type %in% c("example", "file")) {
    stop("`state$data$type` must be `example` or `file`.", call. = FALSE)
  }

  version <- .gqr_state_value(state, "package_version", as.character(utils::packageVersion("GQR")))
  generated_at <- .gqr_state_value(state, "generated_at", Sys.time())
  plot_settings <- .gqr_state_value(state, "plot_settings", list())

  out <- c(
    "# ================================================================",
    "# Reproducible GQR analysis",
    paste0("# Generated by GQR ", version),
    paste0("# Generated: ", format(as.POSIXct(generated_at), "%Y-%m-%d %H:%M:%S %Z")),
    "# ================================================================",
    "",
    "library(GQR)",
    ""
  )

  uploads <- .gqr_state_value(state, "uploads", list())
  if (length(uploads) > 0L) {
    out <- c(out, "# ----------------------------------------------------------------", "# Files uploaded during the Shiny session", "# ----------------------------------------------------------------")
    for (i in seq_along(uploads)) {
      u <- uploads[[i]]
      role <- .gqr_state_value(u, "role", "input")
      nm <- .gqr_state_value(u, "original_name", "unknown")
      status <- .gqr_state_value(u, "status", "uploaded")
      size <- .gqr_state_value(u, "size_bytes", NA_real_)
      md5 <- .gqr_state_value(u, "md5", NA_character_)
      dims <- ""
      nr <- .gqr_state_value(u, "n_rows", NA_integer_)
      nc <- .gqr_state_value(u, "n_cols", NA_integer_)
      if (is.finite(nr) && is.finite(nc)) dims <- paste0("; ", nr, " rows x ", nc, " columns")
      size_txt <- if (is.finite(size)) paste0("; ", format(round(size / 1024^2, 3), trim = TRUE), " MiB") else ""
      md5_txt <- if (!is.na(md5) && nzchar(md5)) paste0("; MD5 ", md5) else ""
      out <- c(out, .gqr_script_comment(paste0(i, ". ", nm, " [", role, "; ", status, "]", dims, size_txt, md5_txt)))
    }
    out <- c(out, "")
  }

  out <- c(out, "# ----------------------------------------------------------------", "# Data", "# ----------------------------------------------------------------")

  if (identical(data_type, "example")) {
    dataset <- .gqr_state_value(data_state, "dataset")
    if (is.null(dataset) || !dataset %in% c("dummy_data", "gardening")) {
      stop("Example data must identify `dummy_data` or `gardening`.", call. = FALSE)
    }
    out <- c(
      out,
      paste0("dat <- GQR::gqr_example_data(", .gqr_script_quote(dataset), ")")
    )
    rename_from <- .gqr_state_value(data_state, "rename_id_from")
    rename_to <- .gqr_state_value(data_state, "rename_id_to")
    if (!is.null(rename_from) && !is.null(rename_to)) {
      out <- c(
        out,
        paste0(
          "names(dat)[names(dat) == ", .gqr_script_quote(rename_from), "] <- ",
          .gqr_script_quote(rename_to)
        )
      )
    }
  } else {
    filename <- .gqr_state_value(data_state, "name")
    if (is.null(filename) || !nzchar(filename)) {
      stop("File data sources must provide `state$data$name`.", call. = FALSE)
    }
    md5 <- .gqr_state_value(data_state, "md5", NA_character_)
    nr <- .gqr_state_value(data_state, "n_rows", NA_integer_)
    nc <- .gqr_state_value(data_state, "n_cols", NA_integer_)
    size <- .gqr_state_value(data_state, "size_bytes", NA_real_)

    if (isTRUE(verify_files)) {
      out <- c(out, paste0("data_file <- ", .gqr_script_quote(filename)))
      if (!is.na(md5) && nzchar(md5)) {
        out <- c(
          out,
          paste0("expected_data_md5 <- ", .gqr_script_quote(md5)),
          "actual_data_md5 <- unname(tools::md5sum(data_file))",
          "if (!identical(actual_data_md5, expected_data_md5)) {",
          '  warning("The checksum of ", data_file, " differs from the file used in the Shiny analysis.")',
          "}"
        )
      }
      out <- c(out, "dat <- GQR::gqr_read(data_file)")
    } else {
      out <- c(out, paste0("dat <- GQR::gqr_read(", .gqr_script_quote(filename), ")"))
    }

    if (is.finite(nr) && is.finite(nc)) {
      out <- c(out, .gqr_script_comment(paste0("File used in Shiny: ", nr, " rows x ", nc, " columns")))
    }
    if (is.finite(size)) {
      out <- c(out, .gqr_script_comment(paste0("File size: ", format(round(size / 1024^2, 3), trim = TRUE), " MiB")))
    }
  }

  column_renames <- .gqr_state_value(state, "column_renames")
  if (!is.null(column_renames)) {
    if (!is.data.frame(column_renames) ||
        !all(c("from", "to") %in% names(column_renames))) {
      stop("`state$column_renames` must contain `from` and `to` columns.", call. = FALSE)
    }

    if (nrow(column_renames) > 0L) {
      out <- c(
        out,
        "",
        "# Column names changed in the Shiny Data tab",
        paste0(
          "names(dat)[match(",
          .gqr_script_vector(as.character(column_renames$from)),
          ", names(dat))] <- ",
          .gqr_script_vector(as.character(column_renames$to))
        )
      )
    }
  }

  factor_cols <- as.character(.gqr_state_value(state, "factor_cols", character(0)))
  if (length(factor_cols) > 0L) {
    out <- c(
      out,
      "",
      "# Columns explicitly marked as factors in the Shiny Data tab",
      paste0(
        "dat[", .gqr_script_vector(factor_cols), "] <- lapply(dat[",
        .gqr_script_vector(factor_cols), "], factor)"
      )
    )
  }

  out <- c(
    out,
    "",
    paste0("analysis_cols <- ", .gqr_script_vector(as.character(state$analysis_cols))),
    paste0("covariate_cols <- ", .gqr_script_vector(as.character(.gqr_state_value(state, "covariate_cols", character(0))))),
    ""
  )

  transform <- .gqr_state_value(state, "transform", "none")
  if (identical(transform, "none")) {
    out <- c(out, "analysis_data <- dat")
  } else {
    out <- c(
      out,
      "analysis_data <- GQR::gqr_transform_data(",
      "  data = dat,",
      "  columns = analysis_cols,",
      paste0("  method = ", .gqr_script_quote(transform), ","),
      '  margin = "auto"',
      ")"
    )
  }

  pca_state <- .gqr_state_value(state, "pca")
  data_filter <- .gqr_state_value(state, "data_filter")
  if (is.null(data_filter) && !is.null(pca_state)) {
    data_filter <- .gqr_state_value(pca_state, "data_filter")
  }
  if (!is.null(data_filter)) {
    filters <- .gqr_state_value(data_filter, "filters")
    ids <- .gqr_state_value(data_filter, "ids")
    id_col_filter <- .gqr_state_value(data_filter, "id_col")
    if (length(filters) > 0L) {
      out <- c(out, "", "# Respondent filters selected on the Dummies tab", .gqr_script_filters("analysis_filters", filters))
    }
    if (length(filters) > 0L || length(ids) > 0L) {
      out <- c(
        out,
        "analysis_data <- GQR::gqr_filter_data(",
        "  data = analysis_data,",
        paste0("  id_col = ", .gqr_script_vector(id_col_filter), ","),
        paste0("  ids = ", .gqr_script_vector(ids), ","),
        paste0("  filters = ", if (length(filters) > 0L) "analysis_filters" else "NULL"),
        ")"
      )
    }
  }

  out <- c(out, "", "# ----------------------------------------------------------------", "# Dummy matrix", "# ----------------------------------------------------------------")

  group_source <- .gqr_state_value(state, "group_source")
  if (!is.null(group_source)) {
    nm <- .gqr_state_value(group_source, "original_name", "grouping file")
    md5 <- .gqr_state_value(group_source, "md5", NA_character_)
    out <- c(out, .gqr_script_comment(paste0("Grouping was initially loaded from: ", nm)))
    if (!is.na(md5) && nzchar(md5)) out <- c(out, .gqr_script_comment(paste0("Grouping-file MD5: ", md5)))
    out <- c(out, "# The final grouping is written explicitly below so GUI edits are reproduced.")
  }

  out <- c(out, .gqr_script_groups(.gqr_state_value(state, "groups")))

  dummy <- .gqr_state_value(state, "dummy", list(mode = if (is.null(state$groups)) "all" else "group_one_per"))
  mode <- .gqr_state_value(dummy, "mode", if (is.null(state$groups)) "all" else "group_one_per")
  patterns <- .gqr_state_value(dummy, "patterns")
  if (!is.null(patterns) && is.finite(patterns)) {
    out <- c(out, .gqr_script_comment(paste0("Synthetic combinations in this dummy matrix: ", format(patterns, big.mark = ",", scientific = FALSE))))
  }
  dummy_lines <- c(
    "D <- GQR::gqr_generate_dummies(",
    "  variables = analysis_cols,",
    paste0("  mode = ", .gqr_script_quote(mode), ",")
  )
  if (identical(mode, "group_one_per")) {
    dummy_lines <- c(dummy_lines, "  groups = groups,", paste0("  allow_ungrouped = ", if (isTRUE(.gqr_state_value(dummy, "allow_ungrouped", TRUE))) "TRUE" else "FALSE", ","))
  }
  if (identical(mode, "random")) {
    dummy_lines <- c(
      dummy_lines,
      paste0("  n_patterns = ", .gqr_script_vector(as.integer(.gqr_state_value(dummy, "n_patterns", 1000L))), ","),
      paste0("  prob = ", .gqr_script_vector(as.numeric(.gqr_state_value(dummy, "prob", 0.5))), ","),
      paste0("  seed = ", .gqr_script_vector(.gqr_state_value(dummy, "seed")), ",")
    )
  }
  dummy_lines <- c(
    dummy_lines,
    paste0("  include_empty = ", if (isTRUE(.gqr_state_value(dummy, "include_empty", TRUE))) "TRUE" else "FALSE", ","),
    paste0("  max_patterns = ", .gqr_script_vector(as.integer(.gqr_state_value(dummy, "max_patterns", 1000000L)))),
    ")"
  )
  out <- c(out, dummy_lines)
  out <- c(
    out,
    .gqr_script_dummies_plots(
      .gqr_state_value(plot_settings, "dummies"),
      has_groups = !is.null(state$groups) && nrow(state$groups) > 0L
    )
  )

  if (is.null(pca_state)) {
    out <- c(out, "", "# PCA has not yet been run in this Shiny session.")
  } else {
    out <- c(out, "", "# ----------------------------------------------------------------", "# Principal component analysis", "# ----------------------------------------------------------------")
    engine <- .gqr_state_value(pca_state, "engine", "design")
    id_col <- .gqr_state_value(pca_state, "id_col")
    n_components <- as.integer(.gqr_state_value(pca_state, "n_components", 5L))
    rotation <- .gqr_state_value(pca_state, "rotation", "none")
    center <- isTRUE(.gqr_state_value(pca_state, "center", TRUE))
    scale <- isTRUE(.gqr_state_value(pca_state, "scale", TRUE))

    if (identical(engine, "design")) {
      out <- c(
        out,
        "pca <- GQR::gqr_pca_design(",
        "  data = analysis_data,",
        "  D = D,",
        "  analysis_cols = analysis_cols,",
        paste0("  id_col = ", .gqr_script_vector(id_col), ","),
        paste0("  n_components = ", n_components, "L,"),
        paste0("  rotation = ", .gqr_script_quote(rotation), ","),
        paste0("  center = ", if (center) "TRUE" else "FALSE", ","),
        paste0("  scale = ", if (scale) "TRUE" else "FALSE", ","),
        '  na_action = "error"',
        ")"
      )
    } else {
      impute_mean <- isTRUE(.gqr_state_value(pca_state, "impute_mean", TRUE))
      out <- c(
        out,
        "prepared_for_w <- GQR::gqr_prepare_data(",
        "  data = analysis_data,",
        "  analysis_cols = analysis_cols,",
        paste0("  id_col = ", .gqr_script_vector(id_col)),
        ")",
        "",
        "W <- GQR::gqr_make_w(",
        "  data = prepared_for_w,",
        "  D = D,",
        paste0("  na_action = ", .gqr_script_quote(if (impute_mean) "mean" else "error")),
        ")",
        "",
        "pca <- GQR::gqr_pca(",
        "  W = W,",
        paste0("  n_components = ", n_components, "L,"),
        paste0("  rotation = ", .gqr_script_quote(rotation), ","),
        paste0("  center = ", if (center) "TRUE" else "FALSE", ","),
        paste0("  scale = ", if (scale) "TRUE" else "FALSE", ","),
        '  method = "correlation",',
        paste0("  impute = ", .gqr_script_quote(if (impute_mean) "mean" else "none")),
        ")"
      )
    }
    out <- c(out, .gqr_script_pca_plot())
  }

  statement <- .gqr_state_value(state, "statement_regression")
  if (!is.null(pca_state) && !is.null(statement) && isTRUE(.gqr_state_value(statement, "enabled", FALSE))) {
    components <- .gqr_state_value(statement, "components")
    out <- c(out, "", "# ----------------------------------------------------------------", "# Statement-Component Regression", "# ----------------------------------------------------------------")
    if (isTRUE(.gqr_state_value(statement, "include_raw", TRUE))) {
      out <- c(
        out,
        "statement_regression_raw <- GQR::gqr_regress_statements(",
        "  pca = pca,",
        "  D = D,",
        "  groups = groups,",
        paste0("  components = ", .gqr_script_vector(components), ","),
        "  standardise = FALSE",
        ")",
        ""
      )
    }
    if (isTRUE(.gqr_state_value(statement, "include_standardised", TRUE))) {
      out <- c(
        out,
        "statement_regression_standardised <- GQR::gqr_regress_statements(",
        "  pca = pca,",
        "  D = D,",
        "  groups = groups,",
        paste0("  components = ", .gqr_script_vector(components), ","),
        "  standardise = TRUE",
        ")"
      )
    }
  }

  if (!is.null(pca_state) && !is.null(statement) && isTRUE(.gqr_state_value(statement, "enabled", FALSE))) {
    out <- c(
      out,
      .gqr_script_statement_plot(
        .gqr_state_value(plot_settings, "statement"),
        groups = .gqr_state_value(state, "groups"),
        analysis_cols = as.character(state$analysis_cols)
      )
    )
  }

  respondent <- .gqr_state_value(state, "respondent_regression")
  if (!is.null(pca_state) && !is.null(respondent) && isTRUE(.gqr_state_value(respondent, "enabled", FALSE))) {
    id_col <- .gqr_state_value(respondent, "id_col", .gqr_state_value(pca_state, "id_col"))
    covariates <- .gqr_state_value(respondent, "covariates", character(0))
    components <- .gqr_state_value(respondent, "components", character(0))
    filters <- .gqr_state_value(respondent, "filters")
    ids <- .gqr_state_value(respondent, "ids")
    limit <- .gqr_state_value(respondent, "limit")

    out <- c(
      out,
      "",
      "# ----------------------------------------------------------------",
      "# Component-Covariate Regression",
      "# ----------------------------------------------------------------",
      paste0("respondent_covariates <- ", .gqr_script_vector(covariates)),
      paste0("respondent_id_col <- ", .gqr_script_vector(id_col)),
      "respondent_metadata <- analysis_data[, intersect(respondent_covariates, names(analysis_data)), drop = FALSE]",
      "if (!is.null(respondent_id_col) && respondent_id_col %in% names(analysis_data)) {",
      "  respondent_metadata[[respondent_id_col]] <- as.character(analysis_data[[respondent_id_col]])",
      "} else {",
      '  respondent_id_col <- "ID"',
      "  row_ids <- rownames(analysis_data)",
      "  if (!is.null(row_ids) && length(row_ids) == nrow(analysis_data) && !identical(row_ids, as.character(seq_len(nrow(analysis_data)))) && !anyDuplicated(row_ids)) {",
      "    respondent_metadata[[respondent_id_col]] <- as.character(row_ids)",
      "  } else {",
      '    respondent_metadata[[respondent_id_col]] <- sprintf(paste0("R%0", nchar(as.character(nrow(analysis_data))), "d"), seq_len(nrow(analysis_data)))',
      "  }",
      "}",
      "respondent_metadata <- respondent_metadata[!duplicated(respondent_metadata[[respondent_id_col]]), , drop = FALSE]"
    )

    if (length(filters) > 0L) {
      out <- c(out, .gqr_script_filters("respondent_filters", filters))
    }
    if (length(filters) > 0L || length(ids) > 0L) {
      out <- c(
        out,
        "respondent_metadata <- GQR::gqr_filter_data(",
        "  data = respondent_metadata,",
        "  id_col = respondent_id_col,",
        paste0("  ids = ", .gqr_script_vector(ids), ","),
        paste0("  filters = ", if (length(filters) > 0L) "respondent_filters" else "NULL"),
        ")"
      )
    }
    if (!is.null(limit) && is.finite(limit)) {
      out <- c(out, paste0("respondent_metadata <- head(respondent_metadata, ", as.integer(limit), "L)"))
    }

    out <- c(
      out,
      "pca_for_respondent_regression <- pca",
      "respondent_ids <- as.character(respondent_metadata[[respondent_id_col]])",
      "loading_index <- match(respondent_ids, rownames(pca_for_respondent_regression$loadings))",
      "keep <- !is.na(loading_index)",
      "respondent_metadata <- respondent_metadata[keep, , drop = FALSE]",
      "loading_index <- loading_index[keep]",
      "pca_for_respondent_regression$loadings <- pca_for_respondent_regression$loadings[loading_index, , drop = FALSE]",
      "rownames(pca_for_respondent_regression$loadings) <- as.character(respondent_metadata[[respondent_id_col]])",
      "",
      "respondent_regression <- GQR::gqr_regress_respondents(",
      "  pca = pca_for_respondent_regression,",
      "  metadata = respondent_metadata,",
      "  id_col = respondent_id_col,",
      "  covariates = respondent_covariates,",
      paste0("  components = ", .gqr_script_vector(components)),
      ")"
    )
  }

  if (!is.null(pca_state)) {
    out <- c(
      out,
      .gqr_script_respondent_plots(
        .gqr_state_value(plot_settings, "respondent"),
        as.character(.gqr_state_value(state, "covariate_cols", character(0)))
      )
    )
  }

  if (isTRUE(include_session_info)) {
    out <- c(out, "", "# ----------------------------------------------------------------", "# Session information", "# ----------------------------------------------------------------", "sessionInfo()")
  }

  paste(out, collapse = "\n")
}
