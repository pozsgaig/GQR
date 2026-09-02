# End-to-end workflow -----------------------------------------------------

#' Run a complete Generalised Q analysis
#'
#' @description
#' Runs data preparation, dummy-design generation, construction of the
#' synthetic evaluation matrix, PCA, and optional statement and respondent
#' regressions. This is the main programmatic entry point to GQR.
#'
#' @param data Respondent-level data frame.
#' @param analysis_cols Character vector naming the numeric simple-statement
#'   variables.
#' @param id_col Optional column containing unique respondent identifiers.
#' @param covariate_cols Optional respondent-level variables used in respondent
#'   regressions.
#' @param transform Transformation applied to the analysis columns: `"none"`,
#'   `"standardise"`, `"normalise"`, `"relative"`, or `"entropy"`.
#' @param transform_margin Transformation margin: `"auto"`, `"columns"`, or
#'   `"rows"`.
#' @param ids Optional respondent IDs to retain.
#' @param filters Optional named list of respondent filters; see
#'   [gqr_filter_data()].
#' @param dummy_mode Synthetic-statement design: `"all"`,
#'   `"group_one_per"`, or `"random"`.
#' @param groups Named list or data frame defining groups for
#'   `dummy_mode = "group_one_per"`.
#' @param n_patterns Number of sampled combinations in random mode.
#' @param prob Probability that a statement is active in random mode.
#' @param seed Optional random seed.
#' @param include_empty Whether to retain the all-zero combination.
#' @param allow_ungrouped Whether grouped mode may leave analysis variables
#'   permanently inactive.
#' @param max_patterns Maximum number of design rows allowed before allocation.
#' @param na_action Handling of non-finite values during W construction:
#'   `"error"`, `"mean"`, or `"zero"`.
#' @param n_components Number of PCA components to retain.
#' @param rotation PCA rotation: `"none"` or `"varimax"`.
#' @param center,scale Logical PCA centring and scaling settings.
#' @param pca_method `"prcomp"` or correlation-based scoring.
#' @param pca_impute PCA missing-value handling: `"none"` or `"mean"`.
#' @param statement_regression Whether to regress combination scores on the
#'   statement-design matrix.
#' @param respondent_regression Whether to regress respondent loadings on
#'   covariates.
#' @param standardise_statement_regression Whether statement regressions use
#'   standardised responses and predictors.
#' @param pca_engine PCA computation engine. `"matrix"` first materialises W;
#'   `"design"` uses [gqr_pca_design()] and avoids the full W matrix;
#'   `"auto"` selects the compact design engine for ordinary PCA when W is
#'   estimated to exceed `compact_threshold_mb`.
#' @param materialise_w Whether the returned object should contain the complete
#'   W matrix. `"auto"` omits W when the compact PCA engine is used and the
#'   estimated W size exceeds `w_memory_limit_mb`.
#' @param compact_threshold_mb Estimated W size above which `"auto"` uses the
#'   compact design PCA engine.
#' @param w_memory_limit_mb Estimated W size above which `"auto"` avoids
#'   materialising W when compact PCA is available.
#' @param progress Optional callback receiving `value` (0--1) and `message`.
#' @param cancel Optional zero-argument cancellation callback.
#'
#' @return An object of class `gqr_analysis` with the prepared data, design-size
#'   estimate, dummy matrix `D`, PCA result, optional statement and respondent
#'   regression results, and (when materialised) the synthetic evaluation
#'   matrix `W`. For large analyses using the compact engine, `W` can be `NULL`
#'   by design because it is not needed for PCA.
#'
#' @details
#' GQR follows the matrix orientation of Generalised Q analysis: rows of `D`
#' and `W` are synthetic statements and columns of `W` are respondents. PCA
#' scores therefore describe combinations, while loadings describe
#' respondents. In grouped designs, one statement per group is omitted as the
#' regression baseline.
#'
#' Full designs grow exponentially. `gqr_analysis()` calls
#' [gqr_estimate_design()] before allocation, but users should still choose a
#' defensible `max_patterns` and consider grouped or random designs.
#'
#' For ordinary PCA, the compact engine uses the identity `W = D V^T` and the
#' fact that the rank of W cannot exceed the number of original statements.
#' It therefore avoids a singular-value decomposition of the complete W matrix
#' and can also avoid storing W altogether. This is exact for the ordinary
#' `prcomp` workflow; correlation/SPSS-style scoring still uses the full W
#' matrix.
#'
#' @references
#' Dentinho, T. P., Kourtit, K., & Nijkamp, P. (2023). Generalized Q analysis
#' as a new tool in social science research: A pedagogical introduction.
#' *Eastern Journal of European Studies*, **14**(2), 5--21.
#' \doi{10.47743/ejes-2023-0201}
#'
#' @seealso [gqr_methodology], [gqr_prepare_data()],
#'   [gqr_generate_dummies()], [gqr_make_w()], [gqr_pca()], [gqr_pca_design()],
#'   [gqr_regress_statements()], [gqr_regress_respondents()]
#'
#' @examples
#' dat <- gqr_example_data("dummy_data")
#' roles <- gqr_example_roles("dummy_data")
#'
#' fit <- gqr_analysis(
#'   data = dat,
#'   analysis_cols = roles$analysis_cols,
#'   id_col = roles$id_col,
#'   covariate_cols = roles$covariate_cols,
#'   dummy_mode = "all",
#'   n_components = 3,
#'   rotation = "varimax"
#' )
#'
#' fit
#' summary(fit)$variance
#'
#' groups <- data.frame(
#'   group = rep(c("Question 1", "Question 2", "Question 3"), each = 3),
#'   variable = paste0("Q", 1:9)
#' )
#'
#' grouped_fit <- gqr_analysis(
#'   dat,
#'   analysis_cols = paste0("Q", 1:9),
#'   id_col = "Respondent",
#'   dummy_mode = "group_one_per",
#'   groups = groups,
#'   n_components = 3,
#'   respondent_regression = FALSE
#' )
#'
#' dim(grouped_fit$D)
#'
#' @export
gqr_analysis <- function(
    data,
    analysis_cols,
    id_col = NULL,
    covariate_cols = NULL,
    transform = "none",
    transform_margin = "auto",
    ids = NULL,
    filters = NULL,
    dummy_mode = c("all", "group_one_per", "random"),
    groups = NULL,
    n_patterns = 1000L,
    prob = 0.5,
    seed = NULL,
    include_empty = TRUE,
    allow_ungrouped = FALSE,
    max_patterns = 1000000L,
    na_action = c("error", "mean", "zero"),
    n_components = 5L,
    rotation = c("none", "varimax"),
    center = TRUE,
    scale = TRUE,
    pca_method = c("prcomp", "correlation"),
    pca_impute = c("none", "mean"),
    statement_regression = TRUE,
    respondent_regression = !is.null(id_col) && length(covariate_cols) > 0L,
    standardise_statement_regression = TRUE,
    pca_engine = c("auto", "matrix", "design"),
    materialise_w = c("auto", "always", "never"),
    compact_threshold_mb = 64,
    w_memory_limit_mb = 64,
    progress = NULL,
    cancel = NULL) {

  dummy_mode <- match.arg(dummy_mode)
  na_action <- match.arg(na_action)
  rotation <- match.arg(rotation)
  pca_method <- match.arg(pca_method)
  pca_impute <- match.arg(pca_impute)
  pca_engine <- match.arg(pca_engine)
  materialise_w <- match.arg(materialise_w)

  .gqr_check_cancel(cancel)
  .gqr_report_progress(progress, 0.01, "Preparing data")

  prepared <- gqr_prepare_data(
    data = data,
    analysis_cols = analysis_cols,
    covariate_cols = covariate_cols,
    id_col = id_col,
    transform = transform,
    transform_margin = transform_margin,
    ids = ids,
    filters = filters
  )

  design <- gqr_estimate_design(
    variables = prepared$analysis_cols,
    mode = dummy_mode,
    groups = groups,
    n_patterns = n_patterns,
    n_respondents = nrow(prepared$data),
    allow_ungrouped = allow_ungrouped
  )

  D <- gqr_generate_dummies(
    variables = prepared$analysis_cols,
    mode = dummy_mode,
    groups = groups,
    n_patterns = n_patterns,
    prob = prob,
    seed = seed,
    include_empty = include_empty,
    allow_ungrouped = allow_ungrouped,
    max_patterns = max_patterns,
    progress = function(value, message) {
      .gqr_report_progress(
        progress,
        0.05 + 0.20 * value,
        message
      )
    },
    cancel = cancel
  )

  engine_used <- pca_engine
  if (engine_used == "auto") {
    engine_used <- if (
      pca_method == "prcomp" &&
      is.finite(design$w_memory_mb) &&
      design$w_memory_mb >= compact_threshold_mb
    ) {
      "design"
    } else {
      "matrix"
    }
  }

  if (engine_used == "design" && pca_method != "prcomp") {
    if (pca_engine == "design") {
      stop(
        "`pca_engine = \"design\"` currently supports only `pca_method = \"prcomp\"`.",
        call. = FALSE
      )
    }
    engine_used <- "matrix"
  }

  materialise_w_used <- switch(
    materialise_w,
    always = TRUE,
    never = FALSE,
    auto = engine_used == "matrix" ||
      !is.finite(design$w_memory_mb) ||
      design$w_memory_mb <= w_memory_limit_mb
  )

  if (!materialise_w_used && engine_used == "matrix") {
    stop(
      "`materialise_w = \"never\"` requires the compact design PCA engine.",
      call. = FALSE
    )
  }

  W <- NULL
  if (!materialise_w_used) {
    .gqr_report_progress(progress, 0.48, "Skipping full W materialisation")
  }
  if (materialise_w_used) {
    W <- gqr_make_w(
      data = prepared,
      D = D,
      na_action = na_action,
      algorithm = if (is.null(progress) && is.null(cancel)) "matmul" else "chunked",
      progress = function(value, message) {
        .gqr_report_progress(progress, 0.25 + 0.25 * value, message)
      },
      cancel = cancel
    )
  }

  if (engine_used == "design") {
    pca <- gqr_pca_design(
      data = prepared,
      D = D,
      n_components = n_components,
      rotation = rotation,
      center = center,
      scale = scale,
      na_action = na_action,
      progress = function(value, message) {
        .gqr_report_progress(progress, 0.50 + 0.30 * value, message)
      },
      cancel = cancel
    )
  } else {
    .gqr_report_progress(progress, 0.55, "Running PCA")
    .gqr_check_cancel(cancel)
    pca <- gqr_pca(
      W = W,
      n_components = n_components,
      rotation = rotation,
      center = center,
      scale = scale,
      method = pca_method,
      impute = pca_impute
    )
    .gqr_report_progress(progress, 0.80, "PCA ready")
  }

  .gqr_check_cancel(cancel)
  statement_models <- NULL
  if (isTRUE(statement_regression)) {
    .gqr_report_progress(progress, 0.84, "Fitting statement regressions")
    statement_models <- gqr_regress_statements(
      pca = pca,
      D = D,
      groups = if (dummy_mode == "group_one_per") groups else NULL,
      standardise = standardise_statement_regression
    )
  }

  .gqr_check_cancel(cancel)
  respondent_models <- NULL
  if (isTRUE(respondent_regression)) {
    .gqr_report_progress(progress, 0.92, "Fitting respondent regressions")
    if (is.null(id_col) || length(covariate_cols) == 0L) {
      stop(
        "Respondent regression requires `id_col` and at least one covariate.",
        call. = FALSE
      )
    }

    respondent_models <- gqr_regress_respondents(
      pca = pca,
      metadata = prepared$data,
      id_col = id_col,
      covariates = covariate_cols
    )
  }

  .gqr_check_cancel(cancel)
  .gqr_report_progress(progress, 1, "Generalised Q analysis ready")

  structure(
    list(
      call = match.call(),
      prepared = prepared,
      design = design,
      D = D,
      W = W,
      pca = pca,
      pca_engine = engine_used,
      W_materialised = !is.null(W),
      statement_regression = statement_models,
      respondent_regression = respondent_models
    ),
    class = "gqr_analysis"
  )
}

#' Print a GQR analysis summary
#'
#' @param x A `gqr_analysis` object.
#' @param ... Additional arguments, currently unused.
#' @return `x`, invisibly.
#' @rdname gqr_analysis
#' @export
print.gqr_analysis <- function(x, ...) {
  cat("Generalised Q analysis\n")
  cat("  Respondents:", nrow(x$prepared$data), "\n")
  cat("  Analysis variables:", ncol(x$D), "\n")
  cat("  Synthetic combinations:", nrow(x$D), "\n")
  cat("  Components:", ncol(x$pca$scores), "\n")
  cat("  PCA method:", x$pca$method, "\n")
  cat("  Rotation:", x$pca$rotation, "\n")
  cat("  PCA engine:", if (is.null(x$pca_engine)) "matrix" else x$pca_engine, "\n")
  cat("  W materialised:", !is.null(x$W), "\n")
  invisible(x)
}

#' Summarise a GQR analysis
#'
#' @param object A `gqr_analysis` object.
#' @param ... Additional arguments, currently unused.
#' @return A list containing analysis dimensions, explained variance, and any removed respondents.
#' @rdname gqr_analysis
#' @export
summary.gqr_analysis <- function(object, ...) {
  variance <- data.frame(
    component = names(object$pca$eigenvalues),
    eigenvalue = unname(object$pca$eigenvalues),
    variance_percent = unname(object$pca$variance_explained),
    cumulative_percent = unname(object$pca$cumulative_variance),
    row.names = NULL
  )

  result <- list(
    dimensions = c(
      respondents = nrow(object$prepared$data),
      variables = ncol(object$D),
      combinations = nrow(object$D),
      components = ncol(object$pca$scores)
    ),
    variance = variance,
    respondents_removed = object$pca$respondents_removed
  )
  class(result) <- "summary.gqr_analysis"
  result
}
