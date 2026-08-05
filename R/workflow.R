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
#'
#' @return An object of class `gqr_analysis` with the prepared data, design-size
#'   estimate, dummy matrix `D`, synthetic evaluation matrix `W`, PCA result,
#'   and optional statement and respondent regression results.
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
#' @references
#' Dentinho, T. P., Kourtit, K., & Nijkamp, P. (2023). Generalized Q analysis
#' as a new tool in social science research: A pedagogical introduction.
#' *Eastern Journal of European Studies*, **14**(2), 5--21.
#' \doi{10.47743/ejes-2023-0201}
#'
#' @seealso [gqr_methodology], [gqr_prepare_data()],
#'   [gqr_generate_dummies()], [gqr_make_w()], [gqr_pca()],
#'   [gqr_regress_statements()], [gqr_regress_respondents()]
#'
#' @examples
#' dat <- gqr_example_data("dummy_data")
#'
#' fit <- gqr_analysis(
#'   data = dat,
#'   analysis_cols = paste0("Q", 1:9),
#'   id_col = "Respondent",
#'   dummy_mode = "all",
#'   n_components = 3,
#'   rotation = "varimax",
#'   respondent_regression = FALSE
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
    standardise_statement_regression = TRUE) {

  dummy_mode <- match.arg(dummy_mode)
  na_action <- match.arg(na_action)
  rotation <- match.arg(rotation)
  pca_method <- match.arg(pca_method)
  pca_impute <- match.arg(pca_impute)

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
    max_patterns = max_patterns
  )

  W <- gqr_make_w(
    data = prepared,
    D = D,
    na_action = na_action
  )

  pca <- gqr_pca(
    W = W,
    n_components = n_components,
    rotation = rotation,
    center = center,
    scale = scale,
    method = pca_method,
    impute = pca_impute
  )

  statement_models <- NULL
  if (isTRUE(statement_regression)) {
    statement_models <- gqr_regress_statements(
      pca = pca,
      D = D,
      groups = if (dummy_mode == "group_one_per") groups else NULL,
      standardise = standardise_statement_regression
    )
  }

  respondent_models <- NULL
  if (isTRUE(respondent_regression)) {
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

  structure(
    list(
      call = match.call(),
      prepared = prepared,
      design = design,
      D = D,
      W = W,
      pca = pca,
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
  cat("  Respondents:", ncol(x$W), "\n")
  cat("  Analysis variables:", ncol(x$D), "\n")
  cat("  Synthetic combinations:", nrow(x$D), "\n")
  cat("  Components:", ncol(x$pca$scores), "\n")
  cat("  PCA method:", x$pca$method, "\n")
  cat("  Rotation:", x$pca$rotation, "\n")
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
      respondents = ncol(object$W),
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
