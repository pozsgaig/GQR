# Component regressions ---------------------------------------------------

.gqr_statement_predictors <- function(D, groups = NULL, baselines = NULL) {
  variables <- colnames(D)
  variable_sd <- apply(D, 2L, stats::sd)
  constant <- variables[!is.finite(variable_sd) | variable_sd == 0]

  baseline_variables <- character()

  if (!is.null(groups)) {
    grouping <- .gqr_prepare_groups(
      groups,
      variables,
      allow_ungrouped = TRUE
    )

    if (is.null(baselines)) {
      baseline_variables <- vapply(
        grouping$split,
        function(x) x[1L],
        character(1)
      )
    } else {
      baselines <- as.character(baselines)

      if (!is.null(names(baselines)) && all(names(baselines) != "")) {
        missing_groups <- setdiff(names(grouping$split), names(baselines))
        if (length(missing_groups) > 0L) {
          stop(
            "No baseline supplied for groups: ",
            paste(missing_groups, collapse = ", "),
            call. = FALSE
          )
        }
        baseline_variables <- unname(baselines[names(grouping$split)])
      } else {
        baseline_variables <- baselines
      }

      for (g in names(grouping$split)) {
        selected <- baseline_variables[baseline_variables %in% grouping$split[[g]]]
        if (length(selected) != 1L) {
          stop(
            "Supply exactly one valid baseline for group `", g, "`.",
            call. = FALSE
          )
        }
      }
    }
  }

  predictors <- setdiff(variables, union(constant, baseline_variables))

  list(
    predictors = predictors,
    baselines = baseline_variables,
    constants = constant
  )
}

.gqr_statement_model <- function(response, D, predictors, standardise) {
  X <- D[, predictors, drop = FALSE]
  y <- as.numeric(response)

  if (standardise) {
    y <- .gqr_scale_vector(y)
    for (j in seq_len(ncol(X))) {
      X[, j] <- .gqr_scale_vector(as.numeric(X[, j]))
    }
  }

  safe_names <- make.names(predictors, unique = TRUE)
  model_data <- data.frame(
    .response = y,
    X,
    check.names = FALSE
  )
  names(model_data) <- c(".response", safe_names)

  model <- stats::lm(.response ~ ., data = model_data)
  attr(model, "term_map") <- stats::setNames(predictors, safe_names)
  model
}

.gqr_restore_statement_terms <- function(coefficient_table, model) {
  term_map <- attr(model, "term_map")
  if (is.null(term_map) || nrow(coefficient_table) == 0L) {
    return(coefficient_table)
  }

  is_predictor <- coefficient_table$term %in% names(term_map)
  coefficient_table$term[is_predictor] <-
    unname(term_map[coefficient_table$term[is_predictor]])
  coefficient_table
}

#' Regress component scores on simple-statement dummies
#'
#' @description
#' Fits one linear model per selected component to identify which simple
#' statements characterise synthetic combinations with high or low component
#' scores.
#'
#' @param pca A result from [gqr_pca()] or [gqr_pca_design()].
#' @param D The design matrix used to construct the corresponding W matrix.
#' @param groups Optional grouped-design specification.
#' @param components Optional component names; all retained components are used
#'   by default.
#' @param standardise Whether the response and non-constant predictors are
#'   standardised before fitting.
#' @param baselines Optional statement baselines for grouped designs. A named
#'   vector can specify one baseline per group.
#'
#' @return An object of class `gqr_statement_regression` containing fitted
#'   models, a combined coefficient table, predictors, omitted baselines,
#'   constant variables, and the standardisation setting.
#'
#' @details
#' In grouped one-per-group designs, the dummies within a group sum to one and
#' cannot all be fitted with an intercept. GQR therefore omits one statement per
#' group as a reference category. Coefficients are quantitative aids to naming
#' and interpreting components; they are not automatic component labels.
#'
#' @references
#' Dentinho, T. P., Kourtit, K., & Nijkamp, P. (2023). Generalized Q analysis
#' as a new tool in social science research: A pedagogical introduction.
#' *Eastern Journal of European Studies*, **14**(2), 5--21.
#' \doi{10.47743/ejes-2023-0201}
#' @examples
#' dat <- gqr_example_data("dummy_data")
#' fit <- gqr_analysis(
#'   dat,
#'   analysis_cols = paste0("Q", 1:9),
#'   id_col = "Respondent",
#'   n_components = 2,
#'   respondent_regression = FALSE
#' )
#' head(fit$statement_regression$coefficients)
#'
#' @export
gqr_regress_statements <- function(
    pca,
    D,
    groups = NULL,
    components = NULL,
    standardise = TRUE,
    baselines = NULL) {

  if (is.null(pca$scores)) {
    stop("`pca` must be a result from `gqr_pca()`.", call. = FALSE)
  }
  if (!is.matrix(D) || is.null(colnames(D))) {
    stop("`D` must be a named matrix.", call. = FALSE)
  }

  scores <- as.matrix(pca$scores)
  if (nrow(scores) != nrow(D)) {
    stop("`D` and PCA scores have different numbers of combinations.", call. = FALSE)
  }

  if (!is.null(rownames(D)) && !is.null(rownames(scores))) {
    if (!setequal(rownames(D), rownames(scores))) {
      stop("`D` and PCA scores have different combination names.", call. = FALSE)
    }
    D <- D[rownames(scores), , drop = FALSE]
  }

  if (is.null(components)) {
    components <- colnames(scores)
  }
  components <- intersect(as.character(components), colnames(scores))
  if (length(components) == 0L) {
    stop("No valid components selected.", call. = FALSE)
  }

  selection <- .gqr_statement_predictors(D, groups, baselines)
  if (length(selection$predictors) == 0L) {
    stop("No non-constant, non-baseline statement predictors remain.", call. = FALSE)
  }

  models <- lapply(
    components,
    function(component) {
      .gqr_statement_model(
        response = scores[, component],
        D = D,
        predictors = selection$predictors,
        standardise = standardise
      )
    }
  )
  names(models) <- components

  coefficients <- do.call(
    rbind,
    lapply(
      components,
      function(component) {
        table <- .gqr_tidy_lm(models[[component]], component)
        .gqr_restore_statement_terms(table, models[[component]])
      }
    )
  )
  rownames(coefficients) <- NULL

  structure(
    list(
      models = models,
      coefficients = coefficients,
      predictors = selection$predictors,
      baselines = selection$baselines,
      constants = selection$constants,
      standardised = isTRUE(standardise)
    ),
    class = "gqr_statement_regression"
  )
}

#' Regress respondent loadings on covariates
#'
#' @description
#' Fits one model per selected component to relate respondent loadings to
#' demographic, geographic, behavioural, or other respondent-level variables.
#'
#' @param pca A result from [gqr_pca()] or [gqr_pca_design()].
#' @param metadata Respondent-level data containing identifiers and covariates.
#' @param id_col Name of the column matching respondent IDs in the PCA loadings.
#' @param covariates Character vector naming predictor columns.
#' @param components Optional component names; all retained components are used
#'   by default.
#'
#' @return An object of class `gqr_respondent_regression` containing the aligned
#'   modelling data, fitted models, combined coefficient table, and usable
#'   covariate names.
#'
#' @details
#' Respondent identifiers must be unique and complete. Covariates without
#' variation are excluded, and at least one usable covariate must remain.
#' Factors are represented through the standard treatment contrasts used by
#' [stats::lm()]. These regressions support interpretation but do not establish
#' causation.
#'
#' @references
#' Dentinho, T. P., Kourtit, K., & Nijkamp, P. (2023). Generalized Q analysis
#' as a new tool in social science research: A pedagogical introduction.
#' *Eastern Journal of European Studies*, **14**(2), 5--21.
#' \doi{10.47743/ejes-2023-0201}
#' @examples
#' dat <- gqr_example_data("dummy_data")
#' fit <- gqr_analysis(
#'   dat,
#'   analysis_cols = paste0("Q", 1:9),
#'   id_col = "Respondent",
#'   n_components = 2,
#'   statement_regression = FALSE,
#'   respondent_regression = FALSE
#' )
#'
#' respondent_models <- gqr_regress_respondents(
#'   pca = fit$pca,
#'   metadata = dat,
#'   id_col = "Respondent",
#'   covariates = c("Numeric_covariate", "Factor_covariate")
#' )
#' head(respondent_models$coefficients)
#'
#' @export
gqr_regress_respondents <- function(
    pca,
    metadata,
    id_col,
    covariates,
    components = NULL) {

  if (is.null(pca$loadings)) {
    stop("`pca` must be a result from `gqr_pca()`.", call. = FALSE)
  }

  metadata <- .gqr_check_data(metadata)
  id_col <- .gqr_check_columns(metadata, id_col, "id_col")
  covariates <- .gqr_check_columns(metadata, covariates, "covariates")

  if (length(id_col) != 1L) {
    stop("`id_col` must name exactly one column.", call. = FALSE)
  }
  if (length(covariates) == 0L) {
    stop("Select at least one covariate.", call. = FALSE)
  }

  ids <- as.character(metadata[[id_col]])
  if (anyNA(ids) || anyDuplicated(ids)) {
    stop("The metadata ID column must be complete and unique.", call. = FALSE)
  }

  loadings <- as.data.frame(pca$loadings, check.names = FALSE)
  loadings$.gqr_id <- rownames(pca$loadings)

  metadata$.gqr_id <- ids
  keep <- match(loadings$.gqr_id, metadata$.gqr_id)
  if (anyNA(keep)) {
    stop(
      "Metadata are missing respondents present in PCA loadings: ",
      paste(loadings$.gqr_id[is.na(keep)], collapse = ", "),
      call. = FALSE
    )
  }

  model_data <- cbind(
    loadings,
    metadata[keep, covariates, drop = FALSE]
  )

  if (is.null(components)) {
    components <- colnames(pca$loadings)
  }
  components <- intersect(as.character(components), colnames(pca$loadings))
  if (length(components) == 0L) {
    stop("No valid components selected.", call. = FALSE)
  }

  usable_covariates <- covariates[vapply(
    covariates,
    function(column) {
      x <- model_data[[column]]
      if (is.numeric(x)) {
        length(unique(x[is.finite(x)])) >= 2L
      } else {
        length(unique(x[!is.na(x)])) >= 2L
      }
    },
    logical(1)
  )]

  if (length(usable_covariates) == 0L) {
    stop("No covariates have enough variation to fit a model.", call. = FALSE)
  }

  safe_names <- make.names(
    c(components, usable_covariates),
    unique = TRUE
  )
  safe_components <- safe_names[seq_along(components)]
  safe_covariates <- safe_names[
    length(components) + seq_along(usable_covariates)
  ]
  component_map <- stats::setNames(components, safe_components)
  covariate_map <- stats::setNames(usable_covariates, safe_covariates)

  safe_data <- data.frame(
    model_data[components],
    model_data[usable_covariates],
    check.names = FALSE
  )
  names(safe_data) <- c(safe_components, safe_covariates)

  models <- lapply(
    safe_components,
    function(component) {
      formula <- stats::reformulate(
        termlabels = safe_covariates,
        response = component
      )
      stats::lm(formula, data = safe_data)
    }
  )
  names(models) <- components

  coefficients <- do.call(
    rbind,
    lapply(
      seq_along(models),
      function(i) {
        component <- names(models)[i]
        table <- .gqr_tidy_lm(models[[i]], component)

        for (safe_name in names(covariate_map)) {
          matching <- startsWith(table$term, safe_name)
          suffix <- substring(
            table$term[matching],
            nchar(safe_name) + 1L
          )
          table$term[matching] <- paste0(
            covariate_map[[safe_name]],
            suffix
          )
        }

        table
      }
    )
  )
  rownames(coefficients) <- NULL

  structure(
    list(
      data = model_data,
      models = models,
      coefficients = coefficients,
      covariates = usable_covariates
    ),
    class = "gqr_respondent_regression"
  )
}

