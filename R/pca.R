# Principal component analysis -------------------------------------------

#' Principal component analysis of a Generalised Q matrix
#'
#' @description
#' Extracts common respondent structures from `W` and optionally applies an
#' orthogonal Varimax rotation.
#'
#' @param W Numeric W matrix with synthetic combinations in rows and respondents
#'   in columns.
#' @param n_components Number of components to retain. If `NULL`, all positive
#'   components are retained for `method = "prcomp"`; the Kaiser rule is used
#'   for `method = "correlation"`.
#' @param rotation One of `"none"` or `"varimax"`.
#' @param center,scale Logical centring and scaling settings for ordinary PCA.
#' @param method One of `"prcomp"` or `"correlation"`. The latter reproduces
#'   the SPSS-style workflow of the original GQR application using a smoothed
#'   respondent correlation matrix, [psych::principal()], and regression-type
#'   scores.
#' @param impute One of `"none"` or `"mean"` for non-finite W values.
#' @param remove_constant Whether zero-variance respondent columns are removed.
#' @param tolerance Numerical tolerance used for ranks and matrix inversion.
#'
#' @return An object of class `gqr_pca_result` containing eigenvalues, variance
#'   percentages, cumulative variance, respondent loadings, synthetic-statement
#'   scores, unrotated results, the rotation matrix, and the names of retained
#'   and removed respondents.
#'
#' @details
#' GQR applies PCA to `W` in its documented orientation: combinations are
#' observations and respondents are variables. Consequently, score rows align
#' with `D`, while loading rows align with respondent identifiers. Transposing W
#' would answer a different analytical question.
#'
#' Varimax is orthogonal. It changes the component orientation but not the
#' retained subspace. For `method = "correlation"`, GQR deliberately uses the
#' same `psych::principal()` implementation as the original Shiny workflow so
#' that rotated component ordering, signs, and downstream statement-regression
#' coefficients remain compatible with earlier GQR/SPSS-style analyses.
#' Component retention and scaling should be justified from the research design
#' rather than selected solely from defaults.
#'
#' @references
#' Dentinho, T. P., Kourtit, K., & Nijkamp, P. (2023). Generalized Q analysis
#' as a new tool in social science research: A pedagogical introduction.
#' *Eastern Journal of European Studies*, **14**(2), 5--21.
#' \doi{10.47743/ejes-2023-0201}
#' @examples
#' dat <- gqr_example_data("dummy_data")
#' prepared <- gqr_prepare_data(dat, paste0("Q", 1:9), id_col = "Respondent")
#' D <- gqr_generate_dummies(prepared$analysis_cols)
#' W <- gqr_make_w(prepared, D = D)
#'
#' pca <- gqr_pca(W, n_components = 3, rotation = "varimax")
#' pca$variance_explained
#' head(pca$loadings)
#' head(pca$scores)
#'
#' @export
gqr_pca <- function(
    W,
    n_components = NULL,
    rotation = c("none", "varimax"),
    center = TRUE,
    scale = TRUE,
    method = c("prcomp", "correlation"),
    impute = c("none", "mean"),
    remove_constant = TRUE,
    tolerance = 1e-08) {

  if (!is.matrix(W) && !is.data.frame(W)) {
    stop("`W` must be a matrix or data frame.", call. = FALSE)
  }

  W <- as.matrix(W)
  storage.mode(W) <- "double"

  if (nrow(W) < 2L || ncol(W) < 2L) {
    stop("`W` must have at least two rows and two columns.", call. = FALSE)
  }

  rotation <- match.arg(rotation)
  method <- match.arg(method)
  impute <- match.arg(impute)

  if (is.null(colnames(W))) {
    colnames(W) <- paste0("R", seq_len(ncol(W)))
  }
  if (is.null(rownames(W))) {
    rownames(W) <- paste0("S", seq_len(nrow(W)))
  }

  if (any(!is.finite(W))) {
    if (impute == "none") {
      stop(
        "`W` contains missing or non-finite values. Use `impute = \"mean\"` or prepare complete data.",
        call. = FALSE
      )
    }
    W <- .gqr_impute_columns(W)
  }

  respondent_sd <- apply(W, 2L, stats::sd)
  constant <- !is.finite(respondent_sd) | respondent_sd <= tolerance

  if (any(constant) && !remove_constant) {
    stop(
      "W contains zero-variance respondent columns: ",
      paste(colnames(W)[constant], collapse = ", "),
      call. = FALSE
    )
  }

  removed <- colnames(W)[constant]
  W_used <- W[, !constant, drop = FALSE]

  if (ncol(W_used) < 2L) {
    stop("Fewer than two non-constant respondent columns remain.", call. = FALSE)
  }

  if (!is.null(n_components)) {
    n_components <- as.integer(n_components)
    if (length(n_components) != 1L ||
        is.na(n_components) ||
        n_components < 1L) {
      stop("`n_components` must be NULL or one positive integer.", call. = FALSE)
    }
  }

  if (method == "prcomp") {
    fit <- stats::prcomp(
      W_used,
      center = isTRUE(center),
      scale. = isTRUE(scale)
    )

    eigenvalues_all <- fit$sdev^2
    available <- sum(eigenvalues_all > tolerance)
    if (available < 1L) {
      stop("PCA found no component with positive variance.", call. = FALSE)
    }
    if (is.null(n_components)) {
      n_components <- available
    }
    n_components <- max(1L, min(as.integer(n_components), available))

    eigenvalues <- eigenvalues_all[seq_len(n_components)]
    explained <- 100 * eigenvalues / sum(eigenvalues_all)

    loadings_unrotated <- fit$rotation[, seq_len(n_components), drop = FALSE]
    scores_unrotated <- fit$x[, seq_len(n_components), drop = FALSE]

    if (rotation == "varimax" && n_components > 1L) {
      rotated <- stats::varimax(loadings_unrotated)
      rotation_matrix <- rotated$rotmat
      loadings <- unclass(rotated$loadings)
      scores <- scores_unrotated %*% rotation_matrix
    } else {
      rotation_matrix <- diag(n_components)
      loadings <- loadings_unrotated
      scores <- scores_unrotated
    }
  } else {
    if (!isTRUE(center) || !isTRUE(scale)) {
      warning(
        "Correlation-based scoring always centres and scales W.",
        call. = FALSE
      )
    }

    # Reproduce the SPSS-style workflow used by the original GQR/Shiny
    # implementation.  In particular, psych::principal() applies the same
    # component ordering/sign convention and Varimax implementation that the
    # earlier app used.  Replacing this with eigen() + stats::varimax() produces
    # an equivalent subspace, but can materially change the displayed rotated
    # component order and therefore the downstream statement regressions.
    R_raw <- stats::cor(W_used, use = "complete.obs")
    R <- psych::cor.smooth(R_raw)

    eig_values <- eigen(R, symmetric = TRUE, only.values = TRUE)$values
    available <- sum(eig_values > tolerance)
    if (available < 1L) {
      stop("Correlation PCA found no component with positive variance.", call. = FALSE)
    }

    if (is.null(n_components)) {
      n_components <- max(1L, sum(eig_values > 1))
    }
    n_components <- max(1L, min(as.integer(n_components), available))

    fit_unrotated <- suppressWarnings(
      psych::principal(
        r = R,
        nfactors = n_components,
        rotate = "none",
        scores = FALSE,
        missing = FALSE
      )
    )

    fit_selected <- if (rotation == "varimax" && n_components > 1L) {
      suppressWarnings(
        psych::principal(
          r = R,
          nfactors = n_components,
          rotate = "varimax",
          scores = FALSE,
          missing = FALSE
        )
      )
    } else {
      fit_unrotated
    }

    # Keep the scree/eigenvalue information unrotated, matching the original
    # app, while using the selected (possibly rotated) loadings for scores and
    # interpretation.
    eigenvalues <- as.numeric(
      fit_unrotated$Vaccounted["SS loadings", seq_len(n_components)]
    )
    explained <- 100 * as.numeric(
      fit_unrotated$Vaccounted["Proportion Var", seq_len(n_components)]
    )

    loadings_unrotated <- as.matrix(
      fit_unrotated$loadings[, seq_len(n_components), drop = FALSE]
    )
    loadings <- as.matrix(
      fit_selected$loadings[, seq_len(n_components), drop = FALSE]
    )

    Z <- base::scale(W_used, center = TRUE, scale = TRUE)
    inv_R <- tryCatch(
      solve(R),
      error = function(e) .gqr_inverse_symmetric(R, tolerance = tolerance)
    )

    scores_unrotated <- Z %*% (inv_R %*% loadings_unrotated)
    scores <- Z %*% (inv_R %*% loadings)

    # psych::principal() may reorder and reorient components after Varimax.
    # The score columns therefore deliberately follow the returned loading
    # order instead of applying a second rotation matrix here.
    rotation_matrix <- if (rotation == "varimax" && n_components > 1L) {
      if (!is.null(fit_selected$rot.mat)) {
        as.matrix(fit_selected$rot.mat)
      } else {
        tryCatch(
          qr.solve(loadings_unrotated, loadings),
          error = function(e) diag(n_components)
        )
      }
    } else {
      diag(n_components)
    }
  }

  component_names <- paste0("PC", seq_len(n_components))
  colnames(loadings_unrotated) <- component_names
  colnames(loadings) <- component_names
  colnames(scores_unrotated) <- component_names
  colnames(scores) <- component_names

  rownames(loadings_unrotated) <- colnames(W_used)
  rownames(loadings) <- colnames(W_used)
  rownames(scores_unrotated) <- rownames(W_used)
  rownames(scores) <- rownames(W_used)

  structure(
    list(
      eigenvalues = stats::setNames(eigenvalues, component_names),
      variance_explained = stats::setNames(explained, component_names),
      cumulative_variance = stats::setNames(cumsum(explained), component_names),
      loadings = loadings,
      loadings_unrotated = loadings_unrotated,
      scores = scores,
      scores_unrotated = scores_unrotated,
      rotation_matrix = rotation_matrix,
      respondents_used = colnames(W_used),
      respondents_removed = removed,
      method = method,
      rotation = rotation,
      center = if (method == "correlation") TRUE else isTRUE(center),
      scale = if (method == "correlation") TRUE else isTRUE(scale)
    ),
    class = "gqr_pca_result"
  )
}

