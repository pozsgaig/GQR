# Compact PCA from the Generalised Q design -------------------------------

#' Fast exact PCA without materialising the complete W matrix
#'
#' @description
#' Computes the ordinary (`prcomp`-style) Generalised Q PCA directly from the
#' original respondent-by-statement data and the dummy design matrix `D`.
#' Because `W = D V^T` has rank no greater than the number of original
#' statements, the decomposition can be carried out in this much smaller
#' statement space. This can be substantially faster and use much less memory
#' when `W` has many rows or respondents.
#'
#' @param data A respondent-level data frame or a `gqr_prepared_data` object.
#' @param D A named binary design matrix produced by [gqr_generate_dummies()].
#' @param analysis_cols Analysis-column names when `data` is a plain data frame.
#' @param id_col Optional respondent identifier column when `data` is plain.
#' @param n_components Number of components to retain. If `NULL`, all positive
#'   components are retained.
#' @param rotation One of `"none"` or `"varimax"`.
#' @param center,scale Logical centring and scaling settings matching
#'   [stats::prcomp()].
#' @param na_action Handling of non-finite analysis values: `"error"`,
#'   `"mean"`, or `"zero"`.
#' @param remove_constant Whether respondent columns with zero variance are
#'   removed before PCA.
#' @param tolerance Numerical tolerance used for ranks and zero variance.
#' @param score_chunk_size Number of synthetic statements processed per block
#'   when component scores are generated.
#' @param progress Optional callback receiving `value` (0--1) and `message`.
#' @param cancel Optional zero-argument callback. Returning `TRUE` stops the
#'   calculation at the next safe checkpoint.
#'
#' @return An object of class `gqr_pca_result`, with the same principal fields
#'   as [gqr_pca()]. `method` is reported as `"design"`.
#'
#' @details
#' For ordinary PCA, materialising `W` is unnecessary. After the requested
#' centring and scaling, the respondent cross-product can be written using the
#' much smaller `m x m` cross-product of `D`, where `m` is the number of simple
#' statements. GQR factorises that matrix and performs an SVD in a space whose
#' rank is at most `m`. Synthetic-statement scores are then reconstructed in
#' blocks. The result is algebraically equivalent to ordinary PCA on the full W
#' matrix, apart from the arbitrary signs of PCA axes and floating-point error.
#'
#' This optimisation applies to ordinary PCA. The SPSS-style smoothed
#' correlation workflow implemented by [gqr_pca()] still requires the full W
#' matrix.
#'
#' @references
#' Dentinho, T. P., Kourtit, K., & Nijkamp, P. (2023). Generalized Q analysis
#' as a new tool in social science research: A pedagogical introduction.
#' *Eastern Journal of European Studies*, **14**(2), 5--21.
#' \doi{10.47743/ejes-2023-0201}
#'
#' @examples
#' dat <- gqr_example_data("dummy_data")
#' prepared <- gqr_prepare_data(dat, paste0("Q", 1:9), id_col = "Respondent")
#' D <- gqr_generate_dummies(prepared$analysis_cols)
#'
#' fast <- gqr_pca_design(
#'   prepared,
#'   D,
#'   n_components = 3,
#'   rotation = "varimax"
#' )
#' fast$variance_explained
#'
#' @export
gqr_pca_design <- function(
    data,
    D,
    analysis_cols = NULL,
    id_col = NULL,
    n_components = NULL,
    rotation = c("none", "varimax"),
    center = TRUE,
    scale = TRUE,
    na_action = c("error", "mean", "zero"),
    remove_constant = TRUE,
    tolerance = 1e-08,
    score_chunk_size = 10000L,
    progress = NULL,
    cancel = NULL) {

  rotation <- match.arg(rotation)
  na_action <- match.arg(na_action)
  score_chunk_size <- as.integer(score_chunk_size)

  if (length(score_chunk_size) != 1L ||
      is.na(score_chunk_size) ||
      score_chunk_size < 1L) {
    stop("`score_chunk_size` must be a positive integer.", call. = FALSE)
  }

  prepared <- .gqr_prepare_analysis_matrix(
    data = data,
    analysis_cols = analysis_cols,
    id_col = id_col,
    na_action = na_action
  )
  D <- .gqr_validate_design_matrix(D, prepared$analysis_cols)

  if (nrow(D) < 2L || nrow(prepared$V) < 2L) {
    stop("At least two combinations and two respondents are required.", call. = FALSE)
  }

  if (!is.null(n_components)) {
    n_components <- as.integer(n_components)
    if (length(n_components) != 1L ||
        is.na(n_components) ||
        n_components < 1L) {
      stop("`n_components` must be NULL or one positive integer.", call. = FALSE)
    }
  }

  .gqr_check_cancel(cancel)
  .gqr_report_progress(progress, 0.02, "Preparing compact PCA")

  k <- nrow(D)

  # Keep D in its integer representation. crossprod() and %*% promote values
  # as needed, avoiding a persistent double-precision copy of a potentially
  # large design matrix.

  # Cross-product for variation around the mean. This is also used to identify
  # respondent columns that have no variation, regardless of the centring
  # option subsequently passed to prcomp.
  d_mean <- colMeans(D)
  D_cross <- crossprod(D)
  D_centered_cross <- D_cross - k * tcrossprod(d_mean)
  D_centered_cross <- (D_centered_cross + t(D_centered_cross)) / 2

  V <- prepared$V
  centred_ss <- rowSums((V %*% D_centered_cross) * V)
  centred_ss[centred_ss < 0 & abs(centred_ss) <= tolerance] <- 0
  respondent_sd <- sqrt(pmax(centred_ss, 0) / (k - 1))
  constant <- !is.finite(respondent_sd) | respondent_sd <= tolerance

  if (any(constant) && !isTRUE(remove_constant)) {
    stop(
      "W contains zero-variance respondent columns: ",
      paste(prepared$ids[constant], collapse = ", "),
      call. = FALSE
    )
  }

  removed <- prepared$ids[constant]
  keep <- !constant
  V <- V[keep, , drop = FALSE]
  respondent_ids <- prepared$ids[keep]

  if (nrow(V) < 2L) {
    stop("Fewer than two non-constant respondent columns remain.", call. = FALSE)
  }

  .gqr_check_cancel(cancel)
  .gqr_report_progress(progress, 0.12, "Computing statement-space cross-products")

  # prcomp first centres W (if requested), then optionally divides each W
  # column by the scale returned by scale.default().
  if (isTRUE(center)) {
    G <- D_centered_cross
  } else {
    G <- D_cross
  }
  G <- (G + t(G)) / 2

  V_scaled <- V
  if (isTRUE(scale)) {
    scale_ss <- rowSums((V %*% G) * V)
    scale_ss[scale_ss < 0 & abs(scale_ss) <= tolerance] <- 0
    scale_values <- sqrt(pmax(scale_ss, 0) / (k - 1))

    invalid_scale <- !is.finite(scale_values) | scale_values <= tolerance
    if (any(invalid_scale)) {
      stop(
        "Cannot scale one or more retained respondent columns.",
        call. = FALSE
      )
    }
    V_scaled <- V / scale_values
  }

  .gqr_check_cancel(cancel)
  .gqr_report_progress(progress, 0.25, "Factorising the compact design")

  eig_G <- eigen(G, symmetric = TRUE)
  max_g <- max(abs(eig_G$values), 1)
  keep_g <- eig_G$values > tolerance * max_g

  if (!any(keep_g)) {
    stop("The design contains no positive-variance dimensions.", call. = FALSE)
  }

  Q <- eig_G$vectors[, keep_g, drop = FALSE]
  lambda <- eig_G$values[keep_g]

  # C'C equals the respondent cross-product of the centred/scaled W matrix,
  # but C has at most m rows rather than k rows.
  C <- (sqrt(lambda) * t(Q)) %*% t(V_scaled)

  .gqr_check_cancel(cancel)
  .gqr_report_progress(progress, 0.40, "Computing compact singular-value decomposition")

  sv <- base::svd(C, nu = 0L, nv = min(nrow(C), ncol(C)))
  singular_values <- sv$d
  eigenvalues_all <- singular_values^2 / (k - 1)
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
  loadings_unrotated <- sv$v[, seq_len(n_components), drop = FALSE]

  # Compute scores as X %*% loadings without creating X = W_scaled.
  score_coef <- t(V_scaled) %*% loadings_unrotated
  score_offset <- if (isTRUE(center)) {
    as.numeric(crossprod(d_mean, score_coef))
  } else {
    rep(0, n_components)
  }

  scores_unrotated <- matrix(
    NA_real_,
    nrow = k,
    ncol = n_components
  )
  starts <- seq.int(1L, k, by = score_chunk_size)

  for (i in seq_along(starts)) {
    .gqr_check_cancel(cancel)
    from <- starts[i]
    to <- min(k, from + score_chunk_size - 1L)
    block <- D[from:to, , drop = FALSE] %*% score_coef
    if (isTRUE(center)) {
      block <- sweep(block, 2L, score_offset, FUN = "-")
    }
    scores_unrotated[from:to, ] <- block
    .gqr_report_progress(
      progress,
      0.50 + 0.42 * i / length(starts),
      sprintf("Reconstructing PCA scores: block %d of %d", i, length(starts))
    )
  }

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

  component_names <- paste0("PC", seq_len(n_components))
  colnames(loadings_unrotated) <- component_names
  colnames(loadings) <- component_names
  colnames(scores_unrotated) <- component_names
  colnames(scores) <- component_names

  rownames(loadings_unrotated) <- respondent_ids
  rownames(loadings) <- respondent_ids
  rownames(scores_unrotated) <- if (is.null(rownames(D))) paste0("S", seq_len(k)) else rownames(D)
  rownames(scores) <- rownames(scores_unrotated)

  .gqr_check_cancel(cancel)
  .gqr_report_progress(progress, 1, "PCA ready")

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
      respondents_used = respondent_ids,
      respondents_removed = removed,
      method = "design",
      rotation = rotation,
      center = isTRUE(center),
      scale = isTRUE(scale)
    ),
    class = "gqr_pca_result"
  )
}
