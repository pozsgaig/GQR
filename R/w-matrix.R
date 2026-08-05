# W matrix ----------------------------------------------------------------

#' Construct the synthetic evaluation matrix
#'
#' @description
#' Combines the binary synthetic-statement design with respondent evaluations to
#' calculate the Generalised Q matrix `W`.
#'
#' @param data A respondent-level data frame or a `gqr_prepared_data` object.
#' @param analysis_cols Analysis-column names when `data` is a plain data frame.
#' @param D A named binary design matrix produced by
#'   [gqr_generate_dummies()].
#' @param id_col Optional respondent identifier column when `data` is plain.
#' @param na_action Handling of non-finite analysis values: `"error"`,
#'   `"mean"`, or `"zero"`.
#'
#' @return A numeric matrix with synthetic combined statements in rows and
#'   respondents in columns.
#'
#' @details
#' Let `V` be the respondent-by-statement matrix and `D` the
#' combination-by-statement design. The function calculates
#'
#' \deqn{W = D V^\mathsf{T}.}
#'
#' Therefore, `W[i, j]` is the sum of respondent `j`'s values for the simple
#' statements active in combination `i`. This encodes the additive-separability
#' assumption central to Generalised Q analysis.
#'
#' @references
#' Dentinho, T. P., Kourtit, K., & Nijkamp, P. (2023). Generalized Q analysis
#' as a new tool in social science research: A pedagogical introduction.
#' *Eastern Journal of European Studies*, **14**(2), 5--21.
#' \doi{10.47743/ejes-2023-0201}
#' @examples
#' dat <- gqr_example_data("dummy_data")
#' prepared <- gqr_prepare_data(
#'   dat,
#'   analysis_cols = paste0("Q", 1:9),
#'   id_col = "Respondent"
#' )
#' D <- gqr_generate_dummies(prepared$analysis_cols, mode = "all")
#' W <- gqr_make_w(prepared, D = D)
#' dim(W)
#'
#' @export
gqr_make_w <- function(
    data,
    analysis_cols = NULL,
    D,
    id_col = NULL,
    na_action = c("error", "mean", "zero")) {

  na_action <- match.arg(na_action)

  if (inherits(data, "gqr_prepared_data")) {
    analysis_cols <- data$analysis_cols
    ids <- data$ids
    data <- data$data
  } else {
    data <- .gqr_check_data(data)
    analysis_cols <- .gqr_check_columns(data, analysis_cols, "analysis_cols")
    ids <- .gqr_make_ids(data, id_col)
  }

  .gqr_check_numeric(data, analysis_cols)

  if (!is.matrix(D) || !is.numeric(D)) {
    stop("`D` must be a numeric matrix.", call. = FALSE)
  }
  if (is.null(colnames(D))) {
    stop("`D` must have variable names as column names.", call. = FALSE)
  }
  if (anyDuplicated(colnames(D))) {
    stop("`D` column names must be unique.", call. = FALSE)
  }
  if (any(!is.finite(D)) || any(!D %in% c(0, 1))) {
    stop("`D` must contain only finite zero/one values.", call. = FALSE)
  }

  missing_in_d <- setdiff(analysis_cols, colnames(D))
  extra_in_d <- setdiff(colnames(D), analysis_cols)
  if (length(missing_in_d) > 0L || length(extra_in_d) > 0L) {
    stop(
      "`D` columns must match `analysis_cols` exactly.",
      call. = FALSE
    )
  }

  D <- D[, analysis_cols, drop = FALSE]
  V <- as.matrix(data[analysis_cols])
  storage.mode(V) <- "double"

  if (any(!is.finite(V))) {
    if (na_action == "error") {
      stop(
        "Analysis data contain missing or non-finite values. Transform or impute them first, or change `na_action`.",
        call. = FALSE
      )
    }
    if (na_action == "mean") {
      V <- .gqr_impute_columns(V)
    } else {
      V[!is.finite(V)] <- 0
    }
  }

  W <- D %*% t(V)
  if (is.null(rownames(D))) {
    rownames(W) <- paste0("S", seq_len(nrow(D)))
  } else {
    rownames(W) <- rownames(D)
  }
  colnames(W) <- ids
  W
}
