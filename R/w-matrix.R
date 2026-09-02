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
#' @param rows Optional row indices or synthetic-statement names identifying a
#'   subset of `D` to evaluate. This is useful for previews without materialising
#'   the complete W matrix.
#' @param algorithm Matrix construction strategy. `"matmul"` uses one BLAS
#'   matrix multiplication; `"chunked"` processes blocks of rows so progress
#'   and cancellation can be checked; `"auto"` chooses chunking when a progress
#'   or cancellation callback is supplied.
#' @param chunk_size Number of D rows processed per block in chunked mode.
#' @param progress Optional callback receiving `value` (0--1) and `message`.
#' @param cancel Optional zero-argument callback. Returning `TRUE` cancels at
#'   the next block boundary.
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
    na_action = c("error", "mean", "zero"),
    rows = NULL,
    algorithm = c("auto", "matmul", "chunked"),
    chunk_size = 5000L,
    progress = NULL,
    cancel = NULL) {

  na_action <- match.arg(na_action)
  algorithm <- match.arg(algorithm)
  chunk_size <- as.integer(chunk_size)

  if (length(chunk_size) != 1L || is.na(chunk_size) || chunk_size < 1L) {
    stop("`chunk_size` must be a positive integer.", call. = FALSE)
  }

  prepared <- .gqr_prepare_analysis_matrix(
    data = data,
    analysis_cols = analysis_cols,
    id_col = id_col,
    na_action = na_action
  )

  D <- .gqr_validate_design_matrix(D, prepared$analysis_cols)

  if (!is.null(rows)) {
    if (is.character(rows)) {
      if (is.null(rownames(D))) {
        stop("Character `rows` require row names on `D`.", call. = FALSE)
      }
      row_index <- match(rows, rownames(D))
      if (anyNA(row_index)) {
        stop("Some requested `rows` are not present in `D`.", call. = FALSE)
      }
    } else {
      row_index <- as.integer(rows)
      if (anyNA(row_index) || any(row_index < 1L | row_index > nrow(D))) {
        stop("Numeric `rows` are outside the valid D row range.", call. = FALSE)
      }
    }
    D <- D[row_index, , drop = FALSE]
  }

  .gqr_check_cancel(cancel)
  .gqr_report_progress(progress, 0, "Constructing W")

  if (algorithm == "auto") {
    algorithm <- if (!is.null(progress) || !is.null(cancel)) "chunked" else "matmul"
  }

  Vt <- t(prepared$V)

  if (algorithm == "matmul" || nrow(D) <= chunk_size) {
    W <- D %*% Vt
    .gqr_report_progress(progress, 0.95, "Matrix multiplication complete")
  } else {
    W <- matrix(
      NA_real_,
      nrow = nrow(D),
      ncol = nrow(prepared$V)
    )

    starts <- seq.int(1L, nrow(D), by = chunk_size)
    for (i in seq_along(starts)) {
      .gqr_check_cancel(cancel)
      from <- starts[i]
      to <- min(nrow(D), from + chunk_size - 1L)
      W[from:to, ] <- D[from:to, , drop = FALSE] %*% Vt
      .gqr_report_progress(
        progress,
        i / length(starts),
        sprintf("Constructing W block %d of %d", i, length(starts))
      )
    }
  }

  if (is.null(rownames(D))) {
    rownames(W) <- paste0("S", seq_len(nrow(D)))
  } else {
    rownames(W) <- rownames(D)
  }
  colnames(W) <- prepared$ids

  .gqr_check_cancel(cancel)
  .gqr_report_progress(progress, 1, "W matrix ready")
  W
}
