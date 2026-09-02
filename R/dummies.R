# Dummy designs -----------------------------------------------------------

#' Estimate the size of a Generalised Q design
#'
#' @description
#' Calculates the number of synthetic statement patterns and approximate
#' memory requirements before allocating the design matrix `D` or evaluation
#' matrix `W`.
#'
#' @param variables Character vector of simple-statement names, or a single
#'   positive number giving the statement count.
#' @param mode One of `"all"`, `"group_one_per"`, or `"random"`.
#' @param groups Group specification required for grouped mode. Supply either a
#'   named list of variables or a data frame with columns `group` and `variable`.
#' @param n_patterns Number of patterns requested in random mode.
#' @param n_respondents Optional respondent count used to estimate W memory.
#' @param allow_ungrouped Whether grouped mode may leave variables permanently
#'   inactive.
#'
#' @return A one-row data frame containing the mode, variable count, pattern
#'   count, estimated dummy-matrix memory in MiB, and estimated W-matrix memory
#'   in MiB.
#'
#' @details
#' Full binary designs contain `2^m` patterns for `m` statements. Grouped
#' designs contain the product of the group sizes. Random designs contain the
#' requested fixed number of rows. Estimates assume four bytes per integer
#' dummy value and eight bytes per numeric W value; temporary copies and PCA
#' objects require additional memory.
#'
#' @references
#' Dentinho, T. P., Kourtit, K., & Nijkamp, P. (2023). Generalized Q analysis
#' as a new tool in social science research: A pedagogical introduction.
#' *Eastern Journal of European Studies*, **14**(2), 5--21.
#' \doi{10.47743/ejes-2023-0201}
#' @examples
#' gqr_estimate_design(
#'   variables = paste0("Q", 1:9),
#'   mode = "all",
#'   n_respondents = 10
#' )
#'
#' groups <- list(
#'   Question_1 = c("Q1", "Q2", "Q3"),
#'   Question_2 = c("Q4", "Q5", "Q6"),
#'   Question_3 = c("Q7", "Q8", "Q9")
#' )
#' gqr_estimate_design(paste0("Q", 1:9), "group_one_per", groups)
#'
#' @export
gqr_estimate_design <- function(
    variables,
    mode = c("all", "group_one_per", "random"),
    groups = NULL,
    n_patterns = 1000L,
    n_respondents = NULL,
    allow_ungrouped = FALSE) {

  mode <- match.arg(mode)

  if (length(variables) == 1L && is.numeric(variables)) {
    n_variables <- as.integer(variables)
    variable_names <- paste0("V", seq_len(n_variables))
  } else {
    variable_names <- as.character(variables)
    n_variables <- length(variable_names)
  }

  if (n_variables < 1L) {
    stop("At least one variable is required.", call. = FALSE)
  }

  patterns <- switch(
    mode,
    all = 2^n_variables,
    random = as.double(n_patterns),
    group_one_per = {
      grouping <- .gqr_prepare_groups(
        groups,
        variable_names,
        allow_ungrouped = allow_ungrouped
      )
      prod(vapply(grouping$split, length, integer(1)))
    }
  )

  d_mb <- patterns * n_variables * 4 / 1024^2
  w_mb <- if (is.null(n_respondents)) {
    NA_real_
  } else {
    patterns * as.double(n_respondents) * 8 / 1024^2
  }

  data.frame(
    mode = mode,
    variables = n_variables,
    patterns = patterns,
    dummy_memory_mb = d_mb,
    w_memory_mb = w_mb
  )
}

#' Generate the synthetic-statement design matrix
#'
#' @description
#' Creates the binary dummy matrix `D` used by Generalised Q analysis. Rows
#' represent synthetic combined statements and columns represent the original
#' simple statements. Each entry is an indicator: `1` means that a statement is
#' included in a combination and `0` means that it is excluded.
#'
#' @param variables Character vector of unique simple-statement names.
#' @param mode One of `"all"`, `"group_one_per"`, or `"random"`.
#' @param groups Named list or data frame with columns `group` and `variable`
#'   for grouped mode.
#' @param n_patterns Number of rows sampled in random mode.
#' @param prob Probability that each statement is active in random mode.
#' @param seed Optional seed used only for random mode.
#' @param include_empty Whether the all-zero combination is retained.
#' @param allow_ungrouped Whether grouped mode may leave variables permanently
#'   inactive.
#' @param max_patterns Maximum permitted number of rows, checked before
#'   allocation.
#' @param progress Optional callback function receiving two arguments,
#'   `value` (from 0 to 1) and `message`. It can be used by scripts or
#'   interfaces to report progress.
#' @param cancel Optional zero-argument callback. If it returns `TRUE`, the
#'   computation stops with a `gqr_cancelled` error at the next safe checkpoint.
#'
#' @return An integer zero/one matrix with named columns and synthetic statement
#'   row names `S1`, `S2`, and so forth.
#'
#' @details
#' The word *dummy* is used in its standard statistical meaning: a zero/one
#' variable indicating membership or inclusion. It does not refer to an
#' artificial respondent or to a missing value. For example, with columns `A`,
#' `B`, and `C`, the row `(1, 0, 1)` denotes a synthetic statement that includes
#' `A` and `C` but excludes `B`.
#'
#' In full mode, the rows follow binary counting. In grouped mode, exactly one
#' variable is active in every group, implementing the structured recombination
#' described by Dentinho et al. (2023). In random mode, entries are independent
#' Bernoulli draws; results should be checked across seeds or larger samples.
#'
#' Use [gqr_estimate_design()] before full or large grouped designs.
#'
#' @references
#' Dentinho, T. P., Kourtit, K., & Nijkamp, P. (2023). Generalized Q analysis
#' as a new tool in social science research: A pedagogical introduction.
#' *Eastern Journal of European Studies*, **14**(2), 5--21.
#' \doi{10.47743/ejes-2023-0201}
#' @examples
#' gqr_generate_dummies(c("A", "B", "C"), mode = "all")
#'
#' groups <- data.frame(
#'   group = c("Question 1", "Question 1", "Question 2", "Question 2"),
#'   variable = c("A", "B", "C", "D")
#' )
#' gqr_generate_dummies(c("A", "B", "C", "D"), "group_one_per", groups)
#'
#' gqr_generate_dummies(
#'   paste0("Q", 1:20),
#'   mode = "random",
#'   n_patterns = 1000,
#'   seed = 42
#' )
#'
#' @export
gqr_generate_dummies <- function(
    variables,
    mode = c("all", "group_one_per", "random"),
    groups = NULL,
    n_patterns = 1000L,
    prob = 0.5,
    seed = NULL,
    include_empty = TRUE,
    allow_ungrouped = FALSE,
    max_patterns = 1000000L,
    progress = NULL,
    cancel = NULL) {

  variables <- as.character(variables)
  if (length(variables) < 1L || anyNA(variables) || any(variables == "")) {
    stop("`variables` must contain at least one non-missing name.", call. = FALSE)
  }
  if (anyDuplicated(variables)) {
    stop("`variables` must be unique.", call. = FALSE)
  }

  mode <- match.arg(mode)
  max_patterns <- as.double(max_patterns)

  .gqr_check_cancel(cancel)
  .gqr_report_progress(progress, 0, "Checking dummy design")

  if (!is.finite(max_patterns) || max_patterns < 1) {
    stop("`max_patterns` must be a positive finite number.", call. = FALSE)
  }

  if (mode == "all") {
    pattern_count <- 2^length(variables)

    if (!is.finite(pattern_count) || pattern_count > max_patterns) {
      stop(
        sprintf(
          "The full design requires %.0f patterns, above `max_patterns = %.0f`. Use grouped or random mode.",
          pattern_count,
          max_patterns
        ),
        call. = FALSE
      )
    }

    row_index <- seq.int(0, pattern_count - 1)
    D <- matrix(
      0L,
      nrow = pattern_count,
      ncol = length(variables)
    )

    for (j in seq_along(variables)) {
      .gqr_check_cancel(cancel)
      D[, j] <- as.integer((row_index %/% 2^(j - 1L)) %% 2)
      .gqr_report_progress(
        progress,
        0.05 + 0.85 * j / length(variables),
        sprintf("Generating dummy variable %d of %d", j, length(variables))
      )
    }
  } else if (mode == "group_one_per") {
    grouping <- .gqr_prepare_groups(
      groups,
      variables,
      allow_ungrouped = allow_ungrouped
    )

    pattern_count <- prod(vapply(grouping$split, length, integer(1)))
    if (!is.finite(pattern_count) || pattern_count > max_patterns) {
      stop(
        sprintf(
          "The grouped design requires %.0f patterns, above `max_patterns = %.0f`.",
          pattern_count,
          max_patterns
        ),
        call. = FALSE
      )
    }

    index_grid <- expand.grid(
      lapply(grouping$split, seq_along),
      KEEP.OUT.ATTRS = FALSE,
      stringsAsFactors = FALSE
    )

    D <- matrix(
      0L,
      nrow = nrow(index_grid),
      ncol = length(variables),
      dimnames = list(NULL, variables)
    )

    for (g in seq_along(grouping$split)) {
      .gqr_check_cancel(cancel)
      selected_variables <- grouping$split[[g]][index_grid[[g]]]
      D[cbind(seq_len(nrow(D)), match(selected_variables, variables))] <- 1L
      .gqr_report_progress(
        progress,
        0.10 + 0.80 * g / length(grouping$split),
        sprintf("Expanding group %d of %d", g, length(grouping$split))
      )
    }
  } else {
    n_patterns <- as.integer(n_patterns)
    if (length(n_patterns) != 1L || is.na(n_patterns) || n_patterns < 1L) {
      stop("`n_patterns` must be a positive integer.", call. = FALSE)
    }
    if (n_patterns > max_patterns) {
      stop("`n_patterns` exceeds `max_patterns`.", call. = FALSE)
    }
    if (length(prob) != 1L || !is.finite(prob) || prob < 0 || prob > 1) {
      stop("`prob` must lie between zero and one.", call. = FALSE)
    }

    .gqr_check_cancel(cancel)
    .gqr_report_progress(progress, 0.25, "Sampling random dummy patterns")
    D <- .gqr_with_seed(
      seed,
      matrix(
        stats::rbinom(
          n_patterns * length(variables),
          size = 1L,
          prob = prob
        ),
        nrow = n_patterns,
        ncol = length(variables)
      )
    )
  }

  storage.mode(D) <- "integer"
  colnames(D) <- variables

  if (!include_empty) {
    D <- D[rowSums(D) > 0L, , drop = FALSE]
  }

  rownames(D) <- paste0("S", seq_len(nrow(D)))
  attr(D, "mode") <- mode
  attr(D, "groups") <- if (mode == "group_one_per") groups else NULL
  .gqr_check_cancel(cancel)
  .gqr_report_progress(progress, 1, "Dummy design ready")
  D
}

