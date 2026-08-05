# Internal utilities -------------------------------------------------------

.gqr_check_data <- function(data) {
  if (!is.data.frame(data)) {
    data <- as.data.frame(data, stringsAsFactors = FALSE)
  }
  if (nrow(data) < 1L || ncol(data) < 1L) {
    stop("`data` must contain at least one row and one column.", call. = FALSE)
  }
  data
}

.gqr_check_columns <- function(data, columns, argument) {
  if (is.null(columns)) {
    return(character())
  }
  columns <- as.character(columns)
  missing <- setdiff(columns, names(data))
  if (length(missing) > 0L) {
    stop(
      sprintf(
        "`%s` contains unknown columns: %s",
        argument,
        paste(missing, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  unique(columns)
}

.gqr_check_numeric <- function(data, columns) {
  bad <- columns[!vapply(data[columns], is.numeric, logical(1))]
  if (length(bad) > 0L) {
    stop(
      "Analysis columns must be numeric: ",
      paste(bad, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.gqr_scale_vector <- function(x) {
  out <- rep(NA_real_, length(x))
  ok <- is.finite(x)

  if (!any(ok)) {
    return(out)
  }

  value_sd <- stats::sd(x[ok])
  if (!is.finite(value_sd) || value_sd == 0) {
    out[ok] <- 0
  } else {
    out[ok] <- (x[ok] - mean(x[ok])) / value_sd
  }

  out
}

.gqr_normalise_vector <- function(x) {
  out <- rep(NA_real_, length(x))
  ok <- is.finite(x)

  if (!any(ok)) {
    return(out)
  }

  value_range <- range(x[ok])
  if (diff(value_range) == 0) {
    out[ok] <- 0
  } else {
    out[ok] <- (x[ok] - value_range[1L]) / diff(value_range)
  }

  out
}

.gqr_relative_vector <- function(x) {
  out <- rep(NA_real_, length(x))
  ok <- is.finite(x)

  if (!any(ok)) {
    return(out)
  }
  if (any(x[ok] < 0)) {
    stop("Relative-importance transformation requires non-negative values.", call. = FALSE)
  }

  total <- sum(x[ok])
  out[ok] <- if (total == 0) 0 else x[ok] / total
  out
}

.gqr_entropy_vector <- function(x) {
  p <- .gqr_relative_vector(x)
  out <- rep(NA_real_, length(x))
  ok <- is.finite(p)
  out[ok] <- ifelse(p[ok] > 0, -p[ok] * log(p[ok]), 0)
  out
}

.gqr_with_seed <- function(seed, code) {
  if (is.null(seed)) {
    return(force(code))
  }

  seed <- as.integer(seed)
  if (length(seed) != 1L || is.na(seed)) {
    stop("`seed` must be NULL or one integer.", call. = FALSE)
  }

  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) {
    old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  }

  on.exit({
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)

  set.seed(seed)
  force(code)
}

.gqr_make_ids <- function(data, id_col = NULL) {
  if (!is.null(id_col)) {
    id_col <- .gqr_check_columns(data, id_col, "id_col")
    if (length(id_col) != 1L) {
      stop("`id_col` must name exactly one column.", call. = FALSE)
    }

    ids <- as.character(data[[id_col]])
    if (anyNA(ids) || any(ids == "")) {
      stop("The ID column contains missing or empty values.", call. = FALSE)
    }
    if (anyDuplicated(ids)) {
      stop("The ID column must contain unique values.", call. = FALSE)
    }
    return(ids)
  }

  row_ids <- rownames(data)
  if (!is.null(row_ids) &&
      length(row_ids) == nrow(data) &&
      !identical(row_ids, as.character(seq_len(nrow(data))))) {
    if (!anyDuplicated(row_ids)) {
      return(row_ids)
    }
  }

  width <- nchar(as.character(nrow(data)))
  sprintf(paste0("R%0", width, "d"), seq_len(nrow(data)))
}

.gqr_prepare_groups <- function(groups, variables, allow_ungrouped = FALSE) {
  if (is.null(groups)) {
    stop("`groups` is required for `mode = \"group_one_per\"`.", call. = FALSE)
  }

  if (is.list(groups) && !is.data.frame(groups)) {
    if (is.null(names(groups)) || any(names(groups) == "")) {
      stop("A grouping list must be named.", call. = FALSE)
    }
    groups <- data.frame(
      group = rep(names(groups), lengths(groups)),
      variable = unlist(groups, use.names = FALSE),
      stringsAsFactors = FALSE
    )
  }

  if (!is.data.frame(groups) ||
      !all(c("group", "variable") %in% names(groups))) {
    stop(
      "`groups` must be a named list or a data frame with `group` and `variable` columns.",
      call. = FALSE
    )
  }

  groups <- groups[c("group", "variable")]
  groups$group <- as.character(groups$group)
  groups$variable <- as.character(groups$variable)
  groups <- groups[!is.na(groups$group) & !is.na(groups$variable), , drop = FALSE]
  groups <- unique(groups)

  if (nrow(groups) == 0L) {
    stop("No variables are assigned to groups.", call. = FALSE)
  }

  unknown <- setdiff(groups$variable, variables)
  if (length(unknown) > 0L) {
    stop(
      "Grouped variables not found in `variables`: ",
      paste(unknown, collapse = ", "),
      call. = FALSE
    )
  }

  duplicated_variables <- unique(groups$variable[duplicated(groups$variable)])
  if (length(duplicated_variables) > 0L) {
    stop(
      "Each variable can belong to only one group: ",
      paste(duplicated_variables, collapse = ", "),
      call. = FALSE
    )
  }

  ungrouped <- setdiff(variables, groups$variable)
  if (!allow_ungrouped && length(ungrouped) > 0L) {
    stop(
      "Every analysis variable must be assigned to a group. Ungrouped: ",
      paste(ungrouped, collapse = ", "),
      call. = FALSE
    )
  }

  group_order <- unique(groups$group)
  split_vars <- lapply(
    group_order,
    function(group_name) groups$variable[groups$group == group_name]
  )
  names(split_vars) <- group_order

  list(
    table = groups,
    split = split_vars,
    ungrouped = ungrouped
  )
}

.gqr_impute_columns <- function(x) {
  for (j in seq_len(ncol(x))) {
    missing <- !is.finite(x[, j])
    if (any(missing)) {
      observed <- x[!missing, j]
      if (length(observed) == 0L) {
        stop(
          "Cannot mean-impute a column containing no finite values: ",
          colnames(x)[j],
          call. = FALSE
        )
      }
      x[missing, j] <- mean(observed)
    }
  }
  x
}

.gqr_smooth_correlation <- function(x, tolerance = 1e-08) {
  x <- (x + t(x)) / 2
  eig <- eigen(x, symmetric = TRUE)
  eig$values[eig$values < tolerance] <- tolerance

  smoothed <- eig$vectors %*%
    (eig$values * t(eig$vectors))

  scale_values <- sqrt(diag(smoothed))
  smoothed <- smoothed / outer(scale_values, scale_values)
  diag(smoothed) <- 1
  dimnames(smoothed) <- dimnames(x)
  smoothed
}

.gqr_inverse_symmetric <- function(x, tolerance = 1e-08) {
  eig <- eigen(x, symmetric = TRUE)
  keep <- eig$values > tolerance

  if (!any(keep)) {
    stop("The matrix has no invertible dimensions.", call. = FALSE)
  }

  eig$vectors[, keep, drop = FALSE] %*%
    ((1 / eig$values[keep]) * t(eig$vectors[, keep, drop = FALSE]))
}

.gqr_tidy_lm <- function(model, component) {
  coefficient_matrix <- suppressWarnings(summary(model))$coefficients
  if (is.null(coefficient_matrix)) {
    return(data.frame())
  }

  data.frame(
    component = component,
    term = rownames(coefficient_matrix),
    estimate = coefficient_matrix[, "Estimate"],
    std.error = coefficient_matrix[, "Std. Error"],
    statistic = coefficient_matrix[, "t value"],
    p.value = coefficient_matrix[, "Pr(>|t|)"],
    row.names = NULL,
    check.names = FALSE
  )
}
