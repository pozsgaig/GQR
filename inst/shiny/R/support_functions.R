# src/support_functions.R

#' Generate dummy combinations (full 2^n, grouped one-per-group, or random)
#'
#' Generalised Q analysis uses binary dummy patterns to encode which basic
#' statements (variables) are included in each synthetic statement S_i.
#'
#' @param nvars Integer scalar. Number of variables.
#' @param varnames Optional character vector of variable names (length nvars).
#' @param groups Optional data frame with columns group and variable (for grouped mode).
#' @param mode Character. One of "all", "group_one_per", "random".
#' @param n_patterns Integer. Number of random patterns when mode = "random".
#' @param prob Numeric in [0,1]. Probability of 1 in random patterns when mode = "random".
#'
#' @return Integer matrix D with 0/1 entries and column names varnames.
gqr_generate_all_dummies <- function(nvars,
                                   varnames = NULL,
                                   groups = NULL,
                                   mode = NULL,
                                   n_patterns = 100L,
                                   prob = 0.5) {

  stopifnot(nvars >= 1L)

  if (is.null(varnames)) {
    varnames <- paste0("V", seq_len(nvars))
  } else {
    stopifnot(length(varnames) == nvars)
  }

  if (is.null(mode)) {
    mode <- if (is.null(groups)) "all" else "group_one_per"
  }
  mode <- match.arg(mode, c("all", "group_one_per", "random"))

  if (mode == "random") {
    stopifnot(n_patterns >= 1L, prob >= 0, prob <= 1)

    n_vars <- length(varnames)
    D <- matrix(
      stats::rbinom(n_patterns * n_vars, size = 1L, prob = prob),
      nrow = n_patterns,
      ncol = n_vars
    )
    colnames(D) <- varnames
    return(D)
  }

  if (mode == "all") {
    ncomb <- 2^nvars
    D <- matrix(0L, nrow = ncomb, ncol = nvars)
    for (i in seq_len(nvars)) {
      block_len <- 2^(i - 1L)
      pattern <- rep(
        c(rep(0L, block_len), rep(1L, block_len)),
        length.out = ncomb
      )
      D[, i] <- pattern
    }
    colnames(D) <- varnames
    return(D)
  }

  # mode == "group_one_per"
  stopifnot(!is.null(groups))
  stopifnot(all(c("group", "variable") %in% names(groups)))

  gdf <- groups |>
    dplyr::filter(!is.na(.data$variable)) |>
    dplyr::mutate(
      group    = as.character(.data$group),
      variable = as.character(.data$variable)
    )

  gdf <- gdf |>
    dplyr::arrange(.data$group, match(.data$variable, varnames))

  if (nrow(gdf) == 0L) {
    stop("gqr_generate_all_dummies: groups has no non-NA variables.")
  }

  gsplit <- gdf |>
    dplyr::group_by(.data$group) |>
    dplyr::summarise(
      vars = list(unique(.data$variable)),
      .groups = "drop"
    )

  groupvars <- gsplit$vars
  allvars   <- varnames

  if (!all(unlist(groupvars) %in% allvars)) {
    stop("Some variables in 'groups' are not in 'varnames'.")
  }

  indexlists <- lapply(groupvars, function(vs) seq_along(vs))

  indexgrid_rev <- expand.grid(
    rev(indexlists),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  indexgrid <- indexgrid_rev[, ncol(indexgrid_rev):1, drop = FALSE]

  npatterns <- nrow(indexgrid)
  D <- matrix(0L, nrow = npatterns, ncol = length(allvars))
  colnames(D) <- allvars

  for (r in seq_len(npatterns)) {
    chosenvars <- character(length(groupvars))
    for (g in seq_along(groupvars)) {
      idx_in_group  <- indexgrid[r, g]
      chosenvars[g] <- groupvars[[g]][idx_in_group]
    }
    D[r, match(chosenvars, allvars)] <- 1L
  }

  D
}

#' Construct the Generalised Q W matrix with flexible dummies
#'
#' @param data_state List returned by dataTabServer()
#' @param dummy_mode "all", "group_one_per", or "random"
#' @param n_random Integer, number of random patterns when dummy_mode = "random"
#' @param prob_random Numeric in [0,1], probability of 1 when dummy_mode = "random"
#'
#' @return Numeric matrix W (combinations × respondents)
make_W <- function(data_state,
                   dummy_mode  = c("all", "group_one_per", "random"),
                   n_random    = 100L,
                   prob_random = 0.5) {

  dummy_mode <- match.arg(dummy_mode)

  df   <- data_state$data_trans()
  vars <- data_state$analysis_cols()
  shiny::req(df, vars)

  V <- as.matrix(df[, vars, drop = FALSE])  # respondents × items

  if (dummy_mode == "all") {

    D <- gqr_generate_all_dummies(
      nvars   = length(vars),
      varnames = vars,
      mode     = "all"
    )

  } else if (dummy_mode == "group_one_per") {

    groups <- data_state$groups()
    shiny::req(groups, nrow(groups) > 0)

    D <- gqr_generate_all_dummies(
      nvars   = length(vars),
      varnames = vars,
      groups   = groups,
      mode     = "group_one_per"
    )

  } else {

    D <- gqr_generate_all_dummies(
      nvars      = length(vars),
      varnames   = vars,
      mode       = "random",
      n_patterns = n_random,
      prob       = prob_random
    )
  }

  W <- D %*% t(V)  # combinations × respondents

  rownames(W) <- paste0("S", seq_len(nrow(W)))
  if ("ID" %in% colnames(df)) {
    # Ensure they are strings, but keep their original order
    colnames(W) <- as.character(df$ID)
  } else {
    # Use zero-padded numbers so alphabetical sorting preserves numeric order
    # (e.g., R01, R02 ... R10)
    num_digits <- nchar(as.character(nrow(df)))
    colnames(W) <- sprintf(paste0("R%0", num_digits, "d"), seq_len(nrow(df)))
  }

  W
}



# Adapt the package PCA result to the names used by the graphical modules.
gqr_app_pca <- function(
    W,
    scale. = TRUE,
    center = TRUE,
    rotate = c("none", "varimax"),
    naxis = NULL,
    SPSS = FALSE,
    add_scores = FALSE,
    impute_mean = TRUE) {

  rotate <- match.arg(rotate)

  result <- GQR::gqr_pca(
    W = W,
    n_components = naxis,
    rotation = rotate,
    center = center,
    scale = scale.,
    method = if (isTRUE(SPSS)) "correlation" else "prcomp",
    impute = if (isTRUE(SPSS) && isTRUE(impute_mean)) "mean" else "none"
  )

  output <- list(
    eigenvalues = unname(result$eigenvalues),
    var_expl = unname(result$variance_explained),
    loadings_rot = result$loadings,
    scores = result$scores,
    sdev = sqrt(unname(result$eigenvalues))
  )

  if (isTRUE(add_scores)) {
    output$data_with_scores <- cbind(
      as.data.frame(W, check.names = FALSE),
      as.data.frame(result$scores, check.names = FALSE)
    )
  }

  output
}

#' Standardise variables and fit linear model for PCA component regressions
#'
#' This mirrors the `standardiseandfit` from proba2.R: scales both the PCA component
#' (dependent variable) and dummy variable predictors (independent variables) to
#' z-scores (mean=0, sd=1). This produces *standardised beta coefficients* (β),
#' which represent effect sizes in standard deviation units - ideal for comparing
#' predictor importance across models/components.
#'
#' Used in Generalised Q analysis to interpret which dummy combinations most
#' strongly predict rotated PCA component scores on the W matrix.
#'
#' @param pc_name Character: name of PCA component column (e.g. "PC1")
#' @param data Data frame: augmented_W with Combination, dummies, PC scores
#' @return Fitted lm object with standardised coefficients
#' @export
standardiseandfit <- function(pc_name, data, dummy_vars) {
  df <- as.data.frame(data, check.names = FALSE)
  df[[pc_name]] <- as.numeric(
    scale(df[[pc_name]], center = TRUE, scale = TRUE)
  )
  df[dummy_vars] <- lapply(
    df[dummy_vars],
    function(x) as.numeric(scale(as.numeric(x), center = TRUE, scale = TRUE))
  )

  form <- stats::reformulate(dummy_vars, response = pc_name)
  stats::lm(form, data = df)
}


# Choose regression predictors from the dummy variables while keeping
# the first variable in each group as the omitted baseline.
#
# Why this is needed:
# In the grouped version of the Generalised Q regressions, one variable per
# group should be left out of the model so that coefficients are interpreted
# relative to that baseline. In proba2.R this was done manually by omitting
# Q1, Q5, and Q8 from the regression formula.
#
# What this function does:
# - takes the dummy-variable names currently used in the app;
# - looks up the grouping structure from datastate$groups();
# - for each group with at least two variables, omits the first variable
#   in the order of dummy_vars;
# - returns the remaining variables to be used as predictors.
#
# Output:
# A character vector of predictor names to include in lm(). The omitted
# variables are the automatic group baselines.
get_regression_predictors <- function(dummy_vars, groups = NULL) {
  if (length(dummy_vars) == 0L || is.null(groups)) {
    return(dummy_vars)
  }

  gdf <- groups |>
    dplyr::filter(!is.na(.data$variable), .data$variable %in% dummy_vars) |>
    dplyr::mutate(
      group = as.character(.data$group),
      variable = as.character(.data$variable),
      order_in_dummy_vars = match(.data$variable, dummy_vars)
    ) |>
    dplyr::arrange(.data$order_in_dummy_vars)

  if (nrow(gdf) == 0L) {
    return(dummy_vars)
  }

  baselines <- gdf |>
    dplyr::group_by(.data$group) |>
    dplyr::filter(dplyr::n() > 1L) |>
    dplyr::slice(1L) |>
    dplyr::ungroup() |>
    dplyr::pull(.data$variable) |>
    unique()

  dummy_vars[!(dummy_vars %in% baselines)]
}

#  full name if the string is 10 characters or fewer, abbreviated otherwise
abbreviate_dummy_names <- function(x, max_n = 10) {
  stopifnot(is.character(x), length(max_n) == 1L, max_n >= 4)

  vapply(
    x,
    function(s) {
      if (is.na(s) || nchar(s) <= max_n) {
        s
      } else {
        paste0(substr(s, 1, max_n - 1), "…")
      }
    },
    character(1)
  )
}

# Return a qualitative palette of visually distinct colours, expanding or
# re-sampling from Polychrome-style many-group palettes as needed for the
# requested number of groups.
qual_pal <- function(n, seed = NULL, dark = FALSE) {
  if (!is.null(seed)) set.seed(seed)

  base_pal <- if (n <= 24) {
    if (dark) Polychrome::dark.colors(24) else Polychrome::light.colors(24)
  } else if (n <= 26) {
    Polychrome::alphabet.colors(26)
  } else if (n <= 32) {
    Polychrome::glasbey.colors(32)
  } else if (n <= 36) {
    Polychrome::palette36.colors(36)
  } else {
    Polychrome::createPalette(
      N = n,
      seedcolors = c("#5A5156", "#E4E1E3", "#F6222E"),
      range = c(30, 90)
    )
  }

  pal <- sample(base_pal, size = n, replace = FALSE)
  unname(pal)
}



`%||%` <- function(x, y) if (is.null(x)) y else x
