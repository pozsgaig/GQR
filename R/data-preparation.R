# Data preparation --------------------------------------------------------

#' Read data for Generalised Q analysis
#'
#' @description
#' Reads a rectangular respondent-level dataset while preserving the original
#' column names. CSV, RDS, RDA, and RData files are supported. This is the file
#' input counterpart to [gqr_prepare_data()] and the Shiny Data tab.
#'
#' @param path A length-one character path to an existing CSV, RDS, RDA, or
#'   RData file.
#'
#' @return A data frame with at least one row and one column.
#'
#' @details
#' CSV delimiters are detected from comma, semicolon, and tab candidates. RDS
#' files must contain one object coercible to a data frame. RDA/RData files must
#' contain exactly one data frame, or a data frame whose object name matches the
#' file name. Column names are not syntactically altered because they may carry
#' meaningful statement labels.
#'
#' Reading data does not assign analytical roles or transform values. Use
#' [gqr_prepare_data()] after import.
#'
#' @references
#' Dentinho, T. P., Kourtit, K., & Nijkamp, P. (2023). Generalized Q analysis
#' as a new tool in social science research: A pedagogical introduction.
#' *Eastern Journal of European Studies*, **14**(2), 5--21.
#' \doi{10.47743/ejes-2023-0201}
#' @examples
#' path <- system.file("extdata", "dummy_data.RDA", package = "GQR")
#' if (nzchar(path)) {
#'   dat <- gqr_read(path)
#'   names(dat)
#' }
#'
#' @export
gqr_read <- function(path) {
  if (length(path) != 1L || !file.exists(path)) {
    stop("`path` must identify an existing file.", call. = FALSE)
  }

  extension <- tolower(tools::file_ext(path))

  if (extension == "csv") {
    header <- readLines(path, n = 1L, warn = FALSE, encoding = "UTF-8")
    if (length(header) == 0L) {
      stop("The CSV file is empty.", call. = FALSE)
    }

    separators <- c(",", ";", "\t")
    separator_counts <- vapply(
      separators,
      function(separator) {
        lengths(regmatches(header, gregexpr(separator, header, fixed = TRUE)))
      },
      integer(1)
    )
    separator <- separators[which.max(separator_counts)]

    out <- utils::read.table(
      path,
      header = TRUE,
      sep = separator,
      quote = "\"",
      comment.char = "",
      check.names = FALSE,
      stringsAsFactors = FALSE,
      fill = TRUE,
      fileEncoding = "UTF-8-BOM"
    )

    if (ncol(out) == 1L && grepl("[,;\t]", names(out)[1L])) {
      stop(
        "The CSV header was not separated into columns. Check the delimiter and file encoding.",
        call. = FALSE
      )
    }

    return(.gqr_check_data(out))
  }

  if (extension == "rds") {
    object <- readRDS(path)
    return(.gqr_check_data(object))
  }

  if (extension %in% c("rda", "rdata")) {
    data_environment <- new.env(parent = emptyenv())
    object_names <- load(path, envir = data_environment)

    data_frames <- object_names[
      vapply(
        object_names,
        function(name) is.data.frame(get(name, envir = data_environment)),
        logical(1)
      )
    ]

    if (length(data_frames) == 1L) {
      return(.gqr_check_data(get(data_frames, envir = data_environment)))
    }

    expected_name <- tools::file_path_sans_ext(basename(path))
    if (expected_name %in% data_frames) {
      return(.gqr_check_data(get(expected_name, envir = data_environment)))
    }

    stop(
      "An RDA/RData file must contain exactly one data frame, or a data frame named after the file.",
      call. = FALSE
    )
  }

  stop("Supported file types are CSV, RDS, RDA, and RData.", call. = FALSE)
}

#' Filter respondents before Generalised Q analysis
#'
#' @description
#' Retains a subset of respondents by identifier and/or named column rules
#' before the synthetic statement matrix is constructed.
#'
#' @param data A respondent-level data frame.
#' @param id_col Optional name of the respondent identifier column.
#' @param ids Optional vector of identifiers to retain.
#' @param filters Optional named list. For a numeric column, a numeric vector of
#'   length two defines an inclusive range. Other vectors define allowed values.
#'   A function may also be supplied and must return one logical value per row.
#'
#' @return A data frame containing the retained respondents in their original
#'   order.
#'
#' @details
#' Identifier and filter conditions are combined with logical AND. Missing
#' filter results are treated as `FALSE`. An error is returned when no
#' respondents remain, preventing a later failure during W-matrix construction.
#'
#' @references
#' Dentinho, T. P., Kourtit, K., & Nijkamp, P. (2023). Generalized Q analysis
#' as a new tool in social science research: A pedagogical introduction.
#' *Eastern Journal of European Studies*, **14**(2), 5--21.
#' \doi{10.47743/ejes-2023-0201}
#' @examples
#' dat <- gqr_example_data("dummy_data")
#'
#' gqr_filter_data(
#'   dat,
#'   id_col = "Respondent",
#'   ids = c("R1", "R2", "R3")
#' )
#'
#' gqr_filter_data(
#'   dat,
#'   filters = list(Q1 = range(dat$Q1), Q2 = function(x) x >= median(x))
#' )
#'
#' @export
gqr_filter_data <- function(data, id_col = NULL, ids = NULL, filters = NULL) {
  data <- .gqr_check_data(data)
  keep <- rep(TRUE, nrow(data))

  if (!is.null(ids)) {
    if (is.null(id_col)) {
      stop("`id_col` is required when `ids` is supplied.", call. = FALSE)
    }
    id_col <- .gqr_check_columns(data, id_col, "id_col")
    keep <- keep & as.character(data[[id_col]]) %in% as.character(ids)
  }

  if (!is.null(filters)) {
    if (!is.list(filters) || is.null(names(filters))) {
      stop("`filters` must be a named list.", call. = FALSE)
    }

    filter_columns <- .gqr_check_columns(data, names(filters), "filters")

    for (column in filter_columns) {
      rule <- filters[[column]]
      values <- data[[column]]

      if (is.function(rule)) {
        selected <- rule(values)
        if (!is.logical(selected) || length(selected) != nrow(data)) {
          stop(
            "Filter function for `", column,
            "` must return one logical value per row.",
            call. = FALSE
          )
        }
      } else if (is.numeric(values) &&
                 is.numeric(rule) &&
                 length(rule) == 2L) {
        selected <- values >= min(rule) & values <= max(rule)
      } else {
        selected <- values %in% rule
      }

      selected[is.na(selected)] <- FALSE
      keep <- keep & selected
    }
  }

  result <- data[keep, , drop = FALSE]
  if (nrow(result) == 0L) {
    stop("No respondents remain after filtering.", call. = FALSE)
  }

  result
}

#' Transform simple-statement variables
#'
#' @description
#' Applies a selected transformation to numeric simple-statement columns while
#' leaving identifiers and respondent covariates unchanged.
#'
#' @param data A data frame.
#' @param columns Character vector naming numeric columns to transform.
#' @param method One of `"none"`, `"standardise"`, `"normalise"`,
#'   `"relative"`, or `"entropy"`.
#' @param margin One of `"auto"`, `"columns"`, or `"rows"`. In automatic
#'   mode, standardisation and min--max normalisation operate by statement
#'   column; relative importance and entropy contributions operate within each
#'   respondent row.
#'
#' @return A data frame with the selected columns replaced by transformed
#'   numeric values.
#'
#' @details
#' `standardise` calculates z-scores; `normalise` maps finite values to the
#' interval from zero to one; `relative` divides values by their finite sum; and
#' `entropy` returns the element-wise Shannon contribution `-p * log(p)` after
#' converting magnitudes to proportions. Constant or zero-sum vectors are
#' returned as zero vectors. Missing values remain missing.
#'
#' The appropriate margin depends on the meaning of the data. The automatic
#' choice is a convenience, not a substitute for a study-specific decision.
#'
#' @references
#' Dentinho, T. P., Kourtit, K., & Nijkamp, P. (2023). Generalized Q analysis
#' as a new tool in social science research: A pedagogical introduction.
#' *Eastern Journal of European Studies*, **14**(2), 5--21.
#' \doi{10.47743/ejes-2023-0201}
#' @examples
#' dat <- gqr_example_data("dummy_data")
#'
#' standardised <- gqr_transform_data(
#'   dat,
#'   columns = paste0("Q", 1:9),
#'   method = "standardise"
#' )
#'
#' relative <- gqr_transform_data(
#'   dat,
#'   columns = paste0("Q", 1:9),
#'   method = "relative",
#'   margin = "rows"
#' )
#'
#' @export
gqr_transform_data <- function(
    data,
    columns,
    method = c("none", "standardise", "normalise", "relative", "entropy"),
    margin = c("auto", "columns", "rows")) {

  data <- .gqr_check_data(data)
  columns <- .gqr_check_columns(data, columns, "columns")
  .gqr_check_numeric(data, columns)

  method <- match.arg(method)
  margin <- match.arg(margin)

  if (method == "none" || length(columns) == 0L) {
    return(data)
  }

  if (margin == "auto") {
    margin <- if (method %in% c("relative", "entropy")) "rows" else "columns"
  }

  transform_function <- switch(
    method,
    standardise = .gqr_scale_vector,
    normalise = .gqr_normalise_vector,
    relative = .gqr_relative_vector,
    entropy = .gqr_entropy_vector
  )

  values <- as.matrix(data[columns])
  storage.mode(values) <- "double"

  if (margin == "columns") {
    transformed <- values
    for (j in seq_len(ncol(values))) {
      transformed[, j] <- transform_function(values[, j])
    }
  } else {
    transformed <- values
    for (i in seq_len(nrow(values))) {
      transformed[i, ] <- transform_function(values[i, ])
    }
  }

  data[columns] <- as.data.frame(
    transformed,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  data
}

#' Prepare respondent data and analytical column roles
#'
#' @description
#' Validates the respondent-level input, assigns simple-statement, covariate,
#' and identifier roles, applies optional respondent filters, and transforms
#' the analytical variables. The returned object can be passed directly to
#' [gqr_make_w()].
#'
#' @param data A respondent-level data frame.
#' @param analysis_cols Character vector naming the numeric simple-statement
#'   variables used to construct synthetic combinations.
#' @param covariate_cols Optional respondent-level variables used for component
#'   interpretation.
#' @param id_col Optional name of a unique respondent identifier column.
#' @param transform Transformation passed to [gqr_transform_data()].
#' @param transform_margin Margin passed to [gqr_transform_data()].
#' @param ids Optional respondent identifiers to retain.
#' @param filters Optional named filter list passed to [gqr_filter_data()].
#'
#' @return An object of class `gqr_prepared_data` containing the filtered raw
#'   data, transformed data, resolved respondent identifiers, column roles, and
#'   transformation settings.
#'
#' @details
#' Analysis and covariate columns must not overlap. If `id_col` is omitted,
#' stable generated identifiers are used. Preparation does not construct the
#' synthetic design; use [gqr_estimate_design()], [gqr_generate_dummies()], and
#' [gqr_make_w()] for the next stages.
#'
#' @references
#' Dentinho, T. P., Kourtit, K., & Nijkamp, P. (2023). Generalized Q analysis
#' as a new tool in social science research: A pedagogical introduction.
#' *Eastern Journal of European Studies*, **14**(2), 5--21.
#' \doi{10.47743/ejes-2023-0201}
#' @examples
#' dat <- gqr_example_data("dummy_data")
#'
#' prepared <- gqr_prepare_data(
#'   dat,
#'   analysis_cols = paste0("Q", 1:9),
#'   id_col = "Respondent",
#'   transform = "standardise"
#' )
#'
#' prepared$analysis_cols
#' prepared$ids
#'
#' @export
gqr_prepare_data <- function(
    data,
    analysis_cols,
    covariate_cols = NULL,
    id_col = NULL,
    transform = "none",
    transform_margin = "auto",
    ids = NULL,
    filters = NULL) {

  data <- .gqr_check_data(data)
  analysis_cols <- .gqr_check_columns(data, analysis_cols, "analysis_cols")
  covariate_cols <- .gqr_check_columns(data, covariate_cols, "covariate_cols")

  if (length(analysis_cols) == 0L) {
    stop("Select at least one analysis column.", call. = FALSE)
  }
  .gqr_check_numeric(data, analysis_cols)

  if (!is.null(id_col)) {
    id_col <- .gqr_check_columns(data, id_col, "id_col")
    if (length(id_col) != 1L) {
      stop("`id_col` must name exactly one column.", call. = FALSE)
    }
  }

  overlap <- intersect(analysis_cols, covariate_cols)
  if (length(overlap) > 0L) {
    stop(
      "Analysis columns and covariates must not overlap: ",
      paste(overlap, collapse = ", "),
      call. = FALSE
    )
  }

  filtered <- gqr_filter_data(
    data,
    id_col = id_col,
    ids = ids,
    filters = filters
  )

  transformed <- gqr_transform_data(
    filtered,
    columns = analysis_cols,
    method = transform,
    margin = transform_margin
  )

  ids_resolved <- .gqr_make_ids(transformed, id_col)

  structure(
    list(
      data_raw = filtered,
      data = transformed,
      analysis_cols = analysis_cols,
      covariate_cols = covariate_cols,
      id_col = id_col,
      ids = ids_resolved,
      transform = transform,
      transform_margin = transform_margin
    ),
    class = "gqr_prepared_data"
  )
}
