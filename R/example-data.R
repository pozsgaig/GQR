# Bundled example datasets ------------------------------------------------

.gqr_package_path <- function() {
  namespace <- environment(gqr_example_data)

  namespace_path <- tryCatch(
    getNamespaceInfo(namespace, "path"),
    error = function(e) ""
  )

  candidates <- unique(c(
    namespace_path,
    system.file(package = "GQR")
  ))
  candidates <- candidates[nzchar(candidates) & dir.exists(candidates)]

  if (length(candidates) == 0L) {
    return("")
  }

  candidates[1L]
}

.gqr_example_path <- function(dataset) {
  filename <- paste0(dataset, ".RDA")
  package_path <- .gqr_package_path()

  candidates <- unique(c(
    file.path(package_path, "inst", "extdata", filename),
    file.path(package_path, "extdata", filename),
    system.file("extdata", filename, package = "GQR")
  ))

  found <- candidates[nzchar(candidates) & file.exists(candidates)]
  if (length(found) == 0L) {
    return("")
  }

  found[1L]
}

#' Load a bundled GQR example dataset
#'
#' @description
#' Loads one of the RDA example files bundled with GQR and returns it directly
#' as a data frame. The same objects are conventional package datasets and can
#' also be loaded with [utils::data()].
#'
#' @param dataset Either `"dummy_data"` or `"gardening"`.
#'
#' @return A data frame. `dummy_data` has 10 rows and 10 columns;
#'   `gardening` has 5,524 rows and 25 columns.
#'
#' @details
#' The accessor is convenient in scripts and in the Shiny application because
#' it returns the object rather than creating it in the calling environment.
#' To use the standard package-data interface, call
#' `data("dummy_data", package = "GQR")` or
#' `data("gardening", package = "GQR")`.
#'
#' The gardening object contains selected columns from the multilingual Central
#' and Eastern European gardening questionnaire dataset described by
#' Varga-Szilay et al. (2026). See [gardening] for a detailed variable
#' description and detailed provenance.
#'
#' @references
#' Varga-Szilay, Z., Šerić Jelaska, L., Vilumets, S., Barševskis, A., Benedek,
#' K., Bevk, D., Jojczyk, A., Krištín, A., Růžičková, J., Veromann, E., Fetykó,
#' K. G., Szövényi, G., & Pozsgai, G. (2026). A multilingual, multi-country
#' dataset on gardening and biodiversity awareness across Central and Eastern
#' Europe. *Scientific Data*. \doi{10.1038/s41597-026-07887-9}
#'
#' @examples
#' dummy <- gqr_example_data("dummy_data")
#' names(dummy)
#'
#' garden <- gqr_example_data("gardening")
#' garden[1:3, 1:5]
#'
#' @export
gqr_example_data <- function(dataset = c("dummy_data", "gardening")) {
  dataset <- match.arg(dataset)
  path <- .gqr_example_path(dataset)

  if (!nzchar(path)) {
    stop(
      "The bundled RDA file for `", dataset, "` could not be found.",
      call. = FALSE
    )
  }

  out <- gqr_read(path)

  expected <- switch(
    dataset,
    dummy_data = c("Respondent", paste0("Q", 1:9)),
    gardening = c("ID", "Country_code", "NUTS")
  )

  missing <- setdiff(expected, names(out))
  if (length(missing) > 0L) {
    stop(
      "The bundled `", dataset, "` object has an unexpected structure. Missing: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  out
}
