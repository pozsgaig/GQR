#' Run the GQR Shiny application
#'
#' @description
#' Launches the complete graphical interface for Generalised Q analysis. The
#' application uses the same analytical concepts as the programmatic API and
#' includes data preparation, variable roles and labels, transformations,
#' grouping, synthetic-statement design, W-matrix graphics, PCA diagnostics,
#' regression summaries, covariate plots, and downloads.
#'
#' @param ... Arguments passed to [shiny::runApp()], such as `launch.browser`,
#'   `host`, or `port`.
#'
#' @return Invisibly returns the value produced by [shiny::runApp()]. The main
#'   effect is launching the application.
#'
#' @details
#' Shiny and plotting packages are Suggested dependencies so that the
#' programmatic GQR workflow can be installed without the graphical stack. If a
#' required graphical dependency is missing, `run_gqr()` reports the packages
#' that must be installed.
#'
#' The method implemented by the application is based on Dentinho, Kourtit and
#' Nijkamp (2023). The bundled gardening example is a selected-column extract
#' from Varga-Szilay et al. (2026).
#'
#' @references
#' Dentinho, T. P., Kourtit, K., & Nijkamp, P. (2023). Generalized Q analysis
#' as a new tool in social science research: A pedagogical introduction.
#' *Eastern Journal of European Studies*, **14**(2), 5--21.
#' \doi{10.47743/ejes-2023-0201}
#'
#' Varga-Szilay, Z., Šerić Jelaska, L., Vilumets, S., Barševskis, A., Benedek,
#' K., Bevk, D., Jojczyk, A., Krištín, A., Růžičková, J., Veromann, E., Fetykó,
#' K. G., Szövényi, G., & Pozsgai, G. (2026). A multilingual, multi-country
#' dataset on gardening and biodiversity awareness across Central and Eastern
#' Europe. *Scientific Data*. \doi{10.1038/s41597-026-07887-9}
#'
#' @examples
#' if (interactive()) {
#'   run_gqr()
#' }
#'
#' @export
run_gqr <- function(...) {
  required <- c(
    "shiny", "tidyr", "broom", "dplyr", "DT", "purrr", "tibble",
    "readr", "ggplot2", "ggnewscale", "Polychrome", "RColorBrewer",
    "scales"
  )

  missing <- required[
    !vapply(required, requireNamespace, logical(1), quietly = TRUE)
  ]

  if (length(missing) > 0L) {
    stop(
      "Install the following packages to run the Shiny application: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  app <- system.file("shiny", package = "GQR")
  if (!nzchar(app)) {
    stop("The installed Shiny application could not be found.", call. = FALSE)
  }

  shiny::runApp(app, ...)
}
