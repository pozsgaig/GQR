# Static references keep the Shiny runtime dependencies visible to package
# dependency checks while the application itself continues to use namespace-
# qualified calls and a bundled app directory. This helper is not executed.
.gqr_shiny_dependency_references <- function() {
  invisible(list(
    callr::r_bg,
    dplyr::mutate,
    DT::datatable,
    ggnewscale::new_scale_fill,
    ggplot2::ggplot,
    Polychrome::glasbey.colors,
    purrr::map,
    RColorBrewer::brewer.pal,
    scales::alpha,
    shiny::runApp,
    tibble::tibble,
    tidyr::pivot_longer
  ))
}

#' Run the GQR Shiny application
#'
#' @description
#' Launches the complete graphical interface for Generalised Q analysis. The
#' application uses the same analytical concepts as the programmatic API and
#' includes data preparation, variable roles and labels, transformations,
#' grouping, synthetic-statement design, W-matrix graphics, PCA diagnostics,
#' regression summaries, covariate plots, downloads, and generation of an executable reproducibility script with input-file provenance.
#'
#' @param ... Arguments passed to [shiny::runApp()], such as `launch.browser`,
#'   `host`, or `port`.
#'
#' @return Invisibly returns the value produced by [shiny::runApp()]. The main
#'   effect is launching the application.
#'
#' @details
#' Packages required by the graphical interface are declared as package imports
#' so a normal GQR installation also installs the Shiny runtime dependencies.
#' Before launching, `run_gqr()` verifies that each required namespace can
#' actually be loaded and reports the underlying loading error when it cannot.
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
    "shiny", "tidyr", "callr", "dplyr", "DT", "purrr", "tibble",
    "ggplot2", "ggnewscale", "Polychrome", "psych",
    "RColorBrewer", "scales"
  )

  dependency_errors <- vapply(
    required,
    function(package) {
      tryCatch(
        {
          loadNamespace(package)
          ""
        },
        error = function(e) conditionMessage(e)
      )
    },
    character(1)
  )

  failed <- nzchar(dependency_errors)
  if (any(failed)) {
    details <- paste0(
      required[failed], ": ", dependency_errors[failed],
      collapse = "\n"
    )
    stop(
      "One or more packages required by the GQR Shiny application could not be loaded:\n\n",
      details,
      call. = FALSE
    )
  }

  app <- system.file("shiny", package = "GQR")
  if (!nzchar(app)) {
    stop("The installed Shiny application could not be found.", call. = FALSE)
  }

  shiny::runApp(app, ...)
}
