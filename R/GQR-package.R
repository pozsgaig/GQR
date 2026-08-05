#' GQR: Generalised Q Analysis in R
#'
#' @description
#' **GQR** implements the Generalised Q method introduced by Dentinho, Kourtit
#' and Nijkamp (2023). It provides a reproducible analytical workflow in
#' ordinary R functions and an optional graphical Shiny interface.
#'
#' Generalised Q analysis extends traditional Q methodology by constructing a
#' large set of synthetic combined statements from smaller groups of simple
#' ranked or scored statements. This can reduce the burden placed on
#' respondents, permit analyses with more respondents than simple statements,
#' and provide quantitative support for naming and interpreting extracted
#' components.
#'
#' @section Mathematical orientation:
#' Let `V` be a respondent-by-statement matrix and `D` a binary design matrix
#' whose rows indicate which simple statements belong to each synthetic
#' combination. GQR constructs
#'
#' \deqn{W = D V^\mathsf{T}.}
#'
#' Rows of `W` are synthetic combined statements and columns are respondents.
#' Principal component analysis is therefore carried out with combinations as
#' observations and respondents as variables. The returned PCA scores describe
#' combinations; the loadings describe respondents.
#'
#' @section Installation:
#' Install the current development version from GitHub with
#' `remotes::install_github("pozsgaig/GQR", dependencies = TRUE,
#' upgrade = "never")`. The source repository is
#' <https://github.com/pozsgaig/GQR>.
#'
#' For package development, run `devtools::document()`, `devtools::test()`, and
#' `devtools::check()` before installation. All manual pages are generated from
#' roxygen2 comments in `R/`; files under `man/` are generated outputs.
#'
#' Load the package with `library(GQR)`. The optional graphical interface is
#' started with [run_gqr()].
#'
#' @section Complete workflow:
#' The programmatic workflow is divided into explicit stages:
#'
#' 1. [gqr_read()] reads CSV, RDS, RDA, or RData files.
#' 2. [gqr_filter_data()], [gqr_transform_data()], and [gqr_prepare_data()]
#'    select respondents, transform analysis variables, and assign column roles.
#' 3. [gqr_estimate_design()] estimates design size before allocation.
#' 4. [gqr_generate_dummies()] creates full, grouped one-per-group, or random
#'    synthetic-statement designs.
#' 5. [gqr_make_w()] calculates the synthetic evaluation matrix.
#' 6. [gqr_pca()] performs PCA with optional Varimax rotation.
#' 7. [gqr_regress_statements()] and [gqr_regress_respondents()] assist
#'    component interpretation.
#'
#' [gqr_analysis()] runs these stages in one call. [run_gqr()] starts the Shiny
#' application.
#'
#' @section Interpretation:
#' Statement-content regressions identify which simple statements distinguish
#' high and low scores on each component. Respondent-covariate regressions relate
#' respondent loadings to metadata such as age group, location, or other study
#' variables. In grouped one-per-group designs, one statement from each group is
#' omitted as a baseline to avoid exact collinearity.
#'
#' @section Assumptions and limitations:
#' The construction of `W` assumes additive separability: a combined
#' statement's synthetic evaluation is represented by the sum of its
#' constituent evaluations. Full binary designs grow as `2^m`; researchers
#' should use [gqr_estimate_design()] and prefer grouped or random designs when
#' exhaustive enumeration is not feasible. PCA and regression results remain
#' exploratory and require substantive interpretation.
#'
#' @section Bundled data:
#' `dummy_data` is a small synthetic example. `gardening` contains selected
#' columns from the multilingual Central and Eastern European gardening
#' questionnaire dataset published by Varga-Szilay et al. (2026). The bundled
#' extract is provided for demonstrating GQR and does not replace the full
#' published dataset.
#'
#' @section References:
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
#' @seealso [gqr_methodology], [gqr_analysis()], [run_gqr()]
#' @docType package
#' @name GQR-package
#' @aliases GQR
#' @keywords package
"_PACKAGE"
