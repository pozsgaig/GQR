#' Small synthetic Generalised Q example
#'
#' A compact dataset supplied for examples and automated tests. It contains ten
#' respondents, nine numeric simple-statement variables, one synthetic numeric
#' covariate, and one synthetic two-level factor covariate. All values are
#' synthetic and have no substantive interpretation.
#'
#' @format A data frame with 10 rows and 12 columns:
#' \describe{
#'   \item{Respondent}{Unique respondent identifier (`R1`--`R10`).}
#'   \item{Q1}{Numeric score for simple statement 1.}
#'   \item{Q2}{Numeric score for simple statement 2.}
#'   \item{Q3}{Numeric score for simple statement 3.}
#'   \item{Q4}{Numeric score for simple statement 4.}
#'   \item{Q5}{Numeric score for simple statement 5.}
#'   \item{Q6}{Numeric score for simple statement 6.}
#'   \item{Q7}{Numeric score for simple statement 7.}
#'   \item{Q8}{Numeric score for simple statement 8.}
#'   \item{Q9}{Numeric score for simple statement 9.}
#'   \item{Numeric_covariate}{Synthetic numeric respondent-level covariate
#'   used to demonstrate component--covariate regression.}
#'   \item{Factor_covariate}{Synthetic factor covariate with two levels, `A`
#'   and `B`, used to demonstrate categorical component--covariate regression.}
#' }
#' @usage data(dummy_data)
#' @examples
#' data("dummy_data", package = "GQR")
#' names(dummy_data)
#' gqr_analysis(
#'   dummy_data,
#'   analysis_cols = paste0("Q", 1:9),
#'   id_col = "Respondent",
#'   covariate_cols = c("Numeric_covariate", "Factor_covariate"),
#'   n_components = 3
#' )
#' @keywords datasets
"dummy_data"

#' Selected columns from the Central and Eastern European gardening survey
#'
#' A selected-column extract from the multilingual questionnaire dataset on
#' garden characteristics, gardening practices, pesticide use, biodiversity
#' support, and environmental awareness described by Varga-Szilay et al.
#' (2026). The source survey covered nine Central and Eastern European
#' countries and was produced in ten languages. The bundled object is intended
#' solely as a worked GQR example and does not replace the complete published
#' dataset or its accompanying documentation.
#'
#' Column names are retained from the supplied extract, including the original
#' spellings `How_importnat_conservation` and `How_important_sel_supply`.
#' Ordinal numeric fields retain the coding of the source extract; consult the
#' data paper and questionnaire documentation before substantive interpretation.
#'
#' @format A data frame with 5,524 rows and 25 columns:
#' \describe{
#'   \item{ID}{Anonymous respondent identifier.}
#'   \item{Country_code}{Two-letter country code.}
#'   \item{NUTS}{NUTS regional code supplied for the respondent.}
#'   \item{Settlement}{Settlement or urbanisation category.}
#'   \item{Gender}{Respondent gender category.}
#'   \item{Age_group}{Respondent age category.}
#'   \item{Education}{Highest education category.}
#'   \item{Children}{Whether children are represented in the household variable supplied in the extract.}
#'   \item{Garden_size}{Garden-size category.}
#'   \item{Garden_type}{Type or tenure context of the garden.}
#'   \item{Gardening_experience}{Categorised duration of gardening experience.}
#'   \item{How_important_making_beautiful}{Ordinal importance assigned to making the garden beautiful.}
#'   \item{How_importnat_conservation}{Ordinal importance assigned to nature or biodiversity conservation.}
#'   \item{How_important_making_money}{Ordinal importance assigned to income or money-related gardening motivation.}
#'   \item{How_important_sel_supply}{Ordinal importance assigned to self-supply or food-production motivation.}
#'   \item{Time_spend}{Ordinal measure of time spent gardening.}
#'   \item{How_often_plant}{Ordinal measure of planting frequency.}
#'   \item{How_important_pesticide}{Ordinal importance assigned to pesticide-related considerations.}
#'   \item{Importance_habitat_loss_urban}{Perceived importance of habitat loss caused by urbanisation.}
#'   \item{Importance_habitat_loss_agriculture}{Perceived importance of habitat loss associated with agriculture.}
#'   \item{Importance_pesticides}{Perceived importance of pesticides as a biodiversity threat.}
#'   \item{Importance_agricultural_intensification}{Perceived importance of agricultural intensification as a biodiversity threat.}
#'   \item{Importance_invasives}{Perceived importance of invasive species as a biodiversity threat.}
#'   \item{Importance_honeybees}{Perceived importance of issues associated with honeybees in the questionnaire's threat section.}
#'   \item{Importance_diseases}{Perceived importance of diseases as a biodiversity threat.}
#' }
#'
#' @references
#' Varga-Szilay, Z., Šerić Jelaska, L., Vilumets, S., Barševskis, A., Benedek,
#' K., Bevk, D., Jojczyk, A., Krištín, A., Růžičková, J., Veromann, E., Fetykó,
#' K. G., Szövényi, G., & Pozsgai, G. (2026). A multilingual, multi-country
#' dataset on gardening and biodiversity awareness across Central and Eastern
#' Europe. *Scientific Data*. \doi{10.1038/s41597-026-07887-9}
#'
#' Varga-Szilay, Z., Fetykó, K. G., Szövényi, G., & Pozsgai, G. (2024).
#' Bridging biodiversity and gardening: Unravelling the interplay of
#' socio-demographic factors, garden practices, and garden characteristics.
#' *Urban Forestry & Urban Greening*, **97**, 128367.
#' \doi{10.1016/j.ufug.2024.128367}
#'
#' Varga-Szilay, Z., Barševskis, A., Benedek, K., Bevk, D., Jojczyk, A.,
#' Krištín, A., Růžičková, J., Šerić Jelaska, L., Veromann, E., Vilumets, S.,
#' Fetykó, K. G., Szövényi, G., & Pozsgai, G. (2025). Improving biodiversity in
#' Central and Eastern European gardens needs regionally scaled strategies.
#' *Urban Forestry & Urban Greening*, **113**, 129074.
#' \doi{10.1016/j.ufug.2025.129074}
#'
#' @usage data(gardening)
#' @examples
#' data("gardening", package = "GQR")
#' dim(gardening)
#' names(gardening)
#' table(gardening$Country_code, useNA = "ifany")
#' @keywords datasets
"gardening"
