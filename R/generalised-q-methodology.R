#' Methodology underlying GQR
#'
#' @description
#' Generalised Q analysis was proposed by Dentinho, Kourtit and Nijkamp (2023)
#' to address important restrictions of traditional Q methodology. Instead of
#' requiring each respondent to rank a large set of complex statements,
#' respondents can rank or score smaller groups of simple statements. The
#' simple responses are then recombined mathematically into a much larger set of
#' synthetic combined statements.
#'
#' @details
#' Suppose there are `m` simple statements and `n` respondents. The input matrix
#' `V` has respondents in rows and statements in columns. A binary design matrix
#' `D` has statements in columns and synthetic combinations in rows. For every
#' combination, a value of one indicates that the corresponding simple
#' statement is active.
#'
#' GQR supports three design strategies:
#'
#' * **Full binary design:** all `2^m` subsets of the simple statements.
#' * **Grouped one-per-group design:** one statement is selected from every
#'   thematic group. For groups of sizes `r1, r2, ..., rq`, the number of
#'   combinations is the product `r1 * r2 * ... * rq`.
#' * **Random design:** a fixed number of Bernoulli combinations is sampled,
#'   providing a Monte Carlo approximation when enumeration is too large.
#'
#' The synthetic evaluation matrix is
#'
#' \deqn{W = D V^\mathsf{T}.}
#'
#' Thus, `W[i, j]` is the sum of respondent `j`'s evaluations of the simple
#' statements included in synthetic combination `i`.
#'
#' PCA is applied to `W`, with synthetic combinations as observations and
#' respondents as variables. Component scores locate synthetic combinations;
#' respondent loadings locate respondents. Orthogonal Varimax rotation can be
#' used to simplify the loading structure.
#'
#' Two regression stages provide objective aids to interpretation:
#'
#' * regressing combination scores on `D` identifies the simple statements
#'   characterising each component;
#' * regressing respondent loadings on metadata identifies respondent or place
#'   characteristics associated with each component.
#'
#' @section Advantages described by Dentinho et al.:
#' The Generalised Q approach can reduce the number of alternatives ranked at
#' one time, expand the synthetic statement space sufficiently for larger
#' respondent samples, support more systematic component naming, and provide a
#' framework for studying response consistency.
#'
#' @section Central assumption:
#' Synthetic scores are additive. Interactions among constituent statements are
#' not represented unless they are explicitly added to the design or analysed
#' separately. Researchers should judge whether additive recombination is
#' substantively defensible.
#'
#' @section Computational considerations:
#' Full designs grow exponentially. For example, 20 simple statements imply
#' 1,048,576 binary combinations before removal of the empty pattern. The W
#' matrix can then dominate memory use because it contains one value for every
#' combination--respondent pair. Use [gqr_estimate_design()] before generating
#' `D`; grouped or random modes are normally preferable for larger problems.
#'
#' @references
#' Dentinho, T. P., Kourtit, K., & Nijkamp, P. (2023). Generalized Q analysis
#' as a new tool in social science research: A pedagogical introduction.
#' *Eastern Journal of European Studies*, **14**(2), 5--21.
#' \doi{10.47743/ejes-2023-0201}
#'
#' Stephenson, W. (1953). *The Study of Behaviour: Q-technique and its
#' Methodology*. University of Chicago Press.
#'
#' @seealso [gqr_generate_dummies()], [gqr_make_w()], [gqr_pca()],
#'   [gqr_analysis()]
#' @name gqr_methodology
#' @keywords methods
NULL
