# GQR: Generalised Q Analysis in R

`GQR` implements the Generalised Q method introduced by Dentinho, Kourtit and
Nijkamp (2023). It is designed for studies in which respondents rank or score
small groups of simple statements, while the analysis requires a much larger
set of combined statements. `GQR` creates those synthetic combinations,
calculates their respondent-specific evaluations, extracts common structures
with principal component analysis, and supports component interpretation with
regression models.

The package can be used through ordinary R functions or through the bundled
Shiny application. Both interfaces use the same analytical orientation:

- rows of `D`: synthetic combined statements;
- columns of `D`: simple statements;
- rows of `W`: synthetic combined statements;
- columns of `W`: respondents;
- PCA scores: positions of synthetic combinations;
- PCA loadings: positions of respondents;
- statement regressions: component scores explained by simple-statement dummies;
- respondent regressions: respondent loadings explained by covariates.

## Methodological basis

The package follows the Generalised Q framework described in:

Dentinho, T. P., Kourtit, K., & Nijkamp, P. (2023). Generalized Q analysis as a
new tool in social science research: A pedagogical introduction. *Eastern
Journal of European Studies*, **14**(2), 5–21.
https://doi.org/10.47743/ejes-2023-0201

For a respondent-by-statement matrix `V` and a binary combination matrix `D`,
GQR constructs:

```text
W = D %*% t(V)
```

The method assumes that a respondent's synthetic evaluation of a combined
statement can be represented by the sum of the evaluations of its constituent
simple statements. See `?gqr_methodology` for the assumptions, interpretation,
and computational implications.

## Installation

The downloadable ZIP is a source directory bundle, not an R binary package.
Extract it first, preferably into a new empty folder named `GQR`.

### Install from the extracted ZIP

```r
install.packages("devtools")

pkg <- "D:/path/to/GQR"

devtools::install(
  pkg,
  upgrade = "never"
)
```

### Document, test, check, and install the development source

```r
pkg <- "D:/path/to/GQR"

devtools::document(pkg)
devtools::test(pkg)
devtools::check(pkg)
devtools::install(pkg, upgrade = "never")
```

`devtools::document()` regenerates all files under `man/` from the roxygen2
comments in `R/`. The `.Rd` files should not be edited directly.

### Build and install a standard source archive

```r
devtools::build("D:/path/to/GQR")

install.packages(
  "D:/path/to/GQR_0.1.0.tar.gz",
  repos = NULL,
  type = "source"
)
```

### Optional graphical dependencies

The core programmatic workflow has a small dependency set. To use the complete
Shiny interface, install the suggested graphical packages when requested by
`run_gqr()`, or install them in advance:

```r
install.packages(c(
  "shiny", "dplyr", "tidyr", "purrr", "tibble", "readr",
  "ggplot2", "ggnewscale", "DT", "broom", "Polychrome",
  "RColorBrewer", "scales"
))
```

Verify the installation:

```r
library(GQR)
packageVersion("GQR")
help(package = "GQR")
```

## Example 1: complete full-design workflow

```r
library(GQR)

dat <- gqr_example_data("dummy_data")

fit <- gqr_analysis(
  data = dat,
  analysis_cols = paste0("Q", 1:9),
  id_col = "Respondent",
  dummy_mode = "all",
  n_components = 3,
  rotation = "varimax",
  respondent_regression = FALSE
)

fit
summary(fit)$variance
head(fit$pca$loadings)
head(fit$pca$scores)
head(fit$statement_regression$coefficients)
```

Nine simple statements produce `2^9 = 512` binary combinations when the empty
combination is retained.

## Example 2: grouped one-per-group design

Grouped designs correspond most closely to the structured recombination
emphasised by Dentinho et al. (2023): exactly one simple statement is selected
from each thematic question or group.

```r
groups <- data.frame(
  group = rep(c("Question 1", "Question 2", "Question 3"), each = 3),
  variable = paste0("Q", 1:9)
)

fit_grouped <- gqr_analysis(
  data = dat,
  analysis_cols = paste0("Q", 1:9),
  id_col = "Respondent",
  dummy_mode = "group_one_per",
  groups = groups,
  n_components = 3,
  rotation = "varimax",
  respondent_regression = FALSE
)

dim(fit_grouped$D)   # 27 combinations by 9 simple statements
summary(fit_grouped)$variance
fit_grouped$statement_regression$baselines
```

## Example 3: random approximation of a large design

```r
fit_random <- gqr_analysis(
  data = dat,
  analysis_cols = paste0("Q", 1:9),
  id_col = "Respondent",
  dummy_mode = "random",
  n_patterns = 250,
  prob = 0.5,
  seed = 42,
  n_components = 3,
  respondent_regression = FALSE
)
```

Random mode is useful when exhaustive enumeration is too large. A seed makes
the sampled design reproducible.

## Example 4: run each analytical stage separately

```r
prepared <- gqr_prepare_data(
  data = dat,
  analysis_cols = paste0("Q", 1:9),
  id_col = "Respondent",
  transform = "standardise"
)

estimate <- gqr_estimate_design(
  variables = prepared$analysis_cols,
  mode = "all",
  n_respondents = nrow(prepared$data)
)

D <- gqr_generate_dummies(
  variables = prepared$analysis_cols,
  mode = "all"
)

W <- gqr_make_w(prepared, D = D)

pca <- gqr_pca(
  W,
  n_components = 3,
  rotation = "varimax"
)

statement_models <- gqr_regress_statements(
  pca = pca,
  D = D,
  standardise = TRUE
)
```

## Example 5: filtering and transformation

```r
selected <- gqr_filter_data(
  dat,
  id_col = "Respondent",
  ids = c("R1", "R2", "R3", "R4", "R5")
)

standardised <- gqr_transform_data(
  selected,
  columns = paste0("Q", 1:9),
  method = "standardise"
)
```

Standardisation and normalisation operate column-wise by default. Relative
importance and entropy contributions operate row-wise in automatic mode,
because they describe the composition of each respondent's statement scores.

## Example 6: gardening data and respondent covariates

The bundled `gardening` object is a selected-column extract from the
multilingual Central and Eastern European gardening questionnaire dataset
published by Varga-Szilay et al. (2026). It is included only as an analysis
example and does not replace the complete published dataset.

```r
data("gardening", package = "GQR")

analysis_columns <- c(
  "How_important_making_beautiful",
  "How_importnat_conservation",
  "How_important_making_money",
  "How_important_sel_supply",
  "Time_spend",
  "How_often_plant",
  "How_important_pesticide"
)

gardening_groups <- data.frame(
  group = c(rep("Motivation", 4), rep("Management", 3)),
  variable = analysis_columns
)

# A larger example; run when needed.
fit_gardening <- gqr_analysis(
  data = gardening,
  analysis_cols = analysis_columns,
  id_col = "ID",
  covariate_cols = c("Country_code", "Age_group", "Garden_size"),
  transform = "standardise",
  dummy_mode = "group_one_per",
  groups = gardening_groups,
  n_components = 4,
  rotation = "varimax"
)
```

Full data reference:

Varga-Szilay, Z., Šerić Jelaska, L., Vilumets, S., Barševskis, A., Benedek, K.,
Bevk, D., Jojczyk, A., Krištín, A., Růžičková, J., Veromann, E., Fetykó, K. G.,
Szövényi, G., & Pozsgai, G. (2026). A multilingual, multi-country dataset on
gardening and biodiversity awareness across Central and Eastern Europe.
*Scientific Data*. https://doi.org/10.1038/s41597-026-07887-9

For the complete variable-level description and detailed provenance, see
`?gardening`.

## Shiny interface

```r
run_gqr()
```

The application includes the landing page, data preparation, column roles and
labels, transformations, grouping, dummy designs, W-matrix graphics, PCA
settings, scree and loading displays, component regressions, covariate plots,
and downloads.

## Documentation

```r
help(package = "GQR")
?gqr_methodology
?gqr_analysis
?gqr_generate_dummies
?gqr_pca
?gardening
```

All manual pages are generated by roxygen2 from comments in `R/`. Do not edit
files under `man/` directly.
