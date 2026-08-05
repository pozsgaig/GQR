# GQR: Generalised Q Analysis in R

[![R >= 4.1.0](https://img.shields.io/badge/R-%E2%89%A5%204.1.0-276DC3.svg)](https://www.r-project.org/)
[![License: GPL-3](https://img.shields.io/badge/license-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.html)
[![Development status](https://img.shields.io/badge/status-development-orange.svg)](https://github.com/pozsgaig/GQR)

**GQR** provides programmatic and graphical tools for **Generalised Q analysis**. The package implements the framework introduced by Dentinho, Kourtit and Nijkamp (2023), in which a large set of synthetic combined statements is constructed from smaller groups of simple ranked or scored statements.

The package supports the complete workflow:

- preparation, filtering and transformation of respondent-level data;
- full, grouped one-per-group and random synthetic-statement designs;
- construction of the binary design (dummy) matrix `D` and synthetic evaluation matrix `W`;
- principal component analysis with optional Varimax rotation;
- regression-based interpretation of statement content and respondent covariates;
- a bundled Shiny application for interactive analysis and visualisation.

> **Development status:** GQR is currently an early development release. Results should be checked carefully and the package interface may still change.

## Methodological basis

Generalised Q analysis extends traditional Q methodology by constructing synthetic combined statements from smaller groups of simple statements. These combinations are represented by **dummy variables**: binary indicators in which `1` means that a simple statement is included in a synthetic statement and `0` means that it is excluded.

For a respondent-by-statement matrix `V` and a binary design matrix `D`, GQR constructs:

```text
W = D %*% t(V)
```

The rows of `D` and `W` represent synthetic combined statements. The columns of `D` represent the original simple statements, while the columns of `W` represent respondents.

The methodological basis is:

> Dentinho, T. P., Kourtit, K., & Nijkamp, P. (2023). Generalized Q analysis as a new tool in social science research: A pedagogical introduction. *Eastern Journal of European Studies*, 14(2), 5–21. https://doi.org/10.47743/ejes-2023-0201

See `?gqr_methodology` and the methodology vignette for the assumptions, matrix orientation and interpretation of the results.

## Installation

GQR is currently available from GitHub.

```r
install.packages("remotes")

remotes::install_github(
  "pozsgaig/GQR",
  dependencies = TRUE,
  upgrade = "never"
)
```

Then load the package:

```r
library(GQR)
```

The repository can also be cloned for development:

```bash
git clone https://github.com/pozsgaig/GQR.git
```

From the cloned package directory:

```r
devtools::document()
devtools::test()
devtools::check()
devtools::install(upgrade = "never")
```

## Quick start

### Full binary design

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
head(fit$pca$scores)
head(fit$pca$loadings)
```

With nine simple statements, the full design contains `2^9 = 512` binary combinations when the empty combination is retained.

### Grouped one-per-group design

Grouped designs select exactly one statement from each thematic group.

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

dim(fit_grouped$D)
summary(fit_grouped)$variance
fit_grouped$statement_regression$baselines
```

Group definitions can also be read from an external CSV file containing the columns `group` and `variable`:

```r
groups <- gqr_read("groups.csv")
```

Example:

```csv
group,variable
Question 1,Q1
Question 1,Q2
Question 1,Q3
Question 2,Q4
Question 2,Q5
Question 2,Q6
Question 3,Q7
Question 3,Q8
Question 3,Q9
```

### Random approximation

Random mode samples a fixed number of binary patterns and is useful when exhaustive enumeration is too large.

```r
fit_random <- gqr_analysis(
  data = dat,
  analysis_cols = paste0("Q", 1:9),
  id_col = "Respondent",
  dummy_mode = "random",
  n_patterns = 500,
  prob = 0.5,
  seed = 42,
  n_components = 3,
  respondent_regression = FALSE
)
```

### Run the workflow step by step

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

## Shiny application

Launch the graphical interface with:

```r
run_gqr()
```

The app provides tabs for:

1. package and methodological overview;
2. data import, labels, roles, transformations and grouping;
3. dummy-design generation and construction of `W`;
4. PCA settings and diagnostics;
5. component interpretation, covariate analyses and downloads.

A detailed guide is available in the Shiny application vignette:

```r
vignette("shiny-application", package = "GQR")
```

## Example datasets

GQR contains two example datasets:

```r
data("dummy_data", package = "GQR")
data("gardening", package = "GQR")
```

They can also be returned directly:

```r
dummy <- gqr_example_data("dummy_data")
garden <- gqr_example_data("gardening")
```

The `gardening` object contains selected columns from the multilingual Central and Eastern European gardening questionnaire dataset. It is included only as an example and does not replace the full published dataset.

> Varga-Szilay, Z., Šerić Jelaska, L., Vilumets, S., Barševskis, A., Benedek, K., Bevk, D., Jojczyk, A., Krištín, A., Růžičková, J., Veromann, E., Fetykó, K. G., Szövényi, G., & Pozsgai, G. (2026). A multilingual, multi-country dataset on gardening and biodiversity awareness across Central and Eastern Europe. *Scientific Data*. https://doi.org/10.1038/s41597-026-07887-9

See `?gardening` for detailed provenance and references to the studies that analysed the dataset.

## Documentation

```r
help(package = "GQR")
?gqr_methodology
?gqr_analysis
?gqr_generate_dummies
?gqr_make_w
?gqr_pca
?gqr_regress_statements
?gqr_regress_respondents
?gardening
```

Available vignettes:

```r
vignette("getting-started", package = "GQR")
vignette("methodology", package = "GQR")
vignette("shiny-application", package = "GQR")
```

All manual pages are generated from roxygen2 comments in `R/`. Files under `man/` should not be edited directly.

## Citation

To obtain the recommended citations in R:

```r
citation("GQR")
```

When using GQR, cite both the software and the methodological paper. When using the bundled gardening example, also cite the source data paper.

## Problems and contributions

Report reproducible problems through the [GitHub issue tracker](https://github.com/pozsgaig/GQR/issues). Include:

- the GQR and R versions;
- the operating system;
- a minimal reproducible example;
- the complete warning or error message;
- `sessionInfo()` output where relevant.

Contributions through pull requests are welcome. Please open an issue first for substantial changes so that the proposed scope can be discussed.

## License

GQR is released under the GNU General Public License version 3 or later (`GPL-3`).
