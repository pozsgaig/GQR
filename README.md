# GQR: Generalised Q Analysis in R

[![R >= 4.1.0](https://img.shields.io/badge/R-%E2%89%A5%204.1.0-276DC3.svg)](https://www.r-project.org/)
[![License: GPL-3](https://img.shields.io/badge/license-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.html)
[![Development status](https://img.shields.io/badge/status-development-orange.svg)](https://github.com/pozsgaig/GQR)

**GQR** provides programmatic and graphical tools for **Generalised Q analysis**. The package implements the framework introduced by Dentinho, Kourtit and Nijkamp (2023), in which a large set of synthetic combined statements is constructed from smaller groups of simple ranked or scored statements.

The package supports the complete workflow:

- preparation, filtering and transformation of respondent-level data;
- full, grouped one-per-group and random synthetic-statement designs;
- construction of the binary design matrix `D` and synthetic evaluation matrix `W`;
- principal component analysis with optional Varimax rotation;
- regression-based interpretation of statement content and respondent covariates;
- a bundled Shiny application for interactive analysis and visualisation.

> **Development status:** GQR is currently an early development release. Results should be checked carefully and the package interface may still change.

### What changed in 0.1.2

Version 0.1.2 extends the Shiny workflow in three main areas. The new **Reproducible R script** tab converts the current analysis into concise executable R code, including current respondent filters and the final settings of the main plots. Uploaded data now handle common UTF-8 and Windows encodings more robustly, and imported headings are converted to stable ASCII, R-compatible names. On the Data tab, columns can be renamed and numeric-coded categorical variables can be marked explicitly as **Factor** variables. Component-Covariate plots now distinguish continuous and categorical covariates, use continuous colour scales for numeric variables and revised discrete palettes for groups, and support categorical hull/ellipse grouping consistently. Version 0.1.2 also fixes ID handling in PCA/W displays and numeric labels in the W preview. See [`NEWS.md`](NEWS.md) for the full change log.

### What changed in 0.1.1

Version 0.1.1 focuses on scalability, numerical compatibility, and consistency between the graphical and programmatic interfaces. Large ordinary-PCA analyses can use the exact compact `gqr_pca_design()` engine without materialising the full `W` matrix; the Shiny app warns before very large ungrouped designs, shows progress, and can stop background calculations. SPSS-style PCA again uses the original `psych::principal()` workflow for compatibility with established results. Grouped statement-regression heatmaps preserve group order, separators, group labels, and explicit zero/reference rows. The bundled dummy example now also contains one numeric and one two-level factor covariate. See [`NEWS.md`](NEWS.md) for the full change log.

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
roles <- gqr_example_roles("dummy_data")

fit <- gqr_analysis(
  data = dat,
  analysis_cols = roles$analysis_cols,
  id_col = roles$id_col,
  covariate_cols = roles$covariate_cols,
  dummy_mode = "all",
  n_components = 3,
  rotation = "varimax"
)

fit
summary(fit)$variance
head(fit$pca$scores)
head(fit$pca$loadings)
head(fit$respondent_regression$coefficients)
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

# For small analyses, W can be materialised explicitly:
W <- gqr_make_w(prepared, D = D)

pca <- gqr_pca(
  W,
  n_components = 3,
  rotation = "varimax"
)

# For large analyses, ordinary PCA can be computed exactly without
# materialising the complete W matrix:
pca_fast <- gqr_pca_design(
  prepared,
  D,
  n_components = 3,
  rotation = "varimax"
)

statement_models <- gqr_regress_statements(
  pca = pca,
  D = D,
  standardise = TRUE
)
```

## Performance with large designs

Full binary designs grow as `2^m`, where `m` is the number of simple statements. Always inspect the expected design size before creating it:

```r
gqr_estimate_design(
  variables = prepared$analysis_cols,
  mode = "all",
  n_respondents = nrow(prepared$data)
)
```

For ordinary PCA, GQR can avoid the main large-matrix bottleneck. Since `W = D %*% t(V)`, the rank of `W` cannot exceed the number of original statements. `gqr_pca_design()` therefore performs an exact PCA in the much smaller statement space and reconstructs only the retained component scores. `gqr_analysis()` selects this compact engine automatically for sufficiently large W matrices.

The optional SPSS-style correlation workflow deliberately retains the original implementation based on `psych::cor.smooth()` and `psych::principal()`. This keeps component ordering, orientation, Varimax behaviour, and downstream statement-regression coefficients compatible with earlier GQR/SPSS-style analyses. Because this workflow operates on the respondent correlation matrix derived from W, it still materialises W and is handled separately from the compact ordinary-PCA engine.

When a W matrix is required for inspection, `gqr_make_w()` can materialise only selected rows:

```r
W_preview <- gqr_make_w(
  prepared,
  D = D,
  rows = 1:100
)
```

Long-running core operations also accept optional `progress` and `cancel` callbacks. The Shiny application runs large dummy and PCA calculations in a separate R process, displays progress, and provides a **Stop calculation** button so the main R session remains responsive.

Matrix multiplication still uses R's BLAS implementation, which may itself be multithreaded. The compact algebra generally provides a much larger gain than explicitly parallelising the already optimised dense matrix multiplication.

## Shiny application

Launch the graphical interface with:

```r
run_gqr()
```

The app provides tabs for:

1. package and methodological overview;
2. data import, labels, roles, transformations and grouping;
3. dummy-matrix generation and construction of `W`;
4. PCA settings and diagnostics;
5. Statement–Component Regression for interpreting components from statement content;
6. Component–Covariate Regression for relating respondent loadings to respondent metadata;
7. **Reproducible R script**, which records input-file provenance and converts the current frozen analysis into copyable/downloadable executable R code.

For uploaded inputs, GQR tries common UTF-8 and Windows/Latin encodings and converts imported headings to unique ASCII, R-compatible names so column references remain stable across platforms. The Data tab also allows direct column renaming and explicit **Factor** marking for numeric-coded categories; analysis and covariate roles are kept mutually exclusive.

The reproducibility tab records the original filename and, optionally, checksum information rather than Shiny's temporary upload path. It writes the final grouping, current respondent filters, factor/rename choices, analytical settings, and the final settings of the main generated plots into the script. Plot code is resolved before export, so unused GUI branches are omitted.

For grouped designs, the statement-regression heatmap follows the group order, draws separators between groups, labels each group at the right, and retains omitted reference or constant/all-zero dummy variables as explicit zero coefficients. In Component-Covariate plots, numeric colour variables use continuous scales while categorical variables use discrete palettes designed to remain distinct for small numbers of groups; black is reserved for larger palettes.

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

The synthetic `dummy_data` object contains nine statement variables (`Q1`--`Q9`) plus `Numeric_covariate` and `Factor_covariate`, a two-level factor. Recommended example roles are available programmatically and are the same defaults used by the Shiny Data tab:

```r
gqr_example_roles("dummy_data")
gqr_example_roles("gardening")
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
?gqr_pca_design
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
