# GQR 0.1.1

This release focuses on performance, numerical compatibility, GUI clarity, and
keeping the Shiny and programmatic workflows on the same analytical core.

## Performance and large designs

- Added the exact compact ordinary-PCA engine `gqr_pca_design()`. Because
  `W = D V^T` has rank no greater than the number of original analysis
  variables, ordinary PCA can be calculated in the much smaller statement
  space without performing an SVD on the complete W matrix. This is an exact
  algebraic reformulation, not an approximation.
- `gqr_analysis()` can select the compact engine automatically for large W
  matrices and can avoid materialising W entirely when it is not needed.
- `gqr_make_w()` can materialise selected rows only, allowing large W matrices
  to be previewed without allocating the complete matrix.
- Added design-size/memory estimation before allocation. The Shiny Data tab now
  warns when users proceed with a potentially very large ungrouped full binary
  design.
- Added progress callbacks and cancellation checkpoints to the core generation,
  W, and compact-PCA functions. The Shiny app runs heavy dummy/PCA calculations
  in a separate R process, displays progress, and provides a **Stop
  calculation** button without terminating the main R session.
- Dense matrix multiplication continues to use R's BLAS/LAPACK backend, which
  may itself be multithreaded. The low-rank reformulation generally avoids much
  more work and memory than explicit R-level parallelisation would save.

## PCA compatibility and regression output

- Restored the original SPSS-style PCA implementation based on
  `psych::cor.smooth()` and `psych::principal()`. This preserves the established
  component ordering/orientation, Varimax behaviour, and downstream
  statement-regression coefficients from the earlier GQR/SPSS-style workflow.
- Added a regression test based on the bundled dummy example to detect future
  changes to the established SPSS-style Varimax coefficient convention.
- Added numerical tests showing that compact ordinary PCA reproduces the
  materialised-W PCA solution to numerical precision, allowing for the
  arbitrary sign orientation of PCA axes.
- Grouped Statement–Component Regression heatmaps now follow the grouping
  order, draw separator lines between groups, show group names in a dedicated
  strip to the right of the component columns, and retain omitted reference or
  constant/all-zero dummy variables explicitly as zero coefficients.

## Shiny interface and package synchronisation

- Renamed the analytical tabs to **Statement–Component Regression** and
  **Component–Covariate Regression** and clarified the PCA heading.
- Added reusable collapsible information boxes to the Dummies, PCA,
  Statement–Component Regression, and Component–Covariate Regression tabs.
- The Shiny app delegates data transformation, design estimation, dummy
  generation, W construction, PCA, statement regression, and respondent
  regression to the exported `gqr_*` package functions. The graphical and
  non-graphical interfaces therefore use the same analytical implementations.
- Added `gqr_example_roles()` so recommended analysis/covariate roles for the
  bundled examples are defined once in the package and reused by the Shiny
  Data tab.

## Example data and documentation

- Extended `dummy_data` with two synthetic respondent-level covariates:
  `Numeric_covariate` and `Factor_covariate` (factor levels `A` and `B`). These
  are automatically selected as the default covariates when **Dummy data** is
  chosen in the Shiny app.
- Updated the package-data documentation, examples, tests, README, and
  vignettes to describe and use the new dummy covariates.
- Added a reproducible `data-raw/dummy_data.R` development script for rebuilding
  both bundled RDA copies of the synthetic example.
- Continued to use roxygen2 comments in `R/` as the single documentation source
  for generated manual pages.

## Development/check fixes

- Corrected Shiny background-PCA tests to verify the actual background-task
  dispatch rather than requiring a direct function call in the UI module.
- Excluded GitHub/development-only files from source-package builds to avoid
  non-package `R CMD check` notes.
- The expected positive-definiteness smoothing warning in the SPSS
  compatibility test is now explicitly tested rather than emitted as an
  incidental test warning.

# GQR 0.1.0

Initial public development release of **GQR: Generalised Q Analysis in R**.

- Implements the Generalised Q workflow described by Dentinho, Kourtit and
  Nijkamp (2023).
- Provides full, grouped one-per-group, and random synthetic-statement designs.
- Constructs the synthetic evaluation matrix and performs PCA with optional
  Varimax rotation.
- Provides statement-content and respondent-covariate regressions.
- Includes a Shiny interface and two bundled RDA example datasets.
- Uses roxygen2 documentation for exported functions, methodology, and data.
