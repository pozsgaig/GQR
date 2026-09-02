# GQR 0.1.1: performance, PCA, and regression-display changes

This development note gives additional technical detail for changes summarised
in `NEWS.md`. It is intended for the GitHub repository and is excluded from the
built R package.

## Exact compact PCA for large designs

The original workflow explicitly constructed the synthetic evaluation matrix

```text
W = D %*% t(V)
```

and then performed PCA on W. For an ungrouped full design, the number of rows
of D and W grows as `2^m`, so storing W and applying a full SVD can become the
main time and memory bottleneck.

For ordinary centred/scaled PCA, GQR 0.1.1 can instead use
`gqr_pca_design()`. Since W is the product of the design matrix D and the
respondent-by-statement data matrix V, its rank cannot exceed the number of
original analysis variables. GQR uses this low-rank structure to perform the
same PCA in the smaller statement space and reconstruct only the retained
component scores. This is algebraically exact; it is not randomised or
approximate PCA.

The test suite compares the compact and materialised-W solutions numerically.
Eigenvalues agree to numerical precision and scores/loadings agree up to the
arbitrary sign orientation of each PCA axis.

`gqr_analysis(pca_engine = "auto")` selects the compact engine for sufficiently
large ordinary-PCA problems. When compact PCA is used, W can also remain
unmaterialised (`materialise_w = "auto"` or `"never"`).

## SPSS-style PCA remains separate

The SPSS-style correlation workflow intentionally does not use the compact
ordinary-PCA engine. It constructs the respondent correlation matrix from W,
smooths a non-positive-definite matrix when necessary with
`psych::cor.smooth()`, and uses `psych::principal()` for the retained and
Varimax-rotated components. This restores compatibility with the earlier
GQR/SPSS-style calculations, including displayed component order/orientation
and downstream Statement–Component Regression coefficients.

A dedicated compatibility test guards known coefficients from the bundled
dummy example so that replacing the rotation/scoring implementation cannot
silently change established results.

## W previews and design warnings

`gqr_make_w()` accepts selected design rows, so the Shiny Dummies/PCA views can
show a small W preview without first allocating the complete W matrix.

`gqr_estimate_design()` is used before allocation to estimate the number of
synthetic statements and W memory requirement. The Shiny Data tab warns users
before proceeding with a large ungrouped design and offers the opportunity to
define groups instead.

## Progress and cancellation

Core functions expose optional progress and cancellation callbacks. In Shiny,
heavy dummy generation and PCA are run through `callr` in a separate R process.
This permits a running calculation to be terminated with **Stop calculation**
without terminating the main R/Shiny session, including during an indivisible
SVD/eigendecomposition step.

Explicit R-level parallel matrix multiplication was not added. `%*%`,
`crossprod()`, SVD and eigendecomposition use BLAS/LAPACK and can already be
multithreaded depending on the user's R installation. Avoiding the full matrix
and full SVD generally yields a larger benefit while avoiding duplicated memory
across worker processes.

## Grouped Statement–Component Regression heatmap

For grouped designs, heatmap rows now follow the grouping/order frozen when D
was generated rather than alphabetical order. Separator lines mark group
boundaries. Group names use a separate strip to the right of the PC columns,
so they cannot overlap the final coefficient column. Reference variables and
constant/all-zero dummy variables remain visible as explicit zero rows.

## GUI/programmatic synchronisation

The Shiny application is a front end to the package core rather than a second
implementation. As of 0.1.1 it delegates:

- transformation to `gqr_transform_data()`;
- design estimation to `gqr_estimate_design()`;
- dummy generation to `gqr_generate_dummies()`;
- W construction/preview to `gqr_make_w()`;
- ordinary compact PCA to `gqr_pca_design()`;
- materialised-W/SPSS PCA to `gqr_pca()`;
- statement regressions to `gqr_regress_statements()`;
- respondent/covariate regressions to `gqr_regress_respondents()`;
- bundled-example defaults to `gqr_example_roles()`.

This reduces the risk that an analysis performed through Shiny differs from the
same analysis performed in an R script.
