test_that("file provenance records original names and checksums", {
  dat <- data.frame(a = 1:3, b = letters[1:3])
  path <- tempfile(fileext = ".csv")
  utils::write.csv(dat, path, row.names = FALSE)
  on.exit(unlink(path), add = TRUE)

  provenance <- gqr_file_provenance(
    path,
    original_name = "survey.csv",
    role = "dataset",
    data = dat
  )

  expect_s3_class(provenance, "gqr_file_provenance")
  expect_identical(provenance$original_name, "survey.csv")
  expect_identical(provenance$role, "dataset")
  expect_identical(provenance$n_rows, 3L)
  expect_identical(provenance$n_cols, 2L)
  expect_true(nchar(provenance$md5) == 32L)
  expect_true(provenance$size_bytes > 0)
})


test_that("generated reproducibility script is valid executable R", {
  state <- list(
    package_version = as.character(utils::packageVersion("GQR")),
    generated_at = as.POSIXct("2026-09-02 12:00:00", tz = "UTC"),
    data = list(
      type = "example",
      dataset = "dummy_data",
      n_rows = nrow(gqr_example_data("dummy_data")),
      n_cols = ncol(gqr_example_data("dummy_data")),
      rename_id_from = "Respondent",
      rename_id_to = "ID"
    ),
    uploads = list(),
    analysis_cols = paste0("Q", 1:4),
    covariate_cols = "Numeric_covariate",
    transform = "none",
    groups = data.frame(
      group = c("G1", "G1", "G2", "G2"),
      variable = paste0("Q", 1:4),
      stringsAsFactors = FALSE
    ),
    group_source = NULL,
    dummy = list(
      mode = "group_one_per",
      include_empty = TRUE,
      allow_ungrouped = TRUE,
      max_patterns = 1000000L
    ),
    pca = list(
      engine = "design",
      method = "prcomp",
      n_components = 2L,
      rotation = "none",
      center = TRUE,
      scale = TRUE,
      SPSS = FALSE,
      impute_mean = TRUE,
      id_col = "ID",
      data_filter = list(filters = NULL, ids = NULL, id_col = "ID")
    ),
    statement_regression = list(
      enabled = TRUE,
      components = c("PC1", "PC2"),
      include_raw = TRUE,
      include_standardised = TRUE
    ),
    respondent_regression = list(
      enabled = TRUE,
      id_col = "ID",
      components = "PC1",
      covariates = "Numeric_covariate",
      filters = NULL,
      ids = NULL,
      limit = NULL
    )
  )

  script <- gqr_reproducible_script(
    state,
    include_session_info = FALSE,
    verify_files = TRUE
  )

  expect_match(script, 'gqr_example_data\\("dummy_data"\\)')
  expect_match(script, "gqr_generate_dummies", fixed = TRUE)
  expect_match(script, "gqr_pca_design", fixed = TRUE)
  expect_match(script, "gqr_regress_statements", fixed = TRUE)
  expect_match(script, "gqr_regress_respondents", fixed = TRUE)
  expect_match(script, "pca$variance_explained", fixed = TRUE)
  expect_false(grepl("pca$var_expl", script, fixed = TRUE))
  expect_silent(parse(text = script))

  env <- new.env(parent = globalenv())
  expect_silent(eval(parse(text = script), envir = env))
  expect_true(inherits(env$pca, "gqr_pca_result"))
  expect_true(inherits(env$statement_regression_standardised, "gqr_statement_regression"))
  expect_true(inherits(env$respondent_regression, "gqr_respondent_regression"))
})


test_that("uploaded file provenance is written into generated scripts", {
  state <- list(
    data = list(
      type = "file",
      name = "final survey.csv",
      md5 = "0123456789abcdef0123456789abcdef",
      size_bytes = 2048,
      n_rows = 100L,
      n_cols = 12L
    ),
    uploads = list(
      list(
        role = "dataset",
        original_name = "draft survey.csv",
        size_bytes = 1024,
        md5 = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
        n_rows = NA_integer_,
        n_cols = NA_integer_,
        status = "uploaded, not used in current analysis"
      ),
      list(
        role = "dataset",
        original_name = "final survey.csv",
        size_bytes = 2048,
        md5 = "0123456789abcdef0123456789abcdef",
        n_rows = 100L,
        n_cols = 12L,
        status = "used in current analysis"
      )
    ),
    analysis_cols = c("Q 1", "Q2"),
    covariate_cols = character(0),
    transform = "none",
    groups = NULL,
    dummy = list(mode = "all", include_empty = TRUE, max_patterns = 1000000L),
    pca = NULL
  )

  script <- gqr_reproducible_script(
    state,
    include_session_info = FALSE,
    verify_files = TRUE
  )

  expect_match(script, "draft survey.csv", fixed = TRUE)
  expect_match(script, "final survey.csv", fixed = TRUE)
  expect_match(script, "expected_data_md5", fixed = TRUE)
  expect_match(script, "100 rows x 12 columns", fixed = TRUE)
  expect_silent(parse(text = script))
})
