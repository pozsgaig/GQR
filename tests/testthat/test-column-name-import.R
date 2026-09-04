test_that("gqr_read converts accented headings to R-compatible names", {
  dat <- data.frame(
    `Questão principal` = 1:3,
    `Região Açores` = 4:6,
    `Questão principal` = 7:9,
    check.names = FALSE
  )

  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)

  utils::write.table(
    dat,
    file = path,
    sep = ",",
    row.names = FALSE,
    col.names = TRUE,
    fileEncoding = "UTF-8"
  )

  out <- gqr_read(path)

  expect_equal(names(out), c("Questao.principal", "Regiao.Acores", "Questao.principal.1"))
  expect_true(all(make.names(names(out), unique = TRUE) == names(out)))
})


test_that("gqr_read accepts common Windows Portuguese CSV encoding", {
  dat <- data.frame(
    `Questão` = 1:2,
    `Preferência` = 3:4,
    `Região` = c("Açores", "Lisboa"),
    check.names = FALSE
  )

  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)

  utils::write.table(
    dat,
    file = path,
    sep = ";",
    row.names = FALSE,
    col.names = TRUE,
    fileEncoding = "CP1252"
  )

  out <- gqr_read(path)

  expect_equal(names(out), c("Questao", "Preferencia", "Regiao"))
  expect_equal(out$Regiao, c("Açores", "Lisboa"))
})


test_that("reproducible scripts replay Data-tab column renames", {
  state <- list(
    package_version = "0.1.2",
    data = list(type = "example", dataset = "dummy_data"),
    analysis_cols = c("Importance", "Q2"),
    covariate_cols = character(0),
    column_renames = data.frame(
      from = "Q1",
      to = "Importance",
      stringsAsFactors = FALSE
    ),
    transform = "none",
    groups = NULL,
    dummy = list(mode = "all", include_empty = TRUE, max_patterns = 1000000L)
  )

  script <- gqr_reproducible_script(
    state,
    include_session_info = FALSE,
    verify_files = FALSE
  )

  expect_match(
    script,
    'names(dat)[match("Q1", names(dat))] <- "Importance"',
    fixed = TRUE
  )
  expect_false(grepl("column_renames <- data.frame", script, fixed = TRUE))
  expect_silent(parse(text = script))
})

test_that("reproducible scripts replay Data-tab factor selections concisely", {
  state <- list(
    package_version = "0.1.2",
    data = list(type = "example", dataset = "dummy_data"),
    analysis_cols = c("Q1", "Q2"),
    covariate_cols = "Numeric_covariate",
    factor_cols = "Numeric_covariate",
    transform = "none",
    groups = NULL,
    dummy = list(mode = "all", include_empty = TRUE, max_patterns = 1000000L)
  )

  script <- gqr_reproducible_script(
    state,
    include_session_info = FALSE,
    verify_files = FALSE
  )

  expect_match(
    script,
    'dat["Numeric_covariate"] <- lapply(dat["Numeric_covariate"], factor)',
    fixed = TRUE
  )
  expect_silent(parse(text = script))
})

