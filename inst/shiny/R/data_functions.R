# Shiny adapters for the package data-preparation API --------------------

gqr_read <- function(path) {
  GQR::gqr_read(path)
}

gqr_read_upload <- function(datapath, original_name) {
  extension <- tools::file_ext(original_name)

  if (!nzchar(extension)) {
    stop("The uploaded file must have a recognised extension.", call. = FALSE)
  }

  readable_path <- tempfile(fileext = paste0(".", extension))
  on.exit(unlink(readable_path), add = TRUE)

  copied <- file.copy(datapath, readable_path, overwrite = TRUE)
  if (!isTRUE(copied)) {
    stop("The uploaded file could not be read.", call. = FALSE)
  }

  GQR::gqr_read(readable_path)
}

gqr_transform_columns <- function(df,
                                cols,
                                method = c(
                                  "standardise",
                                  "normalise",
                                  "relative",
                                  "entropy"
                                )) {
  method <- match.arg(method)

  GQR::gqr_transform_data(
    data = df,
    columns = cols,
    method = method,
    margin = "auto"
  )
}
