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

# Convert user-entered column names and grouping-file variable names to the
# same deterministic R-compatible convention used by GQR::gqr_read().
gqr_compatible_names <- function(x, unique = TRUE) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- trimws(enc2utf8(x))

  from_chars <- c(
    "á", "à", "â", "ã", "ä", "Á", "À", "Â", "Ã", "Ä",
    "ç", "Ç",
    "é", "è", "ê", "ë", "É", "È", "Ê", "Ë",
    "í", "ì", "î", "ï", "Í", "Ì", "Î", "Ï",
    "ñ", "Ñ",
    "ó", "ò", "ô", "õ", "ö", "Ó", "Ò", "Ô", "Õ", "Ö",
    "ú", "ù", "û", "ü", "Ú", "Ù", "Û", "Ü",
    "ý", "ÿ", "Ý"
  )
  to_chars <- c(
    "a", "a", "a", "a", "a", "A", "A", "A", "A", "A",
    "c", "C",
    "e", "e", "e", "e", "E", "E", "E", "E",
    "i", "i", "i", "i", "I", "I", "I", "I",
    "n", "N",
    "o", "o", "o", "o", "o", "O", "O", "O", "O", "O",
    "u", "u", "u", "u", "U", "U", "U", "U",
    "y", "y", "Y"
  )
  for (i in seq_along(from_chars)) {
    x <- gsub(from_chars[[i]], to_chars[[i]], x, fixed = TRUE)
  }

  ascii <- suppressWarnings(iconv(x, from = "", to = "ASCII//TRANSLIT", sub = ""))
  bad <- is.na(ascii)
  if (any(bad)) {
    ascii[bad] <- iconv(x[bad], from = "", to = "ASCII", sub = "_")
  }

  ascii <- gsub("[^A-Za-z0-9._]+", ".", ascii)
  ascii <- gsub("[.]+", ".", ascii)
  ascii <- sub("^[.]+", "", ascii)
  ascii <- sub("[.]+$", "", ascii)
  ascii[!nzchar(ascii)] <- "X"

  make.names(ascii, unique = unique, allow_ = TRUE)
}
