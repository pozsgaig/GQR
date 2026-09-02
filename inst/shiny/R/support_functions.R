# Helpers used only by the GQR Shiny interface


#  full name if the string is 10 characters or fewer, abbreviated otherwise
abbreviate_dummy_names <- function(x, max_n = 10) {
  stopifnot(is.character(x), length(max_n) == 1L, max_n >= 4)

  vapply(
    x,
    function(s) {
      if (is.na(s) || nchar(s) <= max_n) {
        s
      } else {
        paste0(substr(s, 1, max_n - 1), "…")
      }
    },
    character(1)
  )
}

# Return a qualitative palette of visually distinct colours, expanding or
# re-sampling from Polychrome-style many-group palettes as needed for the
# requested number of groups.
qual_pal <- function(n, seed = NULL, dark = FALSE) {
  if (!is.null(seed)) set.seed(seed)

  base_pal <- if (n <= 24) {
    if (dark) Polychrome::dark.colors(24) else Polychrome::light.colors(24)
  } else if (n <= 26) {
    Polychrome::alphabet.colors(26)
  } else if (n <= 32) {
    Polychrome::glasbey.colors(32)
  } else if (n <= 36) {
    Polychrome::palette36.colors(36)
  } else {
    Polychrome::createPalette(
      N = n,
      seedcolors = c("#5A5156", "#E4E1E3", "#F6222E"),
      range = c(30, 90)
    )
  }

  pal <- sample(base_pal, size = n, replace = FALSE)
  unname(pal)
}



`%||%` <- function(x, y) if (is.null(x)) y else x

# Start a heavy GQR computation in a separate R process. This keeps the Shiny
# session responsive and allows the user to stop the calculation without
# terminating the main R session.
gqr_app_start_background <- function(task, args, status_file = tempfile("gqr-progress-", fileext = ".rds")) {
  stopifnot(task %in% c("dummies", "pca_design", "pca_matrix"))

  if (file.exists(status_file)) unlink(status_file)

  process <- callr::r_bg(
    func = function(task, args, status_file) {
      progress <- function(value, message = NULL) {
        saveRDS(
          list(
            value = max(0, min(1, as.numeric(value)[1L])),
            message = if (is.null(message)) "Working..." else as.character(message),
            time = Sys.time()
          ),
          status_file
        )
        invisible(NULL)
      }

      progress(0, "Starting")

      if (identical(task, "dummies")) {
        args$progress <- progress
        result <- do.call(GQR::gqr_generate_dummies, args)
        progress(1, "Dummy design ready")
        return(result)
      }

      if (identical(task, "pca_design")) {
        args$progress <- progress
        result <- do.call(GQR::gqr_pca_design, args)
        progress(1, "PCA ready")
        return(result)
      }

      # Full-W fallback used for SPSS/correlation mode. The process can still
      # be killed from the Shiny session while gqr_pca() itself is running.
      data <- args$data
      D <- args$D
      pca_args <- args$pca_args
      na_action <- if (is.null(args$na_action)) "error" else args$na_action

      W <- GQR::gqr_make_w(
        data = data,
        D = D,
        na_action = na_action,
        algorithm = "chunked",
        progress = function(value, message = NULL) {
          progress(0.55 * value, message)
        }
      )

      progress(0.60, "Running correlation-based PCA")
      result <- do.call(
        GQR::gqr_pca,
        c(list(W = W), pca_args)
      )
      progress(1, "PCA ready")
      result
    },
    args = list(task = task, args = args, status_file = status_file),
    supervise = TRUE
  )

  list(process = process, status_file = status_file)
}


gqr_app_read_progress <- function(status_file) {
  if (is.null(status_file) || !file.exists(status_file)) {
    return(list(value = 0, message = "Starting..."))
  }

  tryCatch(
    readRDS(status_file),
    error = function(e) list(value = 0, message = "Working...")
  )
}


gqr_app_stop_background <- function(task) {
  if (is.null(task) || is.null(task$process)) return(invisible(FALSE))

  process <- task$process
  if (isTRUE(process$is_alive())) {
    try(process$kill(), silent = TRUE)
  }
  if (!is.null(task$status_file) && file.exists(task$status_file)) {
    unlink(task$status_file)
  }
  invisible(TRUE)
}
