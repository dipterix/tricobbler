#' Detect the current operating system
#'
#' @returns character scalar: \code{"darwin"}, \code{"linux"},
#'   \code{"windows"}, \code{"solaris"}, \code{"emscripten"},
#'   or \code{"unknown"}
#' @noRd
get_os <- function() {
  os <- R.version$os
  if (grepl("^darwin", os, ignore.case = TRUE)) {
    return("darwin")
  }
  if (grepl("^linux", os, ignore.case = TRUE)) {
    return("linux")
  }
  if (grepl("^solaris", os, ignore.case = TRUE)) {
    return("solaris")
  }
  if (grepl("^win", os, ignore.case = TRUE)) {
    return("windows")
  }
  if (grepl("^(emscr|wasm)", os, ignore.case = TRUE)) {
    return("emscripten")
  }
  "unknown"
}


#' Count lines in a file efficiently without loading entire file
#' @param path character, file path
#' @param chunk_size integer, bytes to read per chunk (default 5e7)
#' @return integer, number of lines (0 if file doesn't exist or is empty)
#' @noRd
count_lines <- function(path, chunk_size = 5e7) {
  if (!file.exists(path)) {
    return(0L)
  }
  con <- file(path, open = "rb")
  on.exit(close(con))

  LF <- as.raw(10L)
  n_lines <- 0L
  is_empty <- TRUE
  is_last_lf <- FALSE

  repeat {
    bfr <- readBin(con = con, what = raw(), n = chunk_size)
    n <- length(bfr)
    if (n == 0L) break

    is_empty <- FALSE
    n_lines <- n_lines + length(which(bfr == LF))
    is_last_lf <- (bfr[n] == LF)
  }

  # Add 1 if file is non-empty and doesn't end with newline
  if (!is_empty && !is_last_lf) {
    n_lines <- n_lines + 1L
  }

  n_lines
}


concatern <- function(x,
                      collapse = "\n",
                      trim_lines = FALSE,
                      trim_collapsed = TRUE) {
  x <- unlist(x)
  if (trim_lines) {
    x <- trimws(x)
  }
  x <- paste(x, collapse = collapse)
  if (trim_collapsed) {
    x <- trimws(x)
  }
  x
}


save_yaml <- function(x, file, ..., sorted = FALSE) {
  if (inherits(x, "fastmap")) {
    x <- x$as_list(sort = sorted)
  } else if (inherits(x, "fastmap2")) {
    x <- x[["@as_list"]](sort = sorted)
  } else if (inherits(x, c("fastqueue", "fastqueue2"))) {
    x <- x$as_list()
  } else if (sorted) {
    x <- as.list(x, sorted = sorted, ...)
    nms <- names(x)
    if (length(nms) && is.unsorted(nms)) {
      x <- x[order(nms)]
    }
  } else {
    x <- as.list(x, ...)
  }
  yaml::write_yaml(x, file = file, ...)
  if (!inherits(file, "connection")) {
    file <- normalizePath(file)
  }
  invisible(file)
}


package_installed <- function(pkgs, all = FALSE) {
  re <- sapply(pkgs, function(p) {
    system.file("", package = p) != ""
  })
  if (all) {
    re <- all(re)
  }
  re
}

call_pkg_fun <- function(package,
                         f_name,
                         ...,
                         .if_missing = c("error", "warning", "none"),
                         .missing_default = NULL,
                         .call_pkg_function = TRUE) {
  stopifnot(length(package) == 1)

  if (!package_installed(package)) {
    .if_missing <- match.arg(.if_missing)
    switch(.if_missing, "error" = {
      stop("Package ", sQuote(package), " is missing.")
    }, "warning" = {
      warning("Package ", sQuote(package), " is missing.")
    }, {

    })
    return(.missing_default)
  }

  ns <- asNamespace(package)
  fun <- ns[[f_name]]

  if (.call_pkg_function) {
    if (!is.function(fun)) {
      .if_missing <- match.arg(.if_missing)
      switch(.if_missing,
             "error" = {
               stop("Package ",
                    sQuote(package),
                    " does not have function ",
                    sQuote(f_name))
             },
             "warning" = {
               warning("Package ",
                       sQuote(package),
                       " does not have function ",
                       sQuote(f_name))
             },
             {

             })
      return(.missing_default)
    }

    return(fun(...))
  } else {
    return(fun)
  }

}
