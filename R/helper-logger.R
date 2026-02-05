log_to_file <- function(..., path, role = "", level = c("INFO", "TRACE", "DEBUG", "WARN", "ERROR", "FATAL"), verbose = c("cli", "base", "none")) {
  level <- match.arg(level)
  # Normalize verbose parameter
  if (is.logical(verbose)) {
    verbose <- if (isTRUE(verbose)) "cli" else "none"
  } else {
    verbose <- match.arg(verbose)
  }

  if (nzchar(role)) {
    prefix <- sprintf("%s %s [%s]: ", level, format(Sys.time(), "%H:%M:%S"), role)
  } else {
    prefix <- sprintf("%s %s: ", level, format(Sys.time(), "%H:%M:%S"))
  }


  str <- paste(c(...), collapse = "")
  str <- gsub("[\b\r]", "", str)
  str <- strsplit(str, "\n")[[1]]
  str <- paste0(prefix, str, collapse = "\n")

  # write to file with UTF-8 encoding
  if (file.exists(path)) {
    con <- file(path, open = "a", encoding = "UTF-8")
    on.exit(close(con), add = TRUE)
    cat(str, sep = "\n", file = con)
  } else {
    writeLines(str, path)
  }

  # Console output based on verbose setting
  if (verbose == "none") {
    return(invisible(NULL))
  }

  str_wrapped <- strwrap(
    strsplit(str, "\n")[[1]],
    width = getOption("width") - 1L,
    exdent = 4L
  )
  str_wrapped <- paste(str_wrapped, collapse = "\n")

  # Use cli if requested, available, and console supports ANSI colors
  use_cli <- verbose == "cli" &&
    package_installed("cli") &&
    call_pkg_fun(
      "cli",
      "num_ansi_colors",
      .if_missing = "none",
      .missing_default = 1L
    ) > 1L

  if (use_cli) {
    # Use cli for colored output
    cli_style <- switch(
      level,
      "TRACE" = "col_grey",
      "DEBUG" = "col_silver",
      "INFO" = "col_blue",
      "WARN" = "col_yellow",
      "ERROR" = "col_red",
      "FATAL" = "col_magenta",
      NULL
    )
    if (!is.null(cli_style)) {
      styled_str <- call_pkg_fun(
        "cli",
        cli_style,
        str_wrapped,
        .if_missing = "none",
        .missing_default = str_wrapped
      )
    } else {
      styled_str <- str_wrapped
    }
    if (level %in% c("WARN", "ERROR", "FATAL")) {
      message(styled_str)
    } else {
      cat(styled_str, sep = "\n")
    }
  } else {
    # Base: use cat/message without styling
    if (level %in% c("WARN", "ERROR", "FATAL")) {
      message(str_wrapped)
    } else {
      cat(str_wrapped, sep = "\n")
    }
  }
  return(invisible(NULL))
}
