#' @description Recalculate all formulas in an Excel workbook using
#'   LibreOffice and scan for Excel errors.
#'
#' @details LibreOffice must be installed. On macOS, install via
#'   \code{brew install --cask libreoffice} or download from
#'   \url{https://www.libreoffice.org}. On Linux, install via your
#'   package manager (e.g. \code{apt install libreoffice}).
#'
#'   The script:
#'   \enumerate{
#'     \item Installs a LibreOffice Basic macro on first run
#'     \item Opens the file headless, recalculates all formulas
#'     \item Saves and closes
#'     \item Scans every cell for Excel errors (#REF!, #DIV/0!, etc.)
#'     \item Returns JSON with error details
#'   }
#'
#' @param file character, path to the .xlsx file.
#' @param timeout character or numeric, timeout in seconds for
#'   LibreOffice (default \code{"30"}).
#'
#' @return JSON string with recalculation results and error scan.
xlsx_recalc <- function(file, timeout = "30") {

  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Package 'openxlsx' is required.")
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required.")
  }

  if (!file.exists(file)) {
    stop("File not found: ", file)
  }

  timeout <- as.integer(timeout)
  abs_path <- normalizePath(file, mustWork = TRUE)

  # --- Step 1: Setup LibreOffice macro ---
  macro_ok <- setup_libreoffice_macro()
  if (!macro_ok) {
    return(jsonlite::toJSON(
      list(error = "Failed to setup LibreOffice macro. Is LibreOffice installed?"),
      auto_unbox = TRUE, pretty = TRUE
    ))
  }

  # --- Step 2: Run recalculation ---
  soffice <- find_soffice()
  if (is.null(soffice)) {
    return(jsonlite::toJSON(
      list(error = "LibreOffice (soffice) not found on PATH or standard locations."),
      auto_unbox = TRUE, pretty = TRUE
    ))
  }

  macro_url <- paste0(
    "vnd.sun.star.script:Standard.Module1.RecalculateAndSave",
    "?language=Basic&location=application"
  )

  cmd <- c(soffice, "--headless", "--norestore", macro_url, abs_path)

  # Add timeout wrapper
  sys_name <- Sys.info()[["sysname"]]
  if (sys_name == "Linux") {
    cmd <- c("timeout", as.character(timeout), cmd)
  } else if (sys_name == "Darwin") {
    gtimeout <- Sys.which("gtimeout")
    if (nzchar(gtimeout)) {
      cmd <- c("gtimeout", as.character(timeout), cmd)
    }
  }

  result <- tryCatch(
    system2(cmd[[1L]], args = cmd[-1L],
            stdout = TRUE, stderr = TRUE, timeout = timeout + 5L),
    error = function(e) conditionMessage(e),
    warning = function(w) conditionMessage(w)
  )

  # --- Step 3: Scan for errors ---
  tryCatch({
    wb <- openxlsx::loadWorkbook(abs_path)
    sheets <- openxlsx::sheets(wb)

    excel_errors <- c(
      "#VALUE!", "#DIV/0!", "#REF!", "#NAME?",
      "#NULL!", "#NUM!", "#N/A"
    )
    error_details <- setNames(
      replicate(length(excel_errors), character(0L), simplify = FALSE),
      excel_errors
    )
    total_errors <- 0L

    for (sheet_name in sheets) {
      df <- openxlsx::readWorkbook(
        wb, sheet = sheet_name,
        colNames = FALSE,
        skipEmptyRows = FALSE,
        skipEmptyCols = FALSE
      )
      if (is.null(df) || nrow(df) == 0L) next

      for (ci in seq_len(ncol(df))) {
        for (ri in seq_len(nrow(df))) {
          val <- df[[ci]][[ri]]
          if (!is.na(val) && is.character(val)) {
            for (err in excel_errors) {
              if (grepl(err, val, fixed = TRUE)) {
                col_letter <- num_to_col_letter_recalc(ci)
                loc <- sprintf("%s!%s%d", sheet_name, col_letter, ri)
                error_details[[err]] <- c(error_details[[err]], loc)
                total_errors <- total_errors + 1L
                break
              }
            }
          }
        }
      }
    }

    # Count formulas
    formula_count <- 0L
    for (sheet_name in sheets) {
      sheet_idx <- match(sheet_name, sheets)
      ws <- wb$worksheets[[sheet_idx]]
      if (!is.null(ws$sheet_data) && !is.null(ws$sheet_data$f)) {
        for (fml in ws$sheet_data$f) {
          if (!is.null(fml) && !is.na(fml) && nzchar(fml)) {
            formula_count <- formula_count + 1L
          }
        }
      }
    }

    # Build result
    out <- list(
      status = if (total_errors == 0L) "success" else "errors_found",
      total_errors = total_errors,
      total_formulas = formula_count
    )

    error_summary <- list()
    for (err_type in excel_errors) {
      locs <- error_details[[err_type]]
      if (length(locs) > 0L) {
        error_summary[[err_type]] <- list(
          count = length(locs),
          locations = head(locs, 20L)
        )
      }
    }
    out$error_summary <- error_summary

    jsonlite::toJSON(out, auto_unbox = TRUE, pretty = TRUE)

  }, error = function(e) {
    jsonlite::toJSON(
      list(error = conditionMessage(e)),
      auto_unbox = TRUE, pretty = TRUE
    )
  })
}


# ---- Internal helpers -----------------------------------------------

RECALCULATE_MACRO <- '<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE script:module PUBLIC "-//OpenOffice.org//DTD OfficeDocument 1.0//EN" "module.dtd">
<script:module xmlns:script="http://openoffice.org/2000/script" script:name="Module1" script:language="StarBasic">
    Sub RecalculateAndSave()
      ThisComponent.calculateAll()
      ThisComponent.store()
      ThisComponent.close(True)
    End Sub
</script:module>'

find_soffice <- function() {
  # Check PATH first
  soffice <- Sys.which("soffice")
  if (nzchar(soffice)) return(soffice)

  # macOS standard locations
  mac_paths <- c(
    "/Applications/LibreOffice.app/Contents/MacOS/soffice",
    "~/Applications/LibreOffice.app/Contents/MacOS/soffice"
  )
  for (p in mac_paths) {
    p <- path.expand(p)
    if (file.exists(p)) return(p)
  }

  # Linux standard locations
  linux_paths <- c(
    "/usr/bin/soffice",
    "/usr/local/bin/soffice"
  )
  for (p in linux_paths) {
    if (file.exists(p)) return(p)
  }

  NULL
}

setup_libreoffice_macro <- function() {
  sys_name <- Sys.info()[["sysname"]]
  macro_dir <- if (sys_name == "Darwin") {
    path.expand(
      "~/Library/Application Support/LibreOffice/4/user/basic/Standard"
    )
  } else {
    path.expand("~/.config/libreoffice/4/user/basic/Standard")
  }
  macro_file <- file.path(macro_dir, "Module1.xba")

  # Check if macro already installed
  if (file.exists(macro_file)) {
    content <- readLines(macro_file, warn = FALSE)
    if (any(grepl("RecalculateAndSave", content, fixed = TRUE))) {
      return(TRUE)
    }
  }

  # Initialize LibreOffice if macro dir doesn't exist
  if (!dir.exists(macro_dir)) {
    soffice <- find_soffice()
    if (is.null(soffice)) return(FALSE)
    tryCatch(
      system2(soffice, args = c("--headless", "--terminate_after_init"),
              stdout = FALSE, stderr = FALSE, timeout = 10L),
      error = function(e) NULL
    )
    dir.create(macro_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # Write macro
  tryCatch({
    writeLines(RECALCULATE_MACRO, macro_file)
    TRUE
  }, error = function(e) FALSE)
}

num_to_col_letter_recalc <- function(col) {
  result <- ""
  while (col > 0L) {
    col <- col - 1L
    result <- paste0(LETTERS[col %% 26L + 1L], result)
    col <- col %/% 26L
  }
  result
}


# CLI entry-point
if (sys.nframe() == 0L) {
  "Recalculate Excel formulas using LibreOffice.

Usage:
  xlsx_recalc.R --file=<path> [--timeout=<secs>]
  xlsx_recalc.R (-h | --help)

Options:
  --file=<path>       Path to the .xlsx file
  --timeout=<secs>    Timeout in seconds [default: 30]
  -h --help           Show this help
" -> doc

  args <- tricobbler::docopt(doc)
  cat(xlsx_recalc(
    file = args$file,
    timeout = args$timeout %||% "30"
  ), "\n")
}
