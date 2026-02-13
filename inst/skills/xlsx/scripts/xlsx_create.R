#' @description Create a new Excel workbook with tabular data and optional
#'   formulas, then save to disk.
#'
#' @param file character, path where the workbook will be saved
#'   (e.g. \code{"/tmp/demo.xlsx"}).
#' @param data_json character, a JSON string representing an array of
#'   row-objects.
#'   Example: \code{'[{"Item":"A","Value":10},{"Item":"B","Value":20}]'}
#' @param sheet character, name of the worksheet (default \code{"Sheet1"}).
#' @param formulas_json character, a JSON array of formula specs. Each
#'   element must have \code{cell} (e.g. \code{"B4"}) and \code{formula}
#'   (e.g. \code{"SUM(B2:B3)"}).  Leading \code{=} is stripped
#'   automatically.  Pass \code{"{}"} or empty string to skip.
#' @param header logical string, whether to write column names as the
#'   first row (\code{"true"} or \code{"false"}, default \code{"true"}).
#'
#' @return A message confirming the file was created, plus a preview of
#'   the first rows.
xlsx_create <- function(file,
                        data_json = "[]",
                        sheet = "Sheet1",
                        formulas_json = "{}",
                        header = "true") {

  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Package 'openxlsx' is required. Install with: ",
         "install.packages('openxlsx')")
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required.")
  }

  # Parse data -- handle single-quote JSON and array-of-arrays format
  data <- tryCatch(
    jsonlite::fromJSON(data_json, simplifyDataFrame = TRUE),
    error = function(e) {
      # Retry with single-quote fix (Python-style JSON)
      fixed <- gsub("'", '"', data_json)
      tryCatch(
        jsonlite::fromJSON(fixed, simplifyDataFrame = TRUE),
        error = function(e2) {
          stop(
            "Failed to parse data_json as JSON.\n",
            "Received: ", substr(data_json, 1, 200), "\n",
            "Error: ", conditionMessage(e), "\n\n",
            "data_json must be a JSON array of row-objects, e.g.:\n",
            '  [{"Item":"Apples","Price":3},',
            '{"Item":"Bananas","Price":2}]',
            call. = FALSE
          )
        }
      )
    }
  )
  # Convert matrix (from array-of-arrays) to data.frame
  if (is.matrix(data)) {
    data <- as.data.frame(data, stringsAsFactors = FALSE)
  }
  if (!is.data.frame(data) && is.list(data) && length(data) == 0L) {
    data <- data.frame()
  }

  use_header <- tolower(as.character(header)) %in% c("true", "1", "yes")

  # Create workbook

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, sheet)

  if (nrow(data) > 0L) {
    openxlsx::writeData(
      wb, sheet, data,
      startCol = 1, startRow = 1,
      colNames = use_header
    )

    # Detect formula strings (starting with "=") in data cells and
    # overwrite them with writeFormula so Excel evaluates them.
    header_offset <- if (use_header) 1L else 0L
    inline_formula_count <- 0L
    for (ci in seq_len(ncol(data))) {
      col_vals <- data[[ci]]
      if (is.character(col_vals)) {
        for (ri in seq_along(col_vals)) {
          v <- col_vals[[ri]]
          if (!is.na(v) && grepl("^=", v)) {
            fml_text <- sub("^=", "", v)
            openxlsx::writeFormula(
              wb, sheet,
              x = fml_text,
              startCol = ci,
              startRow = ri + header_offset
            )
            inline_formula_count <- inline_formula_count + 1L
          }
        }
      }
    }

    # Bold header
    if (use_header) {
      bold <- openxlsx::createStyle(textDecoration = "bold")
      openxlsx::addStyle(
        wb, sheet, style = bold,
        rows = 1, cols = seq_len(ncol(data)),
        gridExpand = TRUE
      )
    }
  }

  # Parse and write formulas
  if (!is.null(formulas_json) && nzchar(formulas_json) &&
      !identical(formulas_json, "{}") &&
      !identical(formulas_json, "[]")) {
    formulas <- tryCatch(
      jsonlite::fromJSON(formulas_json, simplifyDataFrame = TRUE),
      error = function(e) {
        # Retry with single-quote fix
        fixed <- gsub("'", '"', formulas_json)
        tryCatch(
          jsonlite::fromJSON(fixed, simplifyDataFrame = TRUE),
          error = function(e2) {
            stop(
              "Failed to parse formulas_json as JSON.\n",
              "Received: ", substr(formulas_json, 1, 200), "\n",
              "Error: ", conditionMessage(e), "\n\n",
              "formulas_json must be a JSON object like:\n",
              '  {"B5":"SUM(B2:B4)","C5":"AVERAGE(C2:C4)"}',
              call. = FALSE
            )
          }
        )
      }
    )

    # Handle dict-style input: {"B5": "SUM(B2:B4)", ...}
    # Convert to list-of-lists with $cell and $formula
    if (is.list(formulas) && !is.data.frame(formulas) &&
        length(formulas) > 0L && is.null(formulas$cell) &&
        all(grepl("^[A-Za-z]+\\d+$", names(formulas)))) {
      formulas <- lapply(names(formulas), function(nm) {
        list(cell = nm, formula = as.character(formulas[[nm]]))
      })
    }

    if (is.data.frame(formulas)) {
      formulas <- lapply(seq_len(nrow(formulas)), function(i) {
        as.list(formulas[i, ])
      })
    }
    if (!is.list(formulas)) formulas <- list()
    if (length(formulas) > 0L && !is.null(formulas$cell)) {
      formulas <- list(formulas)
    }

    for (spec in formulas) {
      cell_ref <- as.character(spec$cell)
      formula_text <- as.character(spec$formula)
      # Strip leading '=' if present
      formula_text <- sub("^=", "", formula_text)

      # Parse cell reference (e.g. "B4" -> col=2, row=4)
      parsed <- parse_cell_ref(cell_ref)

      openxlsx::writeFormula(
        wb, sheet,
        x = formula_text,
        startCol = parsed$col,
        startRow = parsed$row
      )
    }
  }

  # Save
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)

  # Build summary
  n_formulas <- 0L
  if (exists("formulas") && is.list(formulas)) {
    n_formulas <- length(formulas)
  }
  if (exists("inline_formula_count")) {
    n_formulas <- n_formulas + inline_formula_count
  }

  msg <- sprintf(
    "Created workbook '%s' with sheet '%s': %d rows x %d cols, %d formula(s).",
    basename(file), sheet,
    nrow(data), ncol(data), n_formulas
  )

  # Return preview
  preview <- if (nrow(data) > 0L) {
    utils::capture.output(print(utils::head(data, 6)))
  } else {
    "(empty data)"
  }

  paste(c(msg, "", "Preview:", preview), collapse = "\n")
}


# -- Helper: parse cell reference like "B4" -> list(col=2, row=4) ---
parse_cell_ref <- function(ref) {
  ref <- toupper(trimws(ref))
  m <- regmatches(ref, regexec("^([A-Z]+)(\\d+)$", ref))[[1L]]
  if (length(m) != 3L) {
    stop("Invalid cell reference: '", ref, "'")
  }
  col_letters <- m[[2L]]
  row_num <- as.integer(m[[3L]])

  # Convert column letters to number (A=1, ..., Z=26, AA=27, ...)
  chars <- strsplit(col_letters, "")[[1L]]
  col_num <- 0L
  for (ch in chars) {
    col_num <- col_num * 26L + match(ch, LETTERS)
  }

  list(col = col_num, row = row_num)
}


# CLI entry-point
if (sys.nframe() == 0L) {
  "Create an Excel workbook with data and formulas.

Usage:
  xlsx_create.R --file=<path> [--data_json=<json>] [--sheet=<name>] [--formulas_json=<json>] [--header=<bool>]
  xlsx_create.R (-h | --help)

Options:
  --file=<path>           Output file path
  --data_json=<json>      JSON array of row objects [default: []]
  --sheet=<name>          Sheet name [default: Sheet1]
  --formulas_json=<json>  JSON array of formula specs [default: {}]
  --header=<bool>         Include header row [default: true]
  -h --help               Show this help
" -> doc

  args <- tricobbler::docopt(doc)
  cat(xlsx_create(
    file = args$file,
    data_json = args$data_json %||% "[]",
    sheet = args$sheet %||% "Sheet1",
    formulas_json = args$formulas_json %||% "{}",
    header = args$header %||% "true"
  ), "\n")
}
