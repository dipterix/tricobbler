#' @description Evaluate Excel formulas stored in a workbook using R.
#'
#' @details
#' \code{openxlsx} writes formulas as strings but does not compute
#' cached values.  Excel or LibreOffice normally recalculates on open.
#' This script provides an R-native evaluation of common formulas
#' so you can inspect computed values without opening Excel.
#'
#' Supported functions: \code{SUM}, \code{AVERAGE}, \code{MIN},
#' \code{MAX}, \code{COUNT}, \code{PRODUCT}, \code{MEDIAN},
#' \code{RANDBETWEEN}, \code{RAND}, \code{INT}, \code{ROUND},
#' \code{ABS}, \code{SQRT}, \code{LOG}, \code{LN}, \code{LOG10},
#' \code{EXP}, \code{MOD}, \code{POWER}, \code{CEILING},
#' \code{FLOOR}, \code{SIGN}, \code{PI}, and
#' simple arithmetic on cell references (e.g. \code{(C4-C2)/C2}).
#'
#' @param file character, path to the workbook.
#' @param cells character, comma-separated cell references to evaluate
#'   (e.g. \code{"B4,B5"}).  If \code{"all"} or empty, all formula
#'   cells on the sheet are evaluated.
#' @param sheet character, sheet name (default \code{"Sheet1"}).
#'
#' @return JSON-formatted evaluation results.
xlsx_eval <- function(file, cells = "all", sheet = "Sheet1") {

  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Package 'openxlsx' is required.")
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required.")
  }

  if (!file.exists(file)) {
    stop("File not found: ", file)
  }

  # Load workbook and read raw sheet (including formula text)
  wb <- openxlsx::loadWorkbook(file)
  sheets <- openxlsx::sheets(wb)
  if (!sheet %in% sheets) {
    stop(sprintf("Sheet '%s' not found. Available: %s",
                 sheet, paste(sheets, collapse = ", ")))
  }

  # Read the data values (formulas appear as NA here)
  df_values <- openxlsx::readWorkbook(
    wb, sheet = sheet,
    colNames = FALSE,
    skipEmptyRows = FALSE,
    skipEmptyCols = FALSE
  )

  if (is.null(df_values) || nrow(df_values) == 0L) {
    return('{"status":"empty","message":"Sheet is empty."}')
  }

  # We also need the raw XML to find formula text.
  # openxlsx stores sheet data internally; extract formula cells.
  # Use the sheet_data slot from the loaded workbook.
  sheet_idx <- match(sheet, sheets)
  ws <- wb$worksheets[[sheet_idx]]

  # Build a map of cell -> formula from the worksheet XML data
  formula_map <- list()
  if (!is.null(ws$sheet_data)) {
    sd <- ws$sheet_data
    # sheet_data is a reference-class with rows, cols, v, f, etc.
    if (!is.null(sd$f) && length(sd$f) > 0L) {
      # sd$rows, sd$cols are integer vectors; sd$f is character
      for (i in seq_along(sd$f)) {
        fml <- sd$f[[i]]
        if (!is.null(fml) && nzchar(fml) && !is.na(fml)) {
          r <- sd$rows[[i]]
          c <- sd$cols[[i]]
          cell_id <- paste0(num_to_col_letter(c), r)
          # Strip XML <f>...</f> tags if present
          fml <- sub("^<f[^>]*>", "", fml)
          fml <- sub("</f>$", "", fml)
          formula_map[[cell_id]] <- fml
        }
      }
    }
  }

  # If no formulas found via internal API, fall back to a simpler approach:
  # read file as a zip and parse xl/worksheets/sheet<N>.xml
  if (length(formula_map) == 0L) {
    formula_map <- extract_formulas_from_xml(file, sheet_idx)
  }

  # Determine which cells to evaluate
  # Accept: NULL, "all", "B5,B6", or character vector c("B5","B6")
  if (is.null(cells) || identical(cells, "") ||
      (length(cells) == 1L && tolower(trimws(cells)) == "all")) {
    target_cells <- names(formula_map)
  } else {
    # Collapse vector/list to single comma-separated string, then split
    cells_str <- paste(cells, collapse = ",")
    target_cells <- trimws(strsplit(toupper(cells_str), ",")[[1L]])
  }

  if (length(target_cells) == 0L) {
    return('{"status":"no_formulas","message":"No formula cells found."}')
  }

  # Evaluate each formula
  results <- list()
  for (cell_id in target_cells) {
    fml <- formula_map[[cell_id]]
    if (is.null(fml)) {
      # Not a formula cell -- read the value directly
      parsed <- parse_cell_ref_eval(cell_id)
      val <- safe_get_value(df_values, parsed$row, parsed$col)
      results[[cell_id]] <- list(
        cell = cell_id, formula = NA_character_,
        value = val, type = "data"
      )
    } else {
      val <- tryCatch(
        eval_formula(fml, df_values, formula_map),
        error = function(e) {
          paste0("#EVAL_ERROR [formula: ", fml, "]: ",
                 conditionMessage(e))
        }
      )
      results[[cell_id]] <- list(
        cell = cell_id, formula = fml,
        value = val, type = "formula"
      )
    }
  }

  # Format output
  out <- list(
    status = "success",
    total_formulas = length(formula_map),
    evaluated = length(target_cells),
    results = unname(results)
  )

  jsonlite::toJSON(out, auto_unbox = TRUE, pretty = TRUE, na = "null")
}


# ---- Internal helpers -----------------------------------------------

num_to_col_letter <- function(col) {
  result <- ""
  while (col > 0L) {
    col <- col - 1L
    result <- paste0(LETTERS[col %% 26L + 1L], result)
    col <- col %/% 26L
  }
  result
}

parse_cell_ref_eval <- function(ref) {
  ref <- toupper(trimws(ref))
  m <- regmatches(ref, regexec("^([A-Z]+)(\\d+)$", ref))[[1L]]
  if (length(m) != 3L) stop("Invalid cell reference: '", ref, "'")
  chars <- strsplit(m[[2L]], "")[[1L]]
  col_num <- 0L
  for (ch in chars) col_num <- col_num * 26L + match(ch, LETTERS)
  list(col = col_num, row = as.integer(m[[3L]]))
}

safe_get_value <- function(df, row, col) {
  if (row < 1L || row > nrow(df) || col < 1L || col > ncol(df)) {
    return(NA_real_)
  }
  val <- df[row, col]
  if (is.na(val)) return(NA_real_)
  num <- suppressWarnings(as.numeric(val))
  if (!is.na(num)) return(num)
  val
}

# Resolve a range like "B2:B5" into a numeric vector of values
# formula_map is used for recursive evaluation of formula cells
resolve_range <- function(range_str, df, formula_map = list()) {
  parts <- regmatches(
    range_str,
    regexec("^([A-Z]+)(\\d+):([A-Z]+)(\\d+)$", toupper(range_str))
  )[[1L]]
  if (length(parts) != 5L) {
    stop("Cannot parse cell range: '", range_str, "'. ",
         "Expected A1-style range like 'B2:B5' or 'A1:C10'. ",
         "Range must be COLUMN_LETTERS + ROW_NUMBER : ",
         "COLUMN_LETTERS + ROW_NUMBER.")
  }
  c1 <- parse_cell_ref_eval(paste0(parts[[2L]], parts[[3L]]))
  c2 <- parse_cell_ref_eval(paste0(parts[[4L]], parts[[5L]]))
  vals <- c()
  for (r in seq(c1$row, c2$row)) {
    for (cc in seq(c1$col, c2$col)) {
      cell_id <- paste0(num_to_col_letter(cc), r)
      vals <- c(vals, resolve_cell_value(cell_id, df, formula_map))
    }
  }
  vals
}

# Resolve a single cell to its value, recursively evaluating formulas
resolve_cell_value <- function(cell_str, df, formula_map = list()) {
  cell_str <- toupper(trimws(cell_str))
  fml <- formula_map[[cell_str]]
  if (!is.null(fml) && nzchar(fml)) {
    return(eval_formula(fml, df, formula_map))
  }
  p <- parse_cell_ref_eval(cell_str)
  safe_get_value(df, p$row, p$col)
}

# Resolve a single cell reference to a number (no recursive eval)
resolve_cell <- function(cell_str, df) {
  p <- parse_cell_ref_eval(cell_str)
  safe_get_value(df, p$row, p$col)
}

# Evaluate a formula string against the data frame
# formula_map enables recursive evaluation of dependent formula cells
eval_formula <- function(fml, df, formula_map = list()) {
  fml <- trimws(fml)
  fml <- sub("^=", "", fml)

  # Try to match function-style: FUNC(args)
  fn_match <- regmatches(
    fml,
    regexec("^(SUM|AVERAGE|AVG|MIN|MAX|COUNT|PRODUCT|MEDIAN|RANDBETWEEN|RAND)\\((.*)\\)$",
            toupper(fml))
  )[[1L]]

  if (length(fn_match) == 3L) {
    func_name <- fn_match[[2L]]
    arg_str <- fn_match[[3L]]

    # Handle RANDBETWEEN specially
    if (func_name == "RANDBETWEEN") {
      bounds <- trimws(strsplit(arg_str, ",")[[1L]])
      lo <- as.integer(bounds[[1L]])
      hi <- as.integer(bounds[[2L]])
      return(sample(lo:hi, 1L))
    }
    # Handle RAND() - no arguments
    if (func_name == "RAND") {
      return(stats::runif(1L))
    }

    # Collect values from all arguments (comma-separated ranges/cells)
    args_parts <- trimws(strsplit(arg_str, ",")[[1L]])
    all_vals <- c()
    for (part in args_parts) {
      if (grepl(":", part)) {
        all_vals <- c(all_vals, resolve_range(part, df, formula_map))
      } else if (grepl("^[A-Z]+\\d+$", toupper(part))) {
        all_vals <- c(all_vals, resolve_cell_value(part, df, formula_map))
      } else {
        # Try as numeric literal
        all_vals <- c(all_vals, as.numeric(part))
      }
    }
    all_vals <- all_vals[!is.na(all_vals)]

    result <- switch(
      func_name,
      "SUM"     = sum(all_vals),
      "AVERAGE" = , "AVG" = mean(all_vals),
      "MIN"     = min(all_vals),
      "MAX"     = max(all_vals),
      "COUNT"   = length(all_vals),
      "PRODUCT" = prod(all_vals),
      "MEDIAN"  = stats::median(all_vals),
      stop("Unsupported Excel function: ", func_name, "(). ",
           "xlsx_eval supports these as standalone: SUM, AVERAGE, ",
           "MIN, MAX, COUNT, PRODUCT, MEDIAN, RANDBETWEEN, RAND. ",
           "It also supports these in arithmetic expressions: ",
           "IF, ROUND, ABS, SQRT, LOG, LN, LOG10, EXP, INT, MOD, ",
           "POWER, CEILING, FLOOR, SIGN, PI. ",
           "For unsupported functions (VLOOKUP, INDEX/MATCH, ",
           "CONCATENATE, etc.), use xlsx_recalc (requires ",
           "LibreOffice) or rewrite the formula using supported ",
           "functions.")
    )
    return(result)
  }

  # For simple arithmetic expressions with cell references,
  # substitute cell values and eval
  expr_str <- toupper(fml)

  # Replace Excel functions embedded in arithmetic expressions:

  # RANDBETWEEN(lo,hi) -> sample(lo:hi, 1L)
  while (grepl("RANDBETWEEN\\(", expr_str)) {
    expr_str <- sub(
      "RANDBETWEEN\\(\\s*([^,]+)\\s*,\\s*([^)]+)\\s*\\)",
      "sample(\\1:\\2, 1L)",
      expr_str
    )
  }

  # RAND() -> runif(1)
  expr_str <- gsub("RAND\\(\\)", "runif(1)", expr_str)

  # INT(x) -> as.integer(x)  (Excel INT truncates toward zero)
  while (grepl("INT\\(", expr_str)) {
    expr_str <- sub("INT\\(", "as.integer(", expr_str)
  }

  # Map common Excel functions to R equivalents (case-insensitive)
  # These are safe 1:1 mappings
  expr_str <- gsub("ROUND\\(", "round(", expr_str)
  expr_str <- gsub("ROUNDUP\\(", "ceiling(", expr_str)
  expr_str <- gsub("ROUNDDOWN\\(", "floor(", expr_str)
  expr_str <- gsub("ABS\\(", "abs(", expr_str)
  expr_str <- gsub("SQRT\\(", "sqrt(", expr_str)
  expr_str <- gsub("LOG10\\(", "log10(", expr_str)
  expr_str <- gsub("LOG\\(", "log(", expr_str)
  expr_str <- gsub("LN\\(", "log(", expr_str)
  expr_str <- gsub("EXP\\(", "exp(", expr_str)
  expr_str <- gsub("MOD\\(", "%%_mod(", expr_str)  # handled below
  expr_str <- gsub("POWER\\(", "^_pow(", expr_str)  # handled below
  expr_str <- gsub("CEILING\\(", "ceiling(", expr_str)
  expr_str <- gsub("FLOOR\\(", "floor(", expr_str)
  expr_str <- gsub("SIGN\\(", "sign(", expr_str)
  expr_str <- gsub("PI\\(\\)", "pi", expr_str)
  expr_str <- gsub("TRUE", "TRUE", expr_str)
  expr_str <- gsub("FALSE", "FALSE", expr_str)

  # IF(cond, true_val, false_val) -> ifelse(cond, true_val, false_val)
  expr_str <- gsub("IF\\(", "ifelse(", expr_str)

  # Handle MOD(a,b) -> (a %% b) and POWER(a,b) -> (a ^ b)
  # These were tagged above; now convert to R syntax
  while (grepl("%%_mod\\(", expr_str)) {
    expr_str <- sub(
      "%%_mod\\(\\s*([^,]+)\\s*,\\s*([^)]+)\\s*\\)",
      "((\\1) %% (\\2))",
      expr_str
    )
  }
  while (grepl("\\^_pow\\(", expr_str)) {
    expr_str <- sub(
      "\\^_pow\\(\\s*([^,]+)\\s*,\\s*([^)]+)\\s*\\)",
      "((\\1) ^ (\\2))",
      expr_str
    )
  }

  # Replace cell references with their numeric values
  cell_refs <- regmatches(expr_str, gregexpr("[A-Z]+\\d+", expr_str))[[1L]]
  cell_refs <- unique(cell_refs)
  for (cr in cell_refs) {
    val <- resolve_cell_value(cr, df, formula_map)
    if (is.na(val)) val <- 0
    expr_str <- gsub(cr, as.character(val), expr_str, fixed = TRUE)
  }

  # Evaluate the arithmetic expression safely
  result <- tryCatch(
    eval(parse(text = expr_str)),
    error = function(e) {
      stop("Cannot evaluate formula '", fml, "' ",
           "(R expression: '", expr_str, "'). ",
           "Error: ", conditionMessage(e), ". ",
           "xlsx_eval supports these Excel functions: ",
           "SUM, AVERAGE, MIN, MAX, COUNT, PRODUCT, MEDIAN, ",
           "RANDBETWEEN(lo,hi), RAND(), INT(x), IF(cond,a,b), ",
           "ROUND(x,n), ",
           "ABS(x), SQRT(x), LOG(x), LN(x), LOG10(x), EXP(x), ",
           "MOD(a,b), POWER(a,b), CEILING(x), FLOOR(x), ",
           "SIGN(x), PI(), plus arithmetic (+, -, *, /, ^) and ",
           "cell references (e.g. B2, C4). ",
           "Unsupported functions like VLOOKUP, CONCATENATE, ",
           "INDEX/MATCH, etc. will fail. ",
           "Prefer RANDBETWEEN(1,5) over RAND()*4+1 for random ",
           "integers. For complex formulas, use xlsx_recalc ",
           "(requires LibreOffice).")
    }
  )
  result
}


# Extract formulas from the raw XML inside the xlsx zip
extract_formulas_from_xml <- function(file, sheet_idx) {
  formula_map <- list()
  tryCatch({
    tmp <- tempfile()
    on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
    utils::unzip(file, exdir = tmp)

    sheet_file <- file.path(
      tmp, "xl", "worksheets",
      paste0("sheet", sheet_idx, ".xml")
    )
    if (!file.exists(sheet_file)) return(formula_map)

    xml_lines <- readLines(sheet_file, warn = FALSE)
    xml_text <- paste(xml_lines, collapse = "")

    # Find <c r="B4" ...><f>SUM(B2:B3)</f>...</c> patterns
    # Use a two-step approach: find all <c> elements, then extract <f>
    # Pattern accounts for optional namespace prefixes
    c_pattern <- '<c\\s+r="([A-Z]+\\d+)"[^>]*>(.*?)</c>'
    c_matches <- gregexpr(c_pattern, xml_text, perl = TRUE)
    c_strs <- regmatches(xml_text, c_matches)[[1L]]

    for (c_str in c_strs) {
      c_inner <- regmatches(
        c_str,
        regexec('<c\\s+r="([A-Z]+\\d+)"[^>]*>(.*?)</c>',
                c_str, perl = TRUE)
      )[[1L]]
      if (length(c_inner) == 3L) {
        cell_id <- c_inner[[2L]]
        body <- c_inner[[3L]]
        # Extract formula from <f>...</f> or <f ...>...</f>
        f_match <- regmatches(
          body,
          regexec('<f[^>]*>([^<]+)</f>', body, perl = TRUE)
        )[[1L]]
        if (length(f_match) == 2L) {
          formula_map[[cell_id]] <- f_match[[2L]]
        }
      }
    }
  }, error = function(e) {
    # Silently fail -- no formulas found
  })

  formula_map
}


# CLI entry-point
if (sys.nframe() == 0L) {
  "Evaluate formulas in an Excel workbook using R.

Usage:
  xlsx_eval.R --file=<path> [--cells=<refs>] [--sheet=<name>]
  xlsx_eval.R (-h | --help)

Options:
  --file=<path>     Path to the .xlsx file
  --cells=<refs>    Comma-separated cell refs or 'all' [default: all]
  --sheet=<name>    Sheet name [default: Sheet1]
  -h --help         Show this help
" -> doc

  args <- tricobbler::docopt(doc)
  cat(xlsx_eval(
    file = args$file,
    cells = args$cells %||% "all",
    sheet = args$sheet %||% "Sheet1"
  ), "\n")
}
