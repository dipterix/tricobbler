#' @description Update specific cell values, formulas, or styles in an
#'   existing Excel workbook.
#'
#' @param file character, path to the existing workbook.
#' @param updates_json character, a JSON array of update specs, or a
#'   dict-style JSON object with cell references as keys.
#'   Each element should have:
#'   \itemize{
#'     \item \code{cell}: A1-style reference (e.g. \code{"B2"})
#'     \item \code{value}: (optional) the new value (number, string,
#'       or logical).  Use \code{null} to clear a cell.
#'     \item \code{formula}: (optional) a formula string instead of a
#'       value (e.g. \code{"SUM(B2:B5)"}).  Do not include leading
#'       \code{=}.
#'     \item \code{style}: (optional) a JSON object with style
#'       properties.  Supported keys:
#'       \itemize{
#'         \item \code{fgFill} or \code{bgColor} or
#'           \code{backgroundColor} or \code{fill}: background
#'           fill color (hex with \code{#}, e.g. \code{"#00FF00"})
#'         \item \code{fontColour} or \code{fontColor} or
#'           \code{color}: font color (hex with \code{#})
#'         \item \code{textDecoration}: \code{"bold"},
#'           \code{"italic"}, etc.
#'         \item \code{bold}: \code{true} to make bold
#'         \item \code{fontSize}: number
#'         \item \code{numFmt}: Excel number format string
#'         \item \code{fontName}: font family name
#'         \item \code{halign}, \code{valign}: alignment
#'         \item \code{wrapText}: \code{true}/\code{false}
#'         \item \code{border}, \code{borderColour},
#'           \code{borderStyle}: border properties
#'       }
#'   }
#'   Array format example:
#'   \code{'[{"cell":"B2","value":99},{"cell":"B5","style":{"fgFill":"#00FF00"}}]'}
#'   Dict format example:
#'   \code{'{"B2":10,"B3":20}'}
#' @param sheet character, sheet name (default \code{"Sheet1"}).
#'
#' @return A summary of applied updates.
xlsx_update <- function(file,
                        updates_json = "[]",
                        sheet = "Sheet1") {

  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Package 'openxlsx' is required. Install with: ",
         "install.packages('openxlsx')")
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required.")
  }

  if (!file.exists(file)) {
    stop("File not found: ", file)
  }

  wb <- openxlsx::loadWorkbook(file)
  sheets <- openxlsx::sheets(wb)
  if (!sheet %in% sheets) {
    stop(sprintf(
      "Sheet '%s' not found. Available: %s",
      sheet, paste(sheets, collapse = ", ")
    ))
  }

  updates <- tryCatch(
    jsonlite::fromJSON(updates_json, simplifyDataFrame = FALSE),
    error = function(e) {
      # Retry with single-quote fix
      fixed <- gsub("'", '"', updates_json)
      tryCatch(
        jsonlite::fromJSON(fixed, simplifyDataFrame = FALSE),
        error = function(e2) {
          stop(
            "Failed to parse updates_json as JSON.\n",
            "Received: ", substr(updates_json, 1, 200), "\n",
            "Error: ", conditionMessage(e), "\n\n",
            "updates_json must be a JSON array or object, e.g.:\n",
            '  [{"cell":"B2","value":10}] or {"B2":10}',
            call. = FALSE
          )
        }
      )
    }
  )
  if (!is.list(updates)) updates <- list()

  # Detect dict-style: {"B2": 10, "B3": 20} or {"B5": {"style": {...}}}
  nms <- names(updates)
  if (!is.null(nms) && length(nms) > 0L &&
      all(grepl("^[A-Za-z]+\\d+$", nms))) {
    updates <- lapply(nms, function(nm) {
      val <- updates[[nm]]
      if (is.list(val)) {
        val$cell <- nm
        # Detect dict-style where value is style props without a "style"
        # wrapper, e.g. {"B5": {"fill": "#00FF00"}} or
        # {"B5": {"fgFill": "#00FF00", "bold": true}}.
        # If the list has no "value", "formula", or "style" key but has
        # recognized style-property keys, wrap them into $style.
        known_action_keys <- c("cell", "value", "formula", "style")
        style_keys <- c(
          "fgFill", "bgColor", "backgroundColor", "fgColor", "fill",
          "fontColour", "fontColor", "color", "font",
          "textDecoration", "bold", "italic",
          "fontSize", "numFmt", "fontName",
          "halign", "valign", "wrapText",
          "border", "borderColour", "borderStyle"
        )
        non_cell_names <- setdiff(names(val), "cell")
        if (length(non_cell_names) > 0L &&
            !any(non_cell_names %in% c("value", "formula", "style")) &&
            any(non_cell_names %in% style_keys)) {
          # Move style-like keys into a nested $style list
          val$style <- val[non_cell_names]
          val[non_cell_names] <- NULL
        }
        val
      } else {
        list(cell = nm, value = val)
      }
    })
  }

  # Normalise: if it looks like a single update, wrap it
  if (!is.null(updates$cell)) {
    updates <- list(updates)
  }

  count_val <- 0L
  count_fml <- 0L
  count_style <- 0L

  for (u in updates) {
    cell_ref <- toupper(trimws(as.character(u$cell)))
    parsed <- parse_cell_ref_upd(cell_ref)

    # Apply style if provided
    if (!is.null(u$style) && is.list(u$style)) {
      style_args <- list()
      s <- u$style
      # Accept both canonical (fgFill/fontColour) and common aliases.
      # Also handle nested openpyxl-style: {"fill":{"fgColor":"#00FF00"}}
      fg <- s$fgFill
      if (is.null(fg)) fg <- s$bgColor
      if (is.null(fg)) fg <- s$backgroundColor
      if (is.null(fg)) fg <- s$fgColor
      if (is.null(fg) && is.list(s$fill)) {
        fg <- s$fill$fgColor
        if (is.null(fg)) fg <- s$fill$start_color
        if (is.null(fg)) fg <- s$fill$color
      }
      if (is.null(fg) && is.character(s$fill)) fg <- s$fill
      if (!is.null(fg)) {
        style_args$fgFill <- as.character(fg)
      }
      fc <- s$fontColour
      if (is.null(fc)) fc <- s$fontColor
      if (is.null(fc)) fc <- s$color
      if (is.null(fc) && is.list(s$font)) {
        fc <- s$font$color
        if (is.null(fc)) fc <- s$font$fontColor
        if (is.null(fc)) fc <- s$font$fontColour
      }
      if (!is.null(fc)) {
        style_args$fontColour <- as.character(fc)
      }
      if (!is.null(s$textDecoration)) {
        style_args$textDecoration <- as.character(s$textDecoration)
      }
      td <- s$bold
      if (!is.null(td) && isTRUE(as.logical(td))) {
        style_args$textDecoration <- c(
          style_args$textDecoration, "bold"
        )
      }
      if (!is.null(s$fontSize)) {
        style_args$fontSize <- as.numeric(s$fontSize)
      }
      if (!is.null(s$numFmt)) {
        style_args$numFmt <- as.character(s$numFmt)
      }
      if (!is.null(s$fontName)) {
        style_args$fontName <- as.character(s$fontName)
      }
      if (!is.null(s$halign)) {
        style_args$halign <- as.character(s$halign)
      }
      if (!is.null(s$valign)) {
        style_args$valign <- as.character(s$valign)
      }
      if (!is.null(s$wrapText)) {
        style_args$wrapText <- as.logical(s$wrapText)
      }
      if (!is.null(s$border)) {
        style_args$border <- as.character(s$border)
      }
      if (!is.null(s$borderColour)) {
        style_args$borderColour <- as.character(s$borderColour)
      }
      if (!is.null(s$borderStyle)) {
        style_args$borderStyle <- as.character(s$borderStyle)
      }
      if (length(style_args) > 0L) {
        cell_style <- do.call(openxlsx::createStyle, style_args)
        openxlsx::addStyle(
          wb, sheet, style = cell_style,
          rows = parsed$row, cols = parsed$col,
          gridExpand = FALSE, stack = TRUE
        )
        count_style <- count_style + 1L
      }
    }

    if (!is.null(u$formula) && nzchar(u$formula)) {
      formula_text <- sub("^=", "", as.character(u$formula))
      openxlsx::writeFormula(
        wb, sheet,
        x = formula_text,
        startCol = parsed$col,
        startRow = parsed$row
      )
      count_fml <- count_fml + 1L
    } else if (!is.null(u$value)) {
      val <- u$value
      openxlsx::writeData(
        wb, sheet,
        x = val,
        startCol = parsed$col,
        startRow = parsed$row,
        colNames = FALSE
      )
      count_val <- count_val + 1L
    }
  }

  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)

  parts <- character(0L)
  if (count_val > 0L) parts <- c(parts, sprintf("%d value(s)", count_val))
  if (count_fml > 0L) parts <- c(parts, sprintf("%d formula(s)", count_fml))
  if (count_style > 0L) parts <- c(parts, sprintf("%d style(s)", count_style))

  sprintf(
    "Updated '%s' sheet '%s': %s written.",
    basename(file), sheet, paste(parts, collapse = ", ")
  )
}


# -- Helper: parse cell reference --
parse_cell_ref_upd <- function(ref) {
  ref <- toupper(trimws(ref))
  m <- regmatches(ref, regexec("^([A-Z]+)(\\d+)$", ref))[[1L]]
  if (length(m) != 3L) {
    stop("Invalid cell reference: '", ref, "'")
  }
  col_letters <- m[[2L]]
  row_num <- as.integer(m[[3L]])
  chars <- strsplit(col_letters, "")[[1L]]
  col_num <- 0L
  for (ch in chars) {
    col_num <- col_num * 26L + match(ch, LETTERS)
  }
  list(col = col_num, row = row_num)
}


# CLI entry-point
if (sys.nframe() == 0L) {
  "Update cells in an Excel workbook.

Usage:
  xlsx_update.R --file=<path> --updates_json=<json> [--sheet=<name>]
  xlsx_update.R (-h | --help)

Options:
  --file=<path>           Path to the .xlsx file
  --updates_json=<json>   JSON array of update specs
  --sheet=<name>          Sheet name [default: Sheet1]
  -h --help               Show this help
" -> doc

  args <- tricobbler::docopt(doc)
  cat(xlsx_update(
    file = args$file,
    updates_json = args$updates_json,
    sheet = args$sheet %||% "Sheet1"
  ), "\n")
}
