#' @description Read and display cell contents from an Excel workbook.
#'   Returns the values in the requested range or full sheet.
#'   Optionally includes style information (fill color, font color, etc.).
#'
#' @param file character, path to the Excel workbook.
#' @param sheet character, name or index of the sheet to read
#'   (default \code{"Sheet1"}).
#' @param range character, an A1-style range such as \code{"A1:C5"} or
#'   a single cell like \code{"B4"}.  If empty or \code{"all"}, the
#'   entire used range is returned.
#' @param include_styles character, \code{"true"} or \code{"false"}
#'   (default \code{"false"}).
#'   When \code{"true"}, appends a section listing cell styles
#'   (fill color, font color, bold, number format, etc.) for every
#'   styled cell in the requested range.
#'
#' @return A formatted text representation of the cell contents,
#'   optionally followed by style details.
xlsx_read <- function(file, sheet = "Sheet1", range = "all",
                      include_styles = "false") {

  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Package 'openxlsx' is required. Install with: ",
         "install.packages('openxlsx')")
  }

  if (!file.exists(file)) {
    stop("File not found: ", file)
  }

  wb <- openxlsx::loadWorkbook(file)

  # Resolve sheet
  sheets <- openxlsx::sheets(wb)
  if (is.numeric(sheet)) {
    sheet <- sheets[as.integer(sheet)]
  }
  if (!sheet %in% sheets) {
    return(sprintf(
      "Sheet '%s' not found. Available sheets: %s",
      sheet, paste(sheets, collapse = ", ")
    ))
  }

  # Read full sheet data as data.frame
  df <- openxlsx::readWorkbook(wb, sheet = sheet, colNames = FALSE,
                               skipEmptyRows = FALSE,
                               skipEmptyCols = FALSE)

  # readWorkbook ignores formula-only columns (no cached values).

  # Detect actual sheet dimensions from the internal sheet_data and
  # pad the data frame with NA columns/rows so formula cells are
  # visible in the output grid.
  sheet_idx <- match(sheet, sheets)
  ws <- wb$worksheets[[sheet_idx]]
  if (!is.null(ws$sheet_data)) {
    sd <- ws$sheet_data
    actual_max_row <- if (length(sd$rows) > 0L) max(sd$rows) else 0L
    actual_max_col <- if (length(sd$cols) > 0L) max(sd$cols) else 0L

    if (is.null(df) || nrow(df) == 0L) {
      # Sheet appeared empty but has formula cells
      if (actual_max_row > 0L && actual_max_col > 0L) {
        df <- as.data.frame(
          matrix(NA, nrow = actual_max_row, ncol = actual_max_col),
          stringsAsFactors = FALSE
        )
      }
    } else {
      # Pad extra columns
      if (actual_max_col > ncol(df)) {
        for (ci in seq(ncol(df) + 1L, actual_max_col)) {
          df[[paste0("X", ci)]] <- NA
        }
      }
      # Pad extra rows
      if (actual_max_row > nrow(df)) {
        extra <- as.data.frame(
          matrix(NA, nrow = actual_max_row - nrow(df), ncol = ncol(df)),
          stringsAsFactors = FALSE
        )
        colnames(extra) <- colnames(df)
        df <- rbind(df, extra)
      }
    }

    # Overlay formula text on cells that contain formulas.
    # openxlsx::readWorkbook returns NA for formula cells (no cached
    # value), so we annotate them with "=<formula>" so the output
    # grid makes the formula visible.
    if (!is.null(sd$f) && length(sd$f) > 0L) {
      for (i in seq_along(sd$f)) {
        fml <- sd$f[[i]]
        if (!is.null(fml) && nzchar(fml) && !is.na(fml)) {
          r <- sd$rows[[i]]
          cc <- sd$cols[[i]]
          # Strip XML tags if present
          fml <- sub("^<f[^>]*>", "", fml)
          fml <- sub("</f>$", "", fml)
          if (r >= 1L && r <= nrow(df) && cc >= 1L && cc <= ncol(df)) {
            df[r, cc] <- paste0("=", fml)
          }
        }
      }
    }
  }

  if (is.null(df) || nrow(df) == 0L) {
    return(sprintf("Sheet '%s' is empty.", sheet))
  }

  # If range is specified, extract subset
  if (!is.null(range) && nzchar(range) &&
      !tolower(range) %in% c("all", "")) {

    parsed <- parse_range(range)

    # Clamp to actual dimensions (df is 0-offset from row 1 already)
    max_row <- nrow(df)
    max_col <- ncol(df)

    r1 <- min(parsed$row1, max_row)
    r2 <- min(parsed$row2, max_row)
    c1 <- min(parsed$col1, max_col)
    c2 <- min(parsed$col2, max_col)

    df <- df[r1:r2, c1:c2, drop = FALSE]
  }

  # Format output
  lines <- utils::capture.output(print(df, right = FALSE))
  output <- paste(lines, collapse = "\n")


  # Optionally include style information
  show_styles <- isTRUE(tolower(trimws(as.character(include_styles))) ==
                         "true")

  if (show_styles) {
    style_objs <- wb$styleObjects
    if (length(style_objs) > 0L) {
      # Determine the range bounds for filtering.
      # Use the actual sheet dimensions (not df rows, which may be
      # truncated when formula-only rows appear empty).
      if (!is.null(range) && nzchar(range) &&
          !tolower(range) %in% c("all", "")) {
        p <- parse_range(range)
        r_min <- p$row1; r_max <- p$row2
        c_min <- p$col1; c_max <- p$col2
      } else {
        # Include all rows/cols that any style object references
        all_rows <- unlist(lapply(style_objs, function(so) so$rows))
        all_cols <- unlist(lapply(style_objs, function(so) so$cols))
        r_min <- 1L
        r_max <- max(nrow(df), all_rows, na.rm = TRUE)
        c_min <- 1L
        c_max <- max(ncol(df), all_cols, na.rm = TRUE)
      }

      # Collect style info per cell (deduplicate by cell ref)
      cell_styles <- list()
      for (so in style_objs) {
        so_sheet <- so$sheet
        if (!identical(so_sheet, sheet)) next

        so_rows <- so$rows
        so_cols <- so$cols
        if (is.null(so_rows) || is.null(so_cols)) next

        for (r in so_rows) {
          for (cc in so_cols) {
            if (r < r_min || r > r_max ||
                cc < c_min || cc > c_max) next

            cell_ref <- paste0(num_to_col_letter(cc), r)
            st <- so$style
            props <- character(0L)

            # Fill color: st$fill is a list with $fillFg (ARGB string)
            fg_argb <- NULL
            tryCatch({
              if (is.list(st$fill) && !is.null(st$fill$fillFg)) {
                fg_argb <- st$fill$fillFg
              }
            }, error = function(e) NULL)
            if (!is.null(fg_argb) && nzchar(fg_argb)) {
              # Convert ARGB (e.g. "FF00FF00") to #hex (e.g. "#00FF00")
              hex <- fg_argb
              if (nchar(hex) == 8L) hex <- substr(hex, 3, 8)
              props <- c(props, paste0("fill=#", hex))
            }

            # Font color: st$fontColour is named chr (ARGB)
            fcol <- NULL
            tryCatch({
              fcol <- st$fontColour
              if (is.null(fcol) || !nzchar(fcol)) fcol <- NULL
            }, error = function(e) NULL)
            if (!is.null(fcol) && nzchar(fcol)) {
              hex <- fcol
              if (nchar(hex) == 8L) hex <- substr(hex, 3, 8)
              props <- c(props, paste0("fontColor=#", hex))
            }

            # Bold / text decoration
            decor <- NULL
            tryCatch({
              decor <- st$fontDecoration
              if (length(decor) == 0L || is.na(decor) ||
                  !nzchar(decor)) decor <- NULL
            }, error = function(e) NULL)
            if (!is.null(decor) && nzchar(decor)) {
              props <- c(props, tolower(decor))
            }

            # Font name
            fname <- NULL
            tryCatch({
              fname <- st$fontName
              if (is.null(fname) || !nzchar(fname)) fname <- NULL
            }, error = function(e) NULL)
            if (!is.null(fname)) {
              props <- c(props, paste0("font=", fname))
            }

            # Number format
            nfmt <- NULL
            tryCatch({
              nfmt <- st$numFmt
              if (is.null(nfmt) || is.na(nfmt) ||
                  !nzchar(nfmt)) nfmt <- NULL
            }, error = function(e) NULL)
            if (!is.null(nfmt) && !identical(nfmt, "GENERAL")) {
              props <- c(props, paste0("numFmt=", nfmt))
            }

            if (length(props) > 0L) {
              key <- cell_ref
              existing <- cell_styles[[key]]
              if (is.null(existing)) {
                cell_styles[[key]] <- props
              } else {
                # Merge (keep unique)
                cell_styles[[key]] <- unique(c(existing, props))
              }
            }
          }
        }
      }

      if (length(cell_styles) > 0L) {
        style_lines <- vapply(names(cell_styles), function(k) {
          sprintf("  %s: %s", k, paste(cell_styles[[k]], collapse = ", "))
        }, character(1L))
        output <- paste0(
          output,
          "\n\nStyles:\n",
          paste(style_lines, collapse = "\n")
        )
      } else {
        output <- paste0(output, "\n\nStyles: (none)")
      }
    } else {
      output <- paste0(output, "\n\nStyles: (none)")
    }
  }

  output
}


# -- Parse A1-style range (e.g. "A1:C5" or "B4") --
parse_range <- function(range) {
  range <- toupper(trimws(range))

  # Single cell: "B4"
  single <- regmatches(range, regexec("^([A-Z]+)(\\d+)$", range))[[1L]]
  if (length(single) == 3L) {
    col <- col_letter_to_num(single[[2L]])
    row <- as.integer(single[[3L]])
    return(list(row1 = row, col1 = col, row2 = row, col2 = col))
  }

  # Range: "A1:C5"
  parts <- regmatches(
    range,
    regexec("^([A-Z]+)(\\d+):([A-Z]+)(\\d+)$", range)
  )[[1L]]
  if (length(parts) == 5L) {
    return(list(
      row1 = as.integer(parts[[3L]]),
      col1 = col_letter_to_num(parts[[2L]]),
      row2 = as.integer(parts[[5L]]),
      col2 = col_letter_to_num(parts[[4L]])
    ))
  }

  stop("Cannot parse range: '", range, "'. Use A1 or A1:C5 format.")
}

col_letter_to_num <- function(letters) {
  chars <- strsplit(toupper(letters), "")[[1L]]
  num <- 0L
  for (ch in chars) {
    num <- num * 26L + match(ch, LETTERS)
  }
  num
}

num_to_col_letter <- function(num) {
  result <- ""
  while (num > 0L) {
    remainder <- (num - 1L) %% 26L
    result <- paste0(LETTERS[remainder + 1L], result)
    num <- (num - 1L) %/% 26L
  }
  result
}


# CLI entry-point
if (sys.nframe() == 0L) {
  "Read cells from an Excel workbook.

Usage:
  xlsx_read.R --file=<path> [--sheet=<name>] [--range=<range>] [--include_styles=<bool>]
  xlsx_read.R (-h | --help)

Options:
  --file=<path>             Path to the .xlsx file
  --sheet=<name>            Sheet name or index [default: Sheet1]
  --range=<range>           Cell range (A1:C5) or 'all' [default: all]
  --include_styles=<bool>   Show styles: true/false [default: false]
  -h --help                 Show this help
" -> doc

  args <- tricobbler::docopt(doc)
  cat(xlsx_read(
    file = args$file,
    sheet = args$sheet %||% "Sheet1",
    range = args$range %||% "all",
    include_styles = args$include_styles %||% "false"
  ), "\n")
}
