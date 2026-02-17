#' Parse Command-Line Arguments with Enhanced Error Messages
#'
#' @description Wrapper around \code{\link[docopt]{docopt}} that fixes
#'   path-parsing bugs (e.g. paths with parentheses) and prints
#'   informative error messages including the full usage string so
#'   agents can self-correct.
#'
#' @param doc character, usage specification string in
#'   \code{\link[docopt]{docopt}} format
#' @param args character vector, command-line arguments to parse
#'   (defaults to \code{commandArgs(TRUE)})
#' @param ... additional arguments passed to \code{\link[docopt]{docopt}}
#' @returns A named list of parsed arguments
#' @export
docopt <- function(doc, args = commandArgs(TRUE), ...) {
  # docopt::docopt() misparses --key=value when the value contains
  # parentheses (e.g. paths like "~/Dropbox (Personal)/...").
  # Workaround: split "--key=value" into c("--key", "value") so docopt

  # sees the value as a separate token and skips pattern-parsing on it.
  args <- unlist(lapply(args, function(a) {
    m <- regexpr(
      "^([-]{1,}[A-Za-z_][A-Za-z0-9_-]*)=(.*)",
      trimws(a), perl = TRUE
    )
    if (m > 0L) {
      starts <- attr(m, "capture.start")
      lengths <- attr(m, "capture.length")
      key <- substr(a, starts[1L], starts[1L] + lengths[1L] - 1L)
      val <- substr(a, starts[2L], starts[2L] + lengths[2L] - 1L)
      c(key, val)
    } else {
      a
    }
  }), use.names = FALSE)
  parsed <- tryCatch(
    {
      docopt::docopt(doc = doc, args = args, ...)
    },
    error = function(e) {
      msg <- conditionMessage(e)
      # docopt throws cryptic errors like "'short' is not a valid field..."
      # when it encounters unrecognized arguments. Provide a clearer message.
      if (grepl("not a valid field or method name", msg, fixed = TRUE)) {
        stop(
          "Unrecognized or invalid argument(s) passed to this script.\n",
          "Received args: ", paste(args, collapse = " "), "\n",
          "Please check the following usage:\n", doc,
          call. = FALSE
        )
      }
      stop(msg, "\nPlease check the following usage:\n", doc)
    }
  )
  parsed
}
