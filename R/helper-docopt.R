#' @export
docopt <- function(doc, args = commandArgs(TRUE), ...) {
  parsed <- tryCatch(
    {
      docopt::docopt(doc = doc, args = args, ...)
    },
    error = function(e) {
      stop(conditionMessage(e), "\nPlease check the following usage:\n", doc)
    }
  )
  parsed
}
