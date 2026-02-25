#' @importFrom R6 R6Class
#' @importFrom S7 new_class
#' @importFrom S7 convert
#' @importFrom ellmer tool
#' @importFrom yaml as.yaml
#' @importFrom utils capture.output
#' @importFrom utils head
#' @importFrom utils modifyList
#' @importFrom utils str
#' @importFrom processx run
#' @importFrom coro await
#' @importFrom coro async
#' @importFrom promises promise
#' @importFrom promises promise_resolve
#' @importFrom RSQLite SQLite
NULL

## usethis namespace: start
## usethis namespace: end
NULL


.onLoad <- function(libname, pkgname) {
  # .onLoad_shinychat()
  S7::methods_register()
}

#' @exportS3Method base::print
print.chat_hidden <- function(x, ...) {
  cat("<chats hidden>\n")
  invisible(x)
}

#' @exportS3Method base::print
print.skill_output <- function(x, ...) {
  if (is.character(x)) {
    if (package_installed("cli")) {
      cli::cat_line(x)
    } else {
      cat(x, "", sep = "\n")
    }
  } else {
    NextMethod()
  }
  invisible(x)
}
