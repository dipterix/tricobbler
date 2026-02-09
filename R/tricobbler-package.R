#' @importFrom R6 R6Class
#' @importFrom S7 new_class
#' @importFrom S7 convert
#' @importFrom ellmer tool
#' @importFrom yaml as.yaml
#' @importFrom utils capture.output
#' @importFrom utils head
#' @importFrom utils str
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
  S7::methods_register()
}
