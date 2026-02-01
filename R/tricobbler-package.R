#' @importFrom R6 R6Class
#' @importFrom S7 new_class
#' @importFrom ellmer tool
#' @importFrom yaml as.yaml
#' @importFrom utils capture.output
#' @importFrom utils head
#' @importFrom utils str
NULL

## usethis namespace: start
## usethis namespace: end
NULL


.globals <- local({
  map <- NULL
  function() {
    if (is.null(map)) {
      map <<- fastmap::fastmap(missing_default = fastmap::key_missing())
    }
    map
  }
})

with_globals <- function(
  named_list, expr, quoted = FALSE, env = parent.frame()
) {

  if (!quoted) {
    expr <- substitute(expr)
  }

  globals <- .globals()

  all_keys <- names(named_list)
  current_globals <- globals$mget(keys = all_keys)
  keys_missing <- all_keys[
    vapply(current_globals, fastmap::is.key_missing, FUN.VALUE = FALSE)
  ]

  globals$mset(.list = named_list)

  on.exit({
    globals$mset(.list = current_globals)
    globals$remove(keys_missing)
  }, add = TRUE, after = TRUE)

  eval(expr, envir = env)

}

get_globals <- function(keys, missing = NULL, simplify = TRUE) {
  globals <- .globals()
  if (simplify && length(keys) == 1) {
    ret <- globals$get(key = keys, missing = missing)
  } else {
    ret <- globals$mget(keys = keys, missing = missing)
  }
  ret
}

.onLoad <- function(libname, pkgname) {
  S7::methods_register()
}
