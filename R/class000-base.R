# Implements some methods
BaseClass = S7::new_class(
  name = "BaseClass",
  abstract = TRUE
)

# --- as.list ---
S7::method(as.list, BaseClass) <- function(x, recursive = TRUE, ...) {
  re <- S7::props(x)
  if(recursive) {

    as_list_impl <- function(x_) {
      if( S7::S7_inherits(x_, BaseClass) ) {
        return(as.list(x_, recursive = TRUE))
      }
      if( is.atomic(x_) || !is.list(x_) ) { return(x_) }
      structure(
        names = names(x_),
        class = class(x_),
        lapply(x_, function(x__) {
          as_list_impl(x__)
        })
      )
    }

    re <- as_list_impl(re)
  }
  structure(
    re,
    class = c(S7::S7_class(x)@name, "list")
  )
}

# --- format & print ---
S7::method(format, BaseClass) <- function(x, ...) {

  cls <- S7::S7_class(x)
  if(S7::prop_exists(x, "version")) {
    version_str <- sprintf("(%s)\n", x@version)
  } else {
    version_str <- " "
  }
  if(S7::prop_exists(x, "stages")) {
    stage_str <- sprintf("Stages: %s\n", paste(x@stages, collapse = ", "))
  } else if (S7::prop_exists(x, "stage")) {
    stage_str <- sprintf("(%s)\n", x@stage)
  } else {
    stage_str <- ""
  }

  if(S7::prop_exists(x, "description")) {
    description_str <- trimws(x@description)
  } else if(S7::prop_exists(x, "description")) {
    description_str <- trimws(x@description)
  } else {
    description_str <- ""
  }

  paste(
    c(
      sprintf("%s (S7 class) - `%s`%s%s%s", cls@name, x@name, version_str, stage_str, description_str)
    ),
    collapse = "\n"
  )
}

S7::method(print, BaseClass) <- function(x, ...) {
  cat(format(x, ...))
  invisible(x)
}
