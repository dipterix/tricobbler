#' Describe an R object for use in \verb{MCP} tools
#'
#' @description
#' An S7 generic function used to describe an R object in a way that can be
#' safely serialized to JSON and sent to \verb{MCP} clients. Unlike direct
#' serialization, this function handles special objects (environments,
#' external pointers, large objects) by generating human-readable descriptions.
#'
#' The maximum print size is controlled by the context cache setting
#' \code{tricobbler.mcp_describe.max_size} (default: 100 lines).
#'
#' @param x The object to describe.
#' @param ... Additional arguments passed down to underlying methods. Unused
#'   arguments are silently ignored.
#'
#' @return A character vector of lines describing the object. If the object
#'   is a simple scalar, may return a single-element character vector.
#'   For \code{ellmer::Content} objects, returns the object as-is.
#'
#' @examples
#' # Describe a data frame
#' mcp_describe(mtcars)
#'
#' # Describe a function
#' mcp_describe(mean)
#'
#' # Describe a matrix
#' mcp_describe(matrix(1:12, 3, 4))
#'
#' @export
mcp_describe <- S7::new_generic("mcp_describe", dispatch_args = "x")

# Helper function to get max_print from context cache
.mcp_get_max_print <- function() {
  ctx <- get_active_context()
  if (is.null(ctx)) {
    return(getOption("tricobbler.mcp_describe.max_size", 100L))
  }
  max_print <- ctx$cache$get("tricobbler.mcp_describe.max_size", 100L)
  if (
    length(max_print) != 1 ||
      !is.numeric(max_print) ||
      !isTRUE(max_print > 0L)
  ) {
    max_print <- getOption("tricobbler.mcp_describe.max_size", 100L)
  }
  max_print
}

# Default method for any object
S7::method(mcp_describe, S7::class_any) <- function(x, ...) {
  .mcp_capture_print(x)
}

# Method for ellmer::Content - return as-is
S7::method(mcp_describe, S7::new_S3_class("Content")) <- function(x, ...) {
  x
}

# Method for json class (from jsonlite::toJSON)
S7::method(mcp_describe, S7::new_S3_class("json")) <- function(x, ...) {
  x
}

# Method for NULL
S7::method(mcp_describe, S7::class_missing) <- function(x, ...) {
  "NULL"
}

# Method for logical
S7::method(mcp_describe, S7::class_logical) <- function(x, ...) {
  if (length(x) == 0) {
    return("logical(0)")
  }

  if (length(x) == 1) {
    return(as.character(x))
  }

  .mcp_capture_print(x)
}

# Method for integer
S7::method(mcp_describe, S7::class_integer) <- function(x, ...) {
  if (length(x) == 0) {
    return("integer(0)")
  }
  if (length(x) == 1) {
    return(paste0(x, "L"))
  }
  .mcp_capture_print(x)
}

# Method for numeric (double)
S7::method(mcp_describe, S7::class_double) <- function(x, ...) {
  if (length(x) == 0) {
    return("numeric(0)")
  }
  if (length(x) == 1) {
    return(as.character(x))
  }
  .mcp_capture_print(x)
}

# Method for character
S7::method(mcp_describe, S7::class_character) <- function(x, ...) {
  if (length(x) == 0) {
    return("character(0)")
  }
  if (length(x) == 1 && nchar(x) < 200) {
    # Return a simple quoted string for short single values
    return(deparse(x))
  }

  .mcp_capture_print(x)
}

# Method for factor
S7::method(mcp_describe, S7::class_factor) <- function(x, ...) {
  .mcp_capture_print(x)
}

# Method for matrix
S7::method(mcp_describe, S7::new_S3_class("matrix")) <- function(x, ...) {
  .mcp_capture_print(x)
}

# Method for array
S7::method(mcp_describe, S7::new_S3_class("array")) <- function(x, ...) {
  .mcp_capture_print(x)
}

# Method for data.frame
S7::method(mcp_describe, S7::new_S3_class("data.frame")) <- function(x, ...) {
  .mcp_capture_print(x)
}

# Method for list
S7::method(mcp_describe, S7::class_list) <- function(x, ...) {
  if (length(x) == 0) {
    return("list()")
  }

  .mcp_capture_print(x)
}

# Method for function
S7::method(mcp_describe, S7::class_function) <- function(x, ...) {
  # Get function definition
  fn_def <- capture.output(print(x))

  # Remove bytecode and environment references
  fn_def <- fn_def[!grepl("^<(bytecode|environment):", fn_def)]

  # If it's a very simple function, we might want to clean it up
  if (length(fn_def) == 0) {
    fn_def <- "<function>"
  }

  fn_def
}

# Method for environment
S7::method(mcp_describe, S7::class_environment) <- function(x, ...) {
  # Environments can't be serialized, so provide a description
  env_name <- environmentName(x)

  if (nzchar(env_name)) {
    desc <- sprintf("<environment: %s>", env_name)
  } else {
    # For unnamed environments, show some info
    desc <- sprintf("<environment: %s>", format(x))
  }

  # List objects in the environment (but don't recurse)
  objs <- ls(envir = x, all.names = FALSE)

  if (length(objs) > 0) {
    n_objs <- length(objs)
    if (n_objs > 10) {
      obj_list <- paste(
        c(head(objs, 10), sprintf("... and %d more", n_objs - 10)),
        collapse = ", "
      )
    } else {
      obj_list <- paste(objs, collapse = ", ")
    }
    desc <- c(desc, sprintf("Objects: %s", obj_list))
  } else {
    desc <- c(desc, "Empty environment")
  }

  desc
}

# Method for externalptr
S7::method(mcp_describe, S7::new_S3_class("externalptr")) <- function(x, ...) {
  # External pointers can't be serialized
  c(
    "<external pointer>",
    sprintf("Address: %s", format(x))
  )
}

# Method for formula
S7::method(mcp_describe, S7::new_S3_class("formula")) <- function(x, ...) {
  deparse(x)
}

# Method for call
S7::method(mcp_describe, S7::class_call) <- function(x, ...) {
  deparse(x)
}

# Method for expression
S7::method(mcp_describe, S7::class_expression) <- function(x, ...) {
  .mcp_capture_print(x)
}

# Method for name (symbol)
S7::method(mcp_describe, S7::class_name) <- function(x, ...) {
  as.character(x)
}

# Method for Date
S7::method(mcp_describe, S7::new_S3_class("Date")) <- function(x, ...) {
  if (length(x) == 0) {
    return("Date(0)")
  }
  if (length(x) == 1) {
    return(as.character(x))
  }
  .mcp_capture_print(x)
}

# Method for POSIXct
S7::method(mcp_describe, S7::new_S3_class("POSIXct")) <- function(x, ...) {
  if (length(x) == 0) {
    return("POSIXct(0)")
  }
  if (length(x) == 1) {
    return(as.character(x))
  }
  .mcp_capture_print(x)
}

# Method for POSIXlt
S7::method(mcp_describe, S7::new_S3_class("POSIXlt")) <- function(x, ...) {
  if (length(x) == 0) {
    return("POSIXlt(0)")
  }
  if (length(x) == 1) {
    return(as.character(x))
  }
  .mcp_capture_print(x)
}

# Helper function to capture print output
.mcp_capture_print <- function(x) {
  max_print <- .mcp_get_max_print()

  # Set reproducible output options
  old_options <- options(
    max.print = max_print,
    width = 80
  )
  on.exit(options(old_options), add = TRUE)

  # Capture output
  out <- tryCatch({
    utils::capture.output(print(x))
  }, error = function(e) {
    # If print fails, try to get some basic info
    c(
      sprintf("<%s object>", paste(class(x), collapse = "/")),
      sprintf("Error printing: %s", conditionMessage(e))
    )
  })

  if (length(out) == 0 || !any(nzchar(out))) {
    # Try message output if regular output is empty
    out <- tryCatch({
      capture.output(print(x), type = "message")
    }, error = function(e) {
      sprintf("<%s object>", paste(class(x), collapse = "/"))
    })
  }

  # Strip ANSI codes if present (in case any packages add them)
  out <- gsub("\033\\[[0-9;]*m", "", out)

  out
}
