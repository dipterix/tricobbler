#' Describe an R object for use in \verb{MCP} tools
#'
#' @description
#' A generic function used to describe an R object in a way that can be
#' safely serialized to JSON and sent to \verb{MCP} clients. Unlike direct
#' serialization, this function handles special objects (environments,
#' external pointers, large objects) by generating human-readable descriptions.
#'
#' @param x The object to describe.
#' @param ... Additional arguments passed down to underlying methods. Unused
#'   arguments are silently ignored.
#' @param max_print Maximum number of items to print for long objects.
#'   Default is 100.
#'
#' @return A character vector of lines describing the object. If the object
#'   is a simple scalar, may return a single-element character vector.
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
mcp_describe <- function(x, ..., max_print = 100) {
  UseMethod("mcp_describe")
}

#' @export
mcp_describe.default <- function(x, ..., max_print = 100) {
  .mcp_capture_print(x, max_print = max_print)
}

#' @export
mcp_describe.json <- function(x, ..., max_print = 100) {
  # jsonlite::toJSON already adds "json" class; return as-is
  as.character(x)
}

#' @export
mcp_describe.NULL <- function(x, ..., max_print = 100) {
  "NULL"
}

#' @export
mcp_describe.logical <- function(x, ..., max_print = 100) {
  if (length(x) == 0) {
    return("logical(0)")
  }
  if (length(x) == 1) {
    return(as.character(x))
  }
  .mcp_capture_print(x, max_print = max_print)
}

#' @export
mcp_describe.integer <- function(x, ..., max_print = 100) {
  if (length(x) == 0) {
    return("integer(0)")
  }
  if (length(x) == 1) {
    return(paste0(x, "L"))
  }
  .mcp_capture_print(x, max_print = max_print)
}

#' @export
mcp_describe.numeric <- function(x, ..., max_print = 100) {
  if (length(x) == 0) {
    return("numeric(0)")
  }
  if (length(x) == 1) {
    return(as.character(x))
  }
  .mcp_capture_print(x, max_print = max_print)
}

#' @export
mcp_describe.character <- function(x, ..., max_print = 100) {
  if (length(x) == 0) {
    return("character(0)")
  }
  if (length(x) == 1 && nchar(x) < 200) {
    # Return a simple quoted string for short single values
    return(deparse(x))
  }
  .mcp_capture_print(x, max_print = max_print)
}

#' @export
mcp_describe.factor <- function(x, ..., max_print = 100) {
  .mcp_capture_print(x, max_print = max_print)
}

#' @export
mcp_describe.matrix <- function(x, ..., max_print = 100) {
  .mcp_capture_print(x, max_print = max_print)
}

#' @export
mcp_describe.array <- function(x, ..., max_print = 100) {
  .mcp_capture_print(x, max_print = max_print)
}

#' @export
mcp_describe.data.frame <- function(x, ..., max_print = 100) {
  .mcp_capture_print(x, max_print = max_print)
}

#' @export
mcp_describe.list <- function(x, ..., max_print = 100) {
  if (length(x) == 0) {
    return("list()")
  }
  .mcp_capture_print(x, max_print = max_print)
}

#' @export
mcp_describe.function <- function(x, ..., max_print = 100) {
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

#' @export
mcp_describe.environment <- function(x, ..., max_print = 100) {
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

#' @export
mcp_describe.externalptr <- function(x, ..., max_print = 100) {
  # External pointers can't be serialized
  c(
    "<external pointer>",
    sprintf("Address: %s", format(x))
  )
}

#' @export
mcp_describe.formula <- function(x, ..., max_print = 100) {
  deparse(x)
}

#' @export
mcp_describe.call <- function(x, ..., max_print = 100) {
  deparse(x)
}

#' @export
mcp_describe.expression <- function(x, ..., max_print = 100) {
  .mcp_capture_print(x, max_print = max_print)
}

#' @export
mcp_describe.name <- function(x, ..., max_print = 100) {
  as.character(x)
}

#' @export
mcp_describe.Date <- function(x, ..., max_print = 100) {
  if (length(x) == 0) {
    return("Date(0)")
  }
  if (length(x) == 1) {
    return(as.character(x))
  }
  .mcp_capture_print(x, max_print = max_print)
}

#' @export
mcp_describe.POSIXct <- function(x, ..., max_print = 100) {
  if (length(x) == 0) {
    return("POSIXct(0)")
  }
  if (length(x) == 1) {
    return(as.character(x))
  }
  .mcp_capture_print(x, max_print = max_print)
}

#' @export
mcp_describe.POSIXlt <- function(x, ..., max_print = 100) {
  if (length(x) == 0) {
    return("POSIXlt(0)")
  }
  if (length(x) == 1) {
    return(as.character(x))
  }
  .mcp_capture_print(x, max_print = max_print)
}

# Helper function to capture print output
.mcp_capture_print <- function(x, max_print = 100) {
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
