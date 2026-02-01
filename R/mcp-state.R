#' Create Session-Scoped \verb{MCP} State Environment
#'
#' @description
#' Factory function that creates isolated state environments for \verb{MCP} tool
#'  sessions. Each session maintains independent pipeline context, eliminating
#'  cross-contamination between concurrent chats or users.
#'
#' @param initialize Optional function to initialize the session state.
#'  Should accept the environment as its first argument and perform any
#'  required setup
#' @param force_init Logical. If `TRUE`, runs the `initialize` function even
#'  if the environment appears already initialized. Default is `FALSE`.
#' @param .state Optional existing environment to use. If provided, it will be
#'   validated and potentially initialized. If `NULL`, creates a new environment
#'
#' @return An environment of class `"tricobbler_mcp_session_state"` with
#'  methods available for key-value storage:
#' \describe{
#'   \item{`reset()`}{Remove all objects from the map}
#'   \item{`set(key, value)`}{Set a key-value pair. Returns the value}
#'   \item{`get(key, missing = NULL)`}{Get a value by key. Returns `missing` 
#'         if key not found}
#'   \item{`mget(keys, missing = NULL)`}{Get multiple values by keys}
#'   \item{`has(key)`}{Check if a key exists. Returns `TRUE` or `FALSE`}
#'   \item{`remove(key)`}{Remove a key-value pair}
#'   \item{`keys(sort = FALSE)`}{Get all keys}
#'   \item{`size()`}{Get the number of items in the map}
#'   \item{`clone()`}{Return a shallow copy of the map}
#'   \item{`as_list(sort = FALSE)`}{Return a named list of all key-value pairs}
#' }
#' and internal variables:
#' \describe{
#'   \item{`.initialized`}{Logical flag indicating initialization status}
#'   \item{`.stores`}{The underlying object that persists the key-values}
#' }
#' Parent environment contains read-only shared variables:
#' \describe{
#'   \item{`.platform`}{Platform information from `.Platform`}
#'   \item{`.interactive`}{Whether session is interactive}
#'   \item{`.session_info`}{Session information from `sessionInfo()`}
#' }
#'
#'
#' @examples
#' # Create a fresh session state
#' state1 <- mcptool_state_factory()
#' state1$set("current_pipeline", "my_pipeline")
#' state1$.initialized  # TRUE
#'
#' # Create with initialization
#' init_fn <- function(env) {
#'   env$set("work_path", getwd())
#' }
#' state2 <- mcptool_state_factory(initialize = init_fn)
#' state2$get("work_path")
#'
#' # Reuse existing environment
#' state3 <- mcptool_state_factory(.state = state1)
#' identical(state1, state3)  # TRUE
#'
#' # Access shared read-only parent variables
#' get(".platform", state1)
#' get(".session_info", state1)
#'
#' @export
mcptool_state_factory <- function(initialize = NULL,
                                  force_init = FALSE,
                                  .state = NULL) {
  # parent_env is the static environment containing read-only variables
  # shared across different tools, and can be shared across the workflows
  #
  # state_env is the environment for the specific toolset (workflow)
  if (is.environment(.state)) {
    parent_env <- parent.env(.state)
    state_env <- .state
  } else {
    parent_env <- new.env(parent = globalenv())
    state_env <- new.env(parent = parent_env)
    .state <- state_env
  }

  # Make sure parent_env has all the basic information
  parent_env$.platform <- .Platform
  parent_env$.interactive <- interactive()
  parent_env$.session_info <- utils::sessionInfo()

  if (force_init || !isTRUE(state_env$.initialized)) {
    if (!inherits(state_env$.stores, "fastmap2")) {
      state_env$.stores <- with(state_env, {
        fastmap::fastmap()
      })
      list2env(state_env$.stores, envir = state_env)
      class(state_env$.stores) <- c("tricobbler_fastmap2", "fastmap2", "list")
    }

    if (is.function(initialize)) {
      initialize(state_env)
    }
    state_env$.initialized <- TRUE
  }


  # Set class for identification
  class(state_env) <- "tricobbler_mcp_session_state"

  return(state_env)
}


#' @export
print.tricobbler_mcp_session_state <- function(x, ...) {
  cat("<RAVE Pipeline MCP Session State>\n")
  cat("  Initialized:", x[[".initialized"]] %||% FALSE, "\n")

  # Show parent environment info
  platform <- get0(".platform", x)
  interactive <- get0(".interactive", x)
  cat("  Platform:", platform$OS.type %||% "unknown", "\n")
  cat("  Interactive:", interactive %||% FALSE, "\n")

  # List user-defined variables (exclude metadata)
  store_vars <- as.list(x$.stores)

  if (length(store_vars) > 0) {
    cat("  Stored variables:\n")
    s <- utils::capture.output({
      str(
        store_vars,
        comp.str = "  - ",
        max.level = 1,
        width = getOption("width") - 4L
      )
    })
    cat(s[-1], sep = "\n")
    cat("\n")
  } else {
    cat("  No session variables stored\n")
  }

  invisible(x)
}


