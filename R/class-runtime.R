#' Agent Runtime Environment for State Execution
#'
#' @description R6 class that provides an isolated execution environment for
#'   agent execution. Contains references to the executing agent (who), the
#'   context (where), and the policy (what). This class is instantiated
#'   per-execution and passed to agents, enabling async-safe execution without
#'   relying on global state.
#'
#' @details
#' ## Purpose
#'
#' The runtime serves three main purposes:
#' 1. **Async safety**: Captures execution context in closures, avoiding race
#'    conditions with global state when agents execute concurrently

#' 2. **Simplified API**: Agents receive a single `runtime` argument instead
#'    of separate `self`, `policy`, `context` arguments
#' 3. **Tool injection**: MCP tools that declare a `.runtime` parameter receive
#'    the runtime automatically via closure capture at instantiation time
#'
#' ## Tool Runtime Injection
#'
#' When \code{\link{mcptool_instantiate}} creates tool wrappers, it checks if
#' the underlying function has a `.runtime` formal parameter. If so, the
#' runtime is automatically injected into calls. This allows downstream
#' packages to create MCP tools that access the execution context:
#'
#' \preformatted{
#' my_mcp_tool <- function(arg1, arg2, .runtime = NULL) {
#'   if (!is.null(.runtime)) {
#'     .runtime$logger("Tool called with runtime access")
#'     ctx <- .runtime$context
#'   }
#'   # ... tool implementation
#' }
#' }
#'
#' @export
AgentRuntime <- R6::R6Class(
  classname = "TricobblerAgentRuntime",
  portable = TRUE,
  cloneable = FALSE,
  private = list(
    .last_error = NULL,
    .agent = NULL,
    .context = NULL,
    .policy = NULL,
    .execution_id = character(),

    .create_run_impl = function(attempt = 0L, now = TRUE) {
      state_name <- private$.policy@name
      stage <- private$.policy@stage
      agent <- private$.agent

      # logger runs in main session
      private$.context$logger(
        "starting (attempt ", attempt, ")",
        caller = self
      )
      succeed <- FALSE
      result_description <- structure(list(), class = "missing")

      # async part
      impl <- function() {
        result <- tryCatch(
          {
            if (now) {
              result <- agent(runtime = self)
            } else {
              result <- await(agent(runtime = self))
            }
            succeed <- TRUE
            result
          },
          error = function(e) {
            private$.last_error <- e
            e
          }
        )

        if (is.function(agent@describe)) {
          tryCatch(
            {
              result_description <- agent@describe(result)
            },
            error = function(e) {}
          )
        }

        # Prepare the result
        if (!succeed && inherits(result, "error")) {
          class(result) <- c("tricobbler_state_error", class(result))
        }

        list(
          result = result,
          succeed = succeed,
          stage = stage,
          state = state_name,
          agent_id = agent@id,
          current_attempt = attempt,
          description = result_description
        )
      }
      if(now) {
        return(impl)
      } else {
        return(async(impl))
      }
    }
  ),
  active = list(
    #' @field agent Agent object, the agent being executed (who)
    agent = function() { private$.agent },

    #' @field context AgentContext object, the execution context (where)
    context = function() { private$.context },

    #' @field policy StatePolicy object, the policy being executed (what)
    policy = function() { private$.policy },

    #' @field execution_id character, unique identifier for this execution
    execution_id = function() { private$.execution_id }
  ),
  public = list(

    #' @description Initialize a new runtime environment
    #' @param agent Agent object being executed
    #' @param context AgentContext object for logging and storage
    #' @param policy StatePolicy object being executed
    #' @param execution_id character, unique execution identifier
    #'   (auto-generated if `NULL`)
    initialize = function(agent, context, policy, execution_id = NULL) {
      private$.agent <- agent
      private$.context <- context
      private$.policy <- policy
      private$.execution_id <- execution_id %||% sprintf(
        "%s.%s.%s.%s",
        policy@stage, policy@name, agent@id,
        substr(
          digest::digest(c(agent@id, policy@name, Sys.time()), algo = "md5"),
          1, 4
        )
      )
      invisible(self)
    },

    #' @description Log a message with the agent as caller
    #' @param ... character, message components to paste together
    #' @param level character, log level (INFO, WARN, ERROR, FATAL, DEBUG)
    #' @param verbose character or logical, verbosity setting
    #' @return NULL invisibly
    logger = function(
      ...,
      level = c("INFO", "TRACE", "DEBUG", "WARN", "ERROR", "FATAL"),
      verbose = c("cli", "base", "none")
    ) {
      private$.context$logger(
        ...,
        caller = private$.agent,
        level = level,
        verbose = verbose
      )
    },

    run_async = function(attempt = 0L) {

      run_impl <- private$.create_run_impl(attempt, now = FALSE)
      run_impl()

    },

    run = function(attempt = 0L) {

      run_impl <- private$.create_run_impl(attempt, now = TRUE)
      run_impl()

    }
  )
)

