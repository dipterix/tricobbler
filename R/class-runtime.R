#' Agent Runtime Environment for State Execution
#'
#' @description R6 class that provides an isolated execution environment for
#'   agent execution. Contains references to the executing agent (who), the
#'   context (where), and the policy (what). This class is instantiated
#'   per-execution and passed to agents, enabling
#'   \verb{async}-safe execution without
#'   relying on global state.
#'
#' @details
#' ## Purpose
#'
#' The runtime serves three main purposes:
#' \enumerate{
#'   \item \strong{\verb{Async} safety}: Captures execution context
#'     in closures, avoiding race conditions with global state when agents
#'     execute concurrently
#'   \item \strong{Simplified API}: Agents receive a single \code{runtime}
#'     argument instead of separate \code{agent}, \code{policy},
#'     \code{context} arguments
#'   \item \strong{Tool injection}: \verb{MCP} tools that declare a
#'     \code{.runtime} parameter receive the runtime automatically via
#'     closure capture at \verb{instantiation} time
#' }
#'
#' ## Tool Runtime Injection
#'
#' When \code{\link{mcptool_instantiate}} creates tool wrappers, it checks if
#' the underlying function has a \code{.runtime} formal parameter. If so, the
#' runtime is automatically injected into calls. This allows downstream
#' packages to create \verb{MCP} tools that access the execution context:
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
    .master_policy = NULL,
    .attempt = 0L,
    .id = character(),
    .attachment_id = character(),
    .pid = NULL,
    .status = "idle",

    .record_result = function(result, succeed, ...) {
      fname <- sprintf("%s.rds", private$.attachment_id)

      description <- NULL
      if (is.function(private$.agent@describe)) {
        tryCatch(
          {
            description <- private$.agent@describe(result)
            description <- paste(description, collapse = "\n")
          },
          error = function(e) {}
        )
      }

      # Prepare the result
      if (!succeed && inherits(result, "error")) {
        class(result) <- c("tricobbler_state_error", class(result))
      }

      attachment <- list(
        result = result,
        succeed = succeed,

        description = description,
        agent_id = private$.agent@id,
        stage = private$.policy@stage,
        state = private$.policy@name,
        current_attempt = private$.attempt,
        ...,
        id = fname,
        ._timestamp = Sys.time()
      )

      attachment_folder <- private$.context$attachment_path
      if (!dir.exists(attachment_folder)) {
        dir.create(attachment_folder, showWarnings = FALSE, recursive = TRUE)
      }

      attachment_path <- file.path(attachment_folder, fname)
      saveRDS(attachment, attachment_path)

      if (inherits(result, "tricobbler_state_error")) {
        level <- "ERROR"
        prefix <- "Following error"
        # message(mcp_describe(result))
      } else {
        level <- "INFO"
        prefix <- "Following result"
      }
      if (!length(description)) {
        description <- "(Agent fail to describe the result. Please retrieve the raw output via the identifier)" # nolint: line_length_linter.
      }

      status <- if (succeed) "finished" else "errored"

      # Show the results
      self$logger(
        sprintf(
          "%s saved: Agent=%s, Stage=%s, State=%s, Attempt=%d, Status=%s, Attachment=%s\n%s",  # nolint: line_length_linter.
          prefix, private$.agent@id, private$.policy@stage,
          private$.policy@name, private$.attempt, status,
          self$attachment_id, description
        ),
        role = sprintf("Runtime %s", self$id), level = level
      )

      # inform context to update the index
      private$.context$record_attachment(self, succeed)

      invisible(attachment)
    }

  ),
  active = list(
    #' @field agent \code{\link{Agent}} object, the agent being executed (who)
    agent = function() {
      private$.agent
    },

    #' @field context \code{\link{AgentContext}} object, the execution
    #'   context (where)
    context = function() {
      private$.context
    },

    #' @field policy \code{\link{StatePolicy}} object, the policy being
    #'   executed (what)
    policy = function() {
      private$.policy
    },

    #' @field attempt integer, retry count if failed
    attempt = function() {
      private$.attempt
    },

    #' @field attachment_id character, attachment prefix
    attachment_id = function() {
      private$.attachment_id
    },

    #' @field id character, short identifier for this execution to show
    #'   in the context logs
    id = function() {
      private$.id
    },

    #' @field master_policy \code{\link{MasterPolicy}} object,
    #'   the master-level policy (global parameters)
    master_policy = function() {
      private$.master_policy
    },

    #' @field status character, current runtime status
    #'   (\code{"idle"}, \code{"running"}, or \code{"completed"})
    status = function() {
      private$.status
    }
  ),
  public = list(

    #' @description Initialize a new runtime environment
    #' @param agent \code{\link{Agent}} object being executed
    #' @param context \code{\link{AgentContext}} object for logging and storage
    #' @param policy \code{\link{StatePolicy}} object being executed
    #' @param attempt integer, retry count (default: \code{0L})
    #' @param master_policy \code{\link{MasterPolicy}} object (optional),
    #'   provides global parameters that cascade to agents
    initialize = function(agent, context, policy, attempt = 0L,
                          master_policy = NULL) {
      attempt <- as.integer(attempt)
      if (!isTRUE(attempt >= 0)) {
        stop("Invalid attempt: must be a non-negative integer")
      }
      private$.agent <- agent
      private$.context <- context
      private$.policy <- policy
      private$.master_policy <- master_policy
      private$.attempt <- attempt

      # AgentRuntime will be created by scheduler, which is the main session
      # this flag make sure if the runtime is serialized to other sessions
      # we have a way to prevent it from logging to the main log
      private$.pid <- Sys.getpid()

      # Calculate unique ID - this will be the attachment prefix
      now <- Sys.time()
      private$.attachment_id <- sprintf(
        "[%s][%s][%s]_%s_%d",
        policy@stage, policy@name, agent@id,
        format(now, "%y%m%dT%H%M%S"), attempt
      )

      private$.id <- substr(
        digest::digest(list(private$.attachment_id, now)),
        1L, 6L
      )

      # Register in the attachment index as 'init'
      private$.context$index$register(
        attachment_id = private$.attachment_id,
        stage = policy@stage,
        state = policy@name,
        agent_id = agent@id,
        attempt = attempt
      )

      self$logger(
        sprintf("Runtime initialized, attachment ID: `%s`", self$attachment_id),
        role = sprintf("Runtime %s", self$id), level = "TRACE"
      )
    },

    #' @description Log a message with the agent as caller
    #' @param ... character, message components to paste together
    #' @param level character, log level (INFO, WARN, ERROR, FATAL, DEBUG)
    #' @param verbose character or logical, verbosity setting
    #' @param public logical, whether to also log to the context log
    #' @param role character, role label for the log entry
    #' @return NULL invisibly
    logger = function(
      ...,
      level = c("INFO", "TRACE", "DEBUG", "WARN", "ERROR", "FATAL"),
      verbose = c("cli", "base", "none"), public = TRUE, role = NA_character_
    ) {

      log_path <- file.path(
        private$.context$attachment_path,
        sprintf("%s.log", self$attachment_id)
      )
      if (is.na(role)) {
        role <- sprintf("Agent %s", private$.agent@id)
      }
      level <- match.arg(level)
      if (length(verbose) > 1) {
        verbose <- match.arg(verbose)
      }

      # log to the attachment
      log_to_file(
        ..., path = log_path,
        role = role, level = level,
        verbose = verbose
      )

      if (public && identical(private$.pid, Sys.getpid())) {
        private$.context$logger(
          ...,
          caller = self,
          level = level,
          verbose = FALSE
        )
      }

    },

    #' @description Retrieve a parameter by name with cascading lookup.
    #'
    #' When \code{levels = "cascade"} (default), looks first in the
    #' state-level policy parameters (\code{policy@@parameters$args}),
    #' then falls back to the master-level policy parameters
    #' (\code{master_policy@@parameters}). Use \code{"local"} or
    #' \code{"global"} to restrict the search scope.
    #'
    #' @param key character, the parameter name to look up
    #' @param missing default value to return when the key is not
    #'   found at the requested level(s) (default: \code{NULL})
    #' @param levels character, lookup scope: \code{"cascade"}
    #'   (local then global), \code{"local"} (state policy only), or
    #'   \code{"global"} (master policy only)
    #' @return The parameter value, or \code{missing} if not found.
    get_parameter = function(key, missing = NULL,
                             levels = c("cascade", "global", "local")) {
      levels <- match.arg(levels)
      # Local: state-level parameters$args
      if (levels %in% c("cascade", "local")) {
        local_args <- private$.policy@parameters$args
        if (!is.null(local_args) && key %in% names(local_args)) {
          return(local_args[[key]])
        }
      }
      # Global: master-level parameters
      if (levels %in% c("cascade", "global")) {
        master <- private$.master_policy
        if (!is.null(master)) {
          master_params <- master@parameters
          if (key %in% names(master_params)) {
            return(master_params[[key]])
          }
        }
      }
      missing
    },

    #' @description Execute the agent asynchronously, returning a
    #'   \code{promises::promise} that resolves with the recorded attachment
    #' @return A \code{promises::promise} object
    run_async = function() {
      state_name <- private$.policy@name
      stage <- private$.policy@stage
      agent <- private$.agent
      attempt <- private$.attempt

      # logger runs in main session
      private$.context$logger(
        "starting (attempt ", attempt, ")",
        caller = self, level = "TRACE"
      )


      private$.status <- "running (async)"

      # Update index status to 'running'
      private$.context$index$update_status(
        private$.attachment_id, "running"
      )

      self$logger(
        sprintf(
          "Runtime started: Agent=%s, Stage=%s, State=%s, Attempt=%d, Attachment=%s",  # nolint: line_length_linter.
          agent@id, stage, state_name, attempt, self$attachment_id),
        role = sprintf("Runtime %s", self$id), level = "TRACE"
      )

      time_started <- Sys.time()

      # agent() is a coro::async coroutine; calling it returns
      # a promise that resolves with the agent's return value.
      # If the coroutine throws synchronously (before any await),
      # the error escapes the promise; catch it and reject.
      agent_promise <- tryCatch(
        agent(runtime = self),
        error = function(e) {
          private$.last_error <- e
          private$.status <- "errored"
          promises::promise_reject(e)
        }
      )

      # Ensure we have a promise
      if (!promises::is.promise(agent_promise)) {
        if (promises::is.promising(agent_promise)) {
          agent_promise <- promises::as.promise(agent_promise)
        } else {
          # Synchronous result (non-promise); wrap it
          val <- agent_promise
          private$.status <- "finished"
          agent_promise <- promises::promise_resolve(val)
        }
      }

      agent_promise$then(
        onFulfilled = function(result) {
          private$.status <- "finished"
          private$.record_result(
            result,
            succeed = TRUE,
            started = time_started,
            duration = Sys.time() - time_started
          )
        },
        onRejected = function(e) {
          private$.last_error <- e
          private$.status <- "errored"
          private$.record_result(
            e,
            succeed = FALSE,
            started = time_started,
            duration = Sys.time() - time_started
          )
        }
      )
    },

    #' @description Execute the agent synchronously, blocking
    #'   until completion and returning the recorded attachment
    #' @return The attachment list (invisibly)
    run = function() {
      state_name <- private$.policy@name
      stage <- private$.policy@stage
      agent <- private$.agent
      attempt <- private$.attempt

      # logger runs in main session
      private$.context$logger(
        "starting (attempt ", attempt, ")",
        caller = self, level = "TRACE"
      )


      private$.status <- "running"

      # Update index status to 'running'
      private$.context$index$update_status(
        private$.attachment_id, "running"
      )

      self$logger(
        sprintf(
          "Runtime started: Agent=%s, Stage=%s, State=%s, Attempt=%d, Attachment=%s",  # nolint: line_length_linter.
          agent@id, stage, state_name, attempt, self$attachment_id),
        role = sprintf("Runtime %s", self$id), level = "TRACE"
      )

      time_started <- Sys.time()

      attachment <- tryCatch(
        {
          result <- agent(runtime = self)
          private$.status <- "finished"
          private$.record_result(
            result,
            succeed = TRUE,
            started = time_started,
            duration = Sys.time() - time_started
          )
        },
        error = function(e) {
          private$.last_error <- e
          private$.status <- "errored"
          private$.record_result(
            e,
            succeed = FALSE,
            started = time_started,
            duration = Sys.time() - time_started
          )
        }
      )

      return(attachment)
    }
  )
)

