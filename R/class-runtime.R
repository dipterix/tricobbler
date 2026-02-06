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
            description <- paste(description, collapse = "")
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
    },

    .create_run_impl = function(now = TRUE) {
      state_name <- private$.policy@name
      stage <- private$.policy@stage
      agent <- private$.agent
      attempt <- private$.attempt

      # logger runs in main session
      private$.context$logger(
        "starting (attempt ", attempt, ")",
        caller = self, level = "TRACE"
      )
      succeed <- FALSE
      result_description <- structure(list(), class = "missing")

      # async part
      impl <- function() {
        if (now) {
          private$.status <- "running"
        } else {
          private$.status <- "running (async)"
        }
        time_started <- Sys.time()

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

        result <- tryCatch(
          {
            result <- agent(runtime = self)
            if (!now) {
              # check if result is a promise
              is_promise <- promises::is.promise(result)
              if (!is_promise && promises::is.promising(result)) {
                result <- promises::as.promise(result)
                is_promise <- TRUE
              }
              if (is_promise) {
                result <- await(result)
              }
            }
            succeed <- TRUE
            private$.status <- "finished"
            result
          },
          error = function(e) {
            private$.last_error <- e
            private$.status <- "errored"
            e
          }
        )

        return(
          private$.record_result(
            result,
            succeed,
            started = time_started,
            duration = Sys.time() - time_started
          )
        )
      }
      if (now) {
        return(impl)
      } else {
        return(async(impl))
      }
    }
  ),
  active = list(
    #' @field agent Agent object, the agent being executed (who)
    agent = function() { private$.agent },

    #' @field context `AgentContext` object, the execution context (where)
    context = function() { private$.context },

    #' @field policy `StatePolicy` object, the policy being executed (what)
    policy = function() { private$.policy },

    #' @field attempt integer, retry count if failed
    attempt = function() { private$.attempt },

    #' @field attachment_id character, attachment prefix
    attachment_id = function() { private$.attachment_id },

    #' @field id character, short identifier for this execution to show
    #'   in the context logs
    id = function() { private$.id },

    status = function() { private$.status }
  ),
  public = list(

    #' @description Initialize a new runtime environment
    #' @param agent Agent object being executed
    #' @param context AgentContext object for logging and storage
    #' @param policy StatePolicy object being executed
    initialize = function(agent, context, policy, attempt = 0L) {
      attempt <- as.integer(attempt)
      if (!isTRUE(attempt >= 0)) {
        stop("Invalid attempt: must be a non-negative integer")
      }
      private$.agent <- agent
      private$.context <- context
      private$.policy <- policy
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

      private$.id <- substr(digest::digest(list(private$.attachment_id, now)), 1L, 6L)

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
    #' @return NULL invisibly
    logger = function(
      ...,
      level = c("INFO", "TRACE", "DEBUG", "WARN", "ERROR", "FATAL"),
      verbose = c("cli", "base", "none"), public = TRUE, role = NA_character_
    ) {

      log_path <- file.path(private$.context$attachment_path, sprintf("%s.log", self$attachment_id))
      if (is.na(role)) {
        role <- sprintf("Agent %s", private$.agent@id)
      }
      level <- match.arg(level)
      if (length(verbose) > 1) {
        verbose <- match.arg(verbose)
      }

      # log to the attachment
      log_to_file(..., path = log_path, role = role, level = level, verbose = verbose)

      if (public && identical(private$.pid, Sys.getpid())) {
        private$.context$logger(
          ...,
          caller = self,
          level = level,
          verbose = FALSE
        )
      }

    },

    run_async = function() {
      run_impl <- private$.create_run_impl(now = FALSE)
      run_impl()
    },

    run = function() {
      run_impl <- private$.create_run_impl(now = TRUE)
      run_impl()
    }
  )
)

