#' @include mcp-describe.R
#' @include class-s7base.R
#' @include class-baseagent.R
#' @include class-context.R
NULL



#' Workflow Execution Scheduler
#'
#' @description R6 class that orchestrates workflow execution by managing stage
#'   progression, state execution, and agent coordination. This is the primary
#'   runtime component in the Runtime Layer (Tier 2) that brings together the
#'   immutable policy definitions (\code{Manifest}) with executable agents.
#' @export
Scheduler <- R6::R6Class(
  classname = "TricobblerScheduler",
  portable = TRUE,
  cloneable = FALSE,
  private = list(
    .run_flag = NULL,
    .last_error = NULL
  ),
  public = list(
    # What: this must set before running the contract
    #' @field manifest Manifest, the workflow blueprint
    manifest = NULL,

    # Who: this may change during the run so
    #' @field agents \code{fastmap::fastmap()} object, registry of Agent
    #'    objects keyed by agent_id
    agents = NULL,

    # Where
    #' @field context Context, the execution environment for logging and storage
    context = NULL,

    # ready, ... (from manifest), finish
    #' @field current_stage character, the currently executing stage name
    current_stage = character(),

    #' @field stage_started \code{POSIXct}, timestamp when current stage started
    stage_started = NULL,

    #' @field current_state character, the currently executing state name
    current_state = character(),

    #' @field state_started \code{POSIXct}, timestamp when current state started
    state_started = NULL,

    # needs human attention? pause all if TRUE
    #' @field suspended logical, whether execution is paused
    suspended = FALSE,

    #' @description Initialize scheduler with manifest blueprint and agents
    #' @param manifest Manifest object, the workflow blueprint
    #' @param agents list, collection of Agent objects
    #' @param context Context object, execution environment
    #'    (default: new \code{\link{AgentContext}})
    initialize = function(
      manifest,
      agents = list(),
      context = AgentContext$new()
    ) {
      stopifnot(S7::S7_inherits(manifest, Manifest))

      self$manifest <- manifest
      self$agents <- fastmap::fastmap()
      lapply(agents, function(agent) {
        if (S7::S7_inherits(agent, Agent)) {
          self$agents$set(agent@id, agent)
        }
        return()
      })

      self$context <- context
      self$context$set_scheduler(self)

      self$current_stage <- "ready"
      self$stage_started <- Sys.time()

      invisible(self)
    },

    #' @description Verify that all required agents are registered
    #' @param on_error character, action on validation failure
    #'    ("error" or "quiet")
    validate = function(on_error = c("error", "quiet")) {
      on_error <- match.arg(on_error)

      # Make sure the agents are ready
      re <- tryCatch(
        {
          state_agents <- unlist(lapply(
            self$manifest@states,
            function(state_policy) {
              # state_policy <- self$manifest@states[[1]]
              state_policy@agent_id
            }
          ))
          state_agents <- unique(state_agents)

          existing_agents <- self$agents$keys()
          missing_agents <- setdiff(state_agents, existing_agents)
          if (length(missing_agents)) {
            stop(sprintf(
              "Validation failure: The following agents are specified in the manifest but are missing: %s", # nolint: line_length_linter.
              paste(sQuote(missing_agents), collapse = ", ")
            ))
          }
          TRUE
        },
        error = function(e) {
          if (on_error == "error") {
            stop(e)
          }
          FALSE
        }
      )
      re
    },

    #' @description Start the workflow execution from the ready stage
    start = function() {
      # TODO: check if .current_stage is running
      if (!identical(self$current_stage, "ready")) {
        stop(
          "Current state is `",
          self$current_stage,
          "`. Please call `scheduler$stop()` to reset the scheduler first."
        )
      }

      self$validate(on_error = "error")

      # Load all the resources
      self$init_resources()

      self$suspended <- FALSE
      private$.last_error <- NULL
      private$.run_flag <- Sys.time()

      self$current_stage <- "ready"
      self$current_state <- character()
      self$stage_started <- Sys.time()

      self$run_stage()
      invisible(self)
    },

    #' @description Initialize resources and prepare for execution
    #' @param reset_context logical, whether to reset the context storage
    init_resources = function(reset_context = FALSE) {
      # TODO: initialize MCP tools; initialize context
      self$context$set_scheduler(self)
      self$context$init_resources()

      self$context$logger("Initializing resources", caller = self)
    },

    #' @description Execute a specific workflow stage
    #' @param stage character, stage name (NULL to run next stage)
    # Might want to use coro in the future for async running
    run_stage = function(stage = NULL) {
      all_stages <- c("ready", self$manifest@master@stages)
      if (is.null(stage)) {
        # automatically next stage
        idx <- which(all_stages %in% self$current_stage)
        if (length(idx) == 0) {
          idx <- 1L
        } else {
          idx <- idx[[1]] + 1
          if (idx > length(all_stages)) {
            idx <- 1L
          }
        }
        stage <- all_stages[[idx]]
      } else {
        stage <- match.arg(stage, all_stages, several.ok = FALSE)
      }
      if (self$suspended) {
        stop(
          "Unable to run stage `",
          stage,
          "` because the progress is suspended."
        )
      }
      # TODO: finalize the current stage: close connections

      # Start the stage
      self$current_stage <- stage
      self$stage_started <- Sys.time()

      self$context$logger("current stage: ", self$current_stage, caller = self)

      if (stage == "ready") {
        self$current_state <- character()
        return(invisible(self))
      }

      # Find the policies
      state_policies <- extract_manifest_state(self$manifest, stage = stage)

      policy_agent_map <- fastmap::fastmap()
      state_names <- lapply(state_policies, function(policy) {
        agent <- self$agents$get(policy@agent_id)
        policy_agent_map$set(policy@name, list(agent = agent, policy = policy))
        return(policy@name)
      })
      state_names <- unlist(state_names)
      current_flag <- private$.run_flag

      run_state <- function(state_name, retry_map = list()) {
        # state_name = state_policies[[1]]@name

        # Check if the work is stopped
        if (!identical(current_flag, private$.run_flag)) {
          stop("Scheduler flags changed. Abort current procedure.")
        }

        self$current_state <- state_name
        self$state_started <- Sys.time()
        item <- policy_agent_map$get(state_name)
        agent <- item$agent
        policy <- item$policy

        # Get cumulative failure count for this state
        init_retry_count <- c(retry_map[[state_name]], 0L)[[1]]
        succeed <- FALSE
        max_retry <- max(policy@max_retry, 0L)

        # Execute agent once (agent handles transient retries
        # internally if needed)
        result <- with_globals(
          list(
            active_context = context,
            active_agent = agent,
            active_policy = policy
          ),
          {
            tryCatch(
              {
                self$context$logger(
                  "starting state: ", state_name,
                  " with agent ", agent@id,
                  " (attempt ", init_retry_count, ")",
                  caller = self
                )
                result <- agent(
                  self = agent,
                  policy = policy,
                  context = self$context
                )
                succeed <- TRUE
                result
              },
              error = function(e) {
                print(e)
                private$.last_error <- e
                e
              }
            )
          }
        )

        if (!identical(current_flag, private$.run_flag)) {
          stop("Scheduler flags changed. Abort current procedure.")
        }

        result_description <- structure(list(), class = "missing")
        if (is.function(agent@describe)) {
          tryCatch(
            {
              result_description <- agent@describe(result)
            },
            error = function(e) {}
          )
        }

        # Record the result
        if (!succeed && inherits(result, "error")) {
          class(result) <- c("tricobbler_state_error", class(result))
        }

        self$context$record_result(
          result = result,
          succeed = succeed,
          stage = stage,
          state = state_name,
          agent_id = agent@id,
          current_attempt = init_retry_count,
          description = result_description
        )

        # Determine next state based on execution outcome
        if (succeed) {
          if (policy@final) {
            return()
          }
          # Success - move to next state in priority order
          state_idx <- which(state_names == state_name)
          if (state_idx == length(state_names)) {
            # stage finished
            return()
          }
          next_name <- state_names[state_idx + 1]
        } else {
          # Failure - increment retry count for this state
          retry_map[[state_name]] <- init_retry_count + 1L

          # Check if retry limit exceeded
          if (retry_map[[state_name]] > max_retry) {
            if (policy@critical) {
              self$suspend()
              return()
            }
            # Skip to next state
            state_idx <- which(state_names == state_name)
            if (state_idx == length(state_names)) {
              # stage finished with failure
              return()
            }
            next_name <- state_names[state_idx + 1]
          } else {
            # Still within retry budget - determine retry target
            if (is.na(policy@on_failure)) {
              # Default: retry current state
              next_name <- state_name
            } else {
              # Jump to on_failure state
              state_idx <- which(state_names == policy@on_failure)
              next_name <- state_names[state_idx]
            }
          }
        }

        # Validate next_name has retry budget remaining
        # If exhausted, skip forward until we find a valid state
        while (TRUE) {
          next_item <- policy_agent_map$get(next_name)
          next_policy <- next_item$policy
          next_attempts <- c(retry_map[[next_name]], 0L)[[1]]
          next_max_retry <- max(next_policy@max_retry, 0L)

          if (next_attempts > next_max_retry) {
            # Target exhausted - use current state's critical flag
            self$context$logger(
              "State `",
              next_name,
              "` exhausted its retry budget (",
              next_attempts,
              " > ",
              next_max_retry,
              "). Skipping forward from current state `",
              state_name,
              "`",
              caller = self
            )

            if (policy@critical) {
              self$suspend()
              return()
            }

            # Skip to next state in priority order
            next_idx <- which(state_names == next_name)
            if (next_idx == length(state_names)) {
              # stage finished with failure
              return()
            }
            next_name <- state_names[next_idx + 1]
          } else {
            # Valid state found - proceed
            break
          }
        }

        Recall(next_name, retry_map = retry_map)
      }

      run_state(state_policies[[1]]@name, retry_map = list())

      # next stage
      self$run_stage()
    },

    #' @description Suspend the workflow execution
    suspend = function() {
      self$suspended <- TRUE
      self$context$logger(
        "scheduler suspended at ",
        self$current_stage, " -> ", self$current_state,
        " due to a critical failure. ",
        "Human attention might be needed. \nLast error: \n",
        paste0(
          c(
            private$.last_error$message,
            utils::capture.output(traceback(private$.last_error))
          ),
          collapse = "\n"
        ),
        caller = self
      )
      # Needs human attention
      .NotYetImplemented()
    },

    #' @description Stop the workflow execution
    stop = function() {
      private$.run_flag <- Sys.time()
      if (self$current_stage != "ready") {
        self$context$logger("Stopping...", caller = self)
        self$current_stage <- "ready"
      }
      # TODO: stop current work
    }
  )
)
