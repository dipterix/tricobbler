#' @include mcp-describe.R
#' @include class-s7base.R
#' @include class-baseagent.R
#' @include class-context.R
#' @include class-event-dispatcher.R
NULL

# DIPSAUS DEBUG START
# source("~/Dropbox (Personal)/projects/tricobbler/adhoc/simple-example.R", echo = TRUE)

#' Workflow Execution Scheduler
#'
#' @description R6 class that orchestrates workflow execution by managing stage
#'   progression, state execution, and agent coordination. This is the primary
#'   runtime component in the Runtime Layer (Tier 2) that brings together the
#'   immutable policy definitions (\code{Manifest}) with executable agents.
#'
#' @details
#' The scheduler uses a queue + waiting-pool dispatch model.
#' Within each stage, independent states are dispatched concurrently
#' via \code{AgentRuntime$run_async()}, bounded by
#' \code{max_concurrency}. Stages themselves are executed
#' sequentially.
#'
#' \describe{
#'   \item{\code{start()}}{Blocks until all stages complete (runs the
#'     event loop inline).}
#'   \item{\code{start_async()}}{Returns a promise that resolves when
#'     all stages complete.}
#'   \item{\code{run_stage(stage)}}{Executes a single named stage
#'     using the queue + pool model. Returns a promise
#'     (\code{coro::async}).}
#' }
#'
#' @export
Scheduler <- R6::R6Class(
  classname = "TricobblerScheduler",
  portable = TRUE,
  cloneable = FALSE,
  private = list(
    .run_flag = NULL,
    .last_error = NULL,
    .dispatcher = NULL,
    # Stage promise settlement functions; set by run_stage(),
    # called by advance() when the stage completes or fails
    .stage_resolve = NULL,
    .stage_reject = NULL,
    # Snapshot of .run_flag at stage start; if .run_flag changes
    # (via stop() or a new start()), advance() detects cancellation
    .stage_flag = NULL,

    # Walk the manifest to find same-stage states that depend on
    # `state_name` and mark them as skipped in completed_map.
    # Also removes them from runtime_map if present. Recurses
    # to transitively skip all downstream dependents.
    skip_dependents = function(state_name, stage) {
      for (sp in self$manifest@states) {
        if (!identical(sp@stage, stage)) { next }
        if (self$completed_map$has(sp@name)) { next }
        # Check whether this state depends on the skipped state
        deps <- sp@depends_on@deps
        depends_on_skipped <- any(vapply(deps, function(dep) {
          is_same_stage <- length(dep$stage) == 0L ||
            is.null(dep$stage) ||
            identical(dep$stage, stage)
          is_same_stage && identical(dep$state, state_name)
        }, FALSE))
        if (!depends_on_skipped) { next }

        # Mark as skipped
        self$completed_map$set(
          key = sp@name,
          value = list(
            policy = sp,
            agent = NULL,
            attempt = NA_integer_,
            attachment_id = NA_character_,
            status = "skipped"
          )
        )
        # Remove from runtime_map if queued
        if (self$runtime_map$has(sp@name)) {
          self$runtime_map$remove(sp@name)
        }
        # Remove from ready_queue if present
        remaining <- self$ready_queue$as_list()
        self$ready_queue$reset()
        remaining <- Filter(
          function(rt) !identical(rt$policy@name, sp@name),
          remaining
        )
        if (length(remaining)) {
          self$ready_queue$madd(.list = remaining)
        }

        self$dispatch_event(
          type = "runtime.skipped",
          message = sprintf(
            "State '%s' skipped (depends on skipped '%s')",
            sp@name, state_name
          ),
          state_name = sp@name,
          stage = stage
        )

        # Recurse: skip states that depend on this one too
        private$skip_dependents(sp@name, stage)
      }
    }
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

    # needs human attention? pause all if TRUE
    #' @field suspended logical, whether execution is paused
    suspended = FALSE,

    #' @field suspend_info list or \code{NULL}, context captured when
    #'   the scheduler suspends (state_name, stage, error). Cleared
    #'   on resume/skip.
    suspend_info = NULL,

    #' @field draining logical, when \code{TRUE} a \code{final} state
    #'   has completed; no new runtimes are dispatched but in-flight
    #'   promises are allowed to finish
    draining = FALSE,

    #' @field max_concurrency integer, maximum number of simultaneous
    #'   promises in the waiting pool (default: \code{100L})
    max_concurrency = 100L,

    # all tasks will be stored here before joining the ready_queue
    runtime_map = NULL,

    # ready_queue: list of state names, highest priority first
    #   all to-do tasks will be saved here
    ready_queue = NULL,

    # waiting_pool: named list, state_name -> promise
    #   storing running agents
    waiting_pool = NULL,

    # completed_map: fastmap, state_name -> list(policy, agent,
    #   attempt, attachment_id, status) where status is one of
    #   "finished", "errored", or "skipped"
    completed_map = NULL,

    # retry_map: named list, state_name -> integer attempt count
    #   the agents will be queued in the next cycle (merged to ready_queue)
    retry_map = NULL,


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

      private$.dispatcher <- EventDispatcher$new()

      self$runtime_map <- fastmap::fastmap()
      self$ready_queue <- fastmap::fastqueue()
      self$waiting_pool <- fastmap::fastmap()
      self$completed_map <- fastmap::fastmap()
      self$retry_map <- fastmap::fastmap()

      self$current_stage <- "ready"
      self$stage_started <- Sys.time()

      invisible(self)
    },

    #' @description Register a listener for a lifecycle event
    #' @param type character, event type (e.g. \code{"suspend"},
    #'   \code{"state_completed"}, \code{"stage_completed"},
    #'   \code{"dispatch"})
    #' @param handler function, callback receiving the event list.
    #'   For \code{"suspend"} events the handler may return an action
    #'   string (\code{"resume"}, \code{"skip"}, \code{"abort"}, or
    #'   \code{"restart_stage"}).
    #' @param id character or \code{NULL}, optional listener ID for
    #'   replacement or removal; auto-generated via
    #'   \code{digest::digest(handler)} when \code{NULL}
    #' @param after logical, if \code{TRUE} (default) append the handler
    #'   after existing handlers; if \code{FALSE} prepend it
    #' @return character, the listener ID (invisibly)
    on = function(type, handler, id = NULL, after = TRUE) {
      private$.dispatcher$on(
        type, handler, id = id,
        after = after
      )
    },

    #' @description Remove a registered lifecycle listener by ID
    #' @param id character, the listener ID returned by \code{$on()}
    off = function(id) {
      private$.dispatcher$off(id)
    },

    dispatch_event = function(type, message = type, ...) {
      private$.dispatcher$emit(
        tricobbler_event(type = type,
                         message = message, ...)
      )
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

    #' @description Stop the workflow execution
    stop = function() {
      private$.run_flag <- Sys.time()
      if (self$current_stage != "ready") {
        self$context$logger("Stopping...", caller = self)
        self$current_stage <- "ready"
      }
      # TODO: stop current work
    },

    #' @description Start the workflow execution
    #' @return A \code{promise} that resolves when all stages complete.
    #'   Callers may block on the result via \code{later::run_now()}
    #'   or \code{await()} inside a \code{coro::async} context.
    start = function() {
      if (!identical(self$current_stage, "ready")) {
        stop(
          "Current state is `",
          self$current_stage,
          "`. Please call `scheduler$stop()` to reset the scheduler first."
        )
      }

      self$validate(on_error = "error")
      self$runtime_map$reset()


      self$suspended <- FALSE
      private$.last_error <- NULL
      private$.run_flag <- Sys.time()

      self$current_stage <- "ready"
      self$init_resources()

      stages <- self$manifest@master@stages

      impl <- async(function() {
        for (stage in stages) {
          if (!identical(private$.run_flag, private$.stage_flag) &&
              !is.null(private$.stage_flag)) {
            stop("Scheduler cancelled; aborting.")
          }

          await(self$run_stage(stage))
        }
        invisible()
      })

      impl()$then(
        onFulfilled = function(...) {
          message("Done.")
        },
        onRejected = function(e) {
          msg <- conditionMessage(e)
          message(msg)
          message("Abort.")
        }
      )
    },

    #' @description Initialize resources and prepare for execution
    #' @param reset_context logical, whether to reset the context storage
    init_resources = function(reset_context = FALSE) {
      # TODO: initialize MCP tools; initialize context
      self$context$set_scheduler(self)
      self$context$init_resources()

      self$context$logger("Initializing resources", caller = self)
      self$dispatch_event(type = "init_resources",
                          message = "Initializing resources")

    },


    init_stage = function(stage) {

      self$dispatch_event(
        type = "init_stage.begin",
        message = sprintf("Initializing stage `%s`", stage),
        stage = stage
      )

      if (stage == "ready") { return() }

      # --- Build policy map and initial data structures ---
      state_policies <- extract_manifest_state(
        self$manifest, stage = stage
      )

      # initial runtimes
      lapply(state_policies, function(policy) {
        # policy <- state_policies[[1]]
        agent <- self$agents$get(policy@agent_id)
        self$runtime_map$set(
          policy@name,
          AgentRuntime$new(
            agent = agent,
            context = self$context,
            policy = policy,
            attempt = 0L
          )
        )
        return()
      })

      self$dispatch_event(
        type = "init_stage.end",
        message = sprintf("Initialized stage `%s`: %d runtime created",
                          stage, length(state_policies)),
        stage = stage
      )

      return()
    },

    # cycle 1: check the runtimes from `self$runtime_map`, extract ready-states
    # als
    enqueue_runtime = function() {

      # Don't enqueue new runtimes while draining (a final state
      # has completed); in-flight promises will finish on their own
      if (self$draining || self$suspended) {
        return(invisible(FALSE))
      }

      # TODO: check suspended

      self$dispatch_event(
        type = "enqueue_runtime.begin"
      )

      state_keys <- self$runtime_map$keys()
      state_cleared <- vapply(state_keys, function(state_key) {
        runtime <- self$runtime_map$get(state_key)
        deps <- runtime$policy@depends_on@deps
        deps_resolved <- vapply(deps, function(dep) {
          if (length(dep$stage) == 1 &&
              isTRUE(dep$stage != runtime$policy@stage)) {
            # other stages
            # TODO: check if stage is done as well
            return(TRUE)
          }
          return(self$completed_map$has(dep$state))
        }, FUN.VALUE = FALSE)
        all(deps_resolved)
      }, FALSE)

      changed <- FALSE
      if (any(state_cleared)) {

        queued_keys <- state_keys[state_cleared]
        all_runtimes <- c(self$runtime_map$mget(queued_keys),
                          self$ready_queue$as_list())

        priorities <- vapply(all_runtimes, function(runtime) {
          runtime$policy@priority
        }, FUN.VALUE = 0L)

        all_runtimes <- all_runtimes[order(priorities, decreasing = TRUE)]

        self$ready_queue$reset()

        self$ready_queue$madd(.list = all_runtimes)
        self$runtime_map$remove(queued_keys)

        self$dispatch_event(
          type = "enqueue_runtime.changed"
        )

        changed <- TRUE
      }

      self$dispatch_event(
        type = "enqueue_runtime.end"
      )

      return(invisible(changed))
    },

    execute_runtime = function() {

      if (self$suspended || self$draining) {
        return(invisible(0L))
      }

      # return number of newly started runtime
      if (self$ready_queue$size() == 0) { return(invisible(0L)) }

      available_slots <- self$max_concurrency - self$waiting_pool$size()
      if (available_slots <= 0) { return(invisible(0L)) }

      # --- Critical-state priority barrier ---
      # Peek at all queued items; if any is critical, only dispatch
      # states at priority >= the highest critical state's priority.
      # Lower-priority states are pushed back to runtime_map so
      # they re-enter enqueue_runtime() after the critical state
      # settles.
      all_queued <- self$ready_queue$as_list()
      self$ready_queue$reset()

      critical_barrier <- -Inf
      for (rt in all_queued) {
        if (isTRUE(rt$policy@critical)) {
          critical_barrier <- max(critical_barrier, rt$policy@priority)
        }
      }

      if (is.finite(critical_barrier)) {
        dispatch_list <- list()
        hold_back <- list()
        for (rt in all_queued) {
          if (rt$policy@priority >= critical_barrier) {
            dispatch_list <- c(dispatch_list, list(rt))
          } else {
            hold_back <- c(hold_back, list(rt))
          }
        }
        # Put dispatchable items back in ready_queue
        if (length(dispatch_list)) {
          self$ready_queue$madd(.list = dispatch_list)
        }
        # Move held-back items to runtime_map (they'll be
        # re-enqueued by the next advance() cycle)
        for (rt in hold_back) {
          self$runtime_map$set(rt$policy@name, rt)
        }
      } else {
        # No critical barrier; re-add all
        if (length(all_queued)) {
          self$ready_queue$madd(.list = all_queued)
        }
      }

      if (self$ready_queue$size() == 0) { return(invisible(0L)) }

      available_slots <- min(available_slots, self$ready_queue$size())

      scheduled_names <- lapply(seq_len(available_slots), function(ii) {
        runtime <- self$ready_queue$remove()

        self$dispatch_event(
          type = "runtime.dispatch",
          message = sprintf(
            "Dispatching state '%s' (attempt %d)",
            runtime$policy@name, runtime$attempt
          ),
          state_name = runtime$policy@name,
          stage = runtime$policy@stage,
          attempt = runtime$attempt
        )

        promise <- runtime$run_async()
        promise <- promise$then(
          onFulfilled = function(result) {

            succeed <- result$succeed
            state_name <- runtime$policy@name
            policy <- runtime$policy

            # only save the runtime summary rather than the result
            runtime_summary <- list(
              policy = policy,
              agent = runtime$agent,
              attempt = runtime$attempt,
              attachment_id = runtime$attachment_id
            )

            self$waiting_pool$remove(state_name)

            if (succeed) {
              runtime_summary$status <- "finished"
              self$completed_map$set(
                key = state_name,
                value = runtime_summary
              )

              self$dispatch_event(
                type = "runtime.resolved",
                message = sprintf(
                  "State '%s' completed (success)", state_name
                ),
                runtime = runtime,
                result = result
              )

              # Final flag: stop dispatching new runtimes, let
              # in-flight promises drain
              if (isTRUE(policy@final)) {
                self$draining <- TRUE
                self$ready_queue$reset()
                self$dispatch_event(
                  type = "runtime.final",
                  message = sprintf(
                    "Final state '%s' completed; draining",
                    state_name
                  ),
                  state_name = state_name
                )
              }
            } else {
              # --- Failure handling ---
              max_retry <- max(policy@max_retry, 0L)

              if (runtime$attempt >= max_retry) {
                # Retries exhausted

                self$dispatch_event(
                  type = "runtime.exhausted",
                  message = sprintf(
                    "State '%s' exhausted retries (%d/%d)",
                    state_name, runtime$attempt, max_retry
                  ),
                  runtime = runtime,
                  result = result
                )

                if (isTRUE(policy@critical)) {
                  private$.last_error <- result$error
                  # suspend() returns the chosen action; do NOT
                  # add to completed_map before the user decides
                  suspend_action <- self$suspend(
                    error = result$error,
                    state_name = state_name,
                    stage = policy@stage,
                    runtime_summary = runtime_summary
                  )
                  # suspend() already called advance() or ended
                  # the stage; skip the trailing advance()
                  return(invisible())
                } else {
                  # Non-critical exhausted: mark as errored
                  runtime_summary$status <- "errored"
                  self$completed_map$set(
                    key = state_name,
                    value = runtime_summary
                  )
                }
              } else if (!is.na(policy@on_failure)) {
                # Redirect to on_failure target
                target <- policy@on_failure
                if (
                  !self$completed_map$has(target) &&
                    !self$waiting_pool$has(target)
                ) {
                  # Create a runtime for the target state
                  target_policy <- NULL
                  for (sp in self$manifest@states) {
                    if (identical(sp@name, target)) {
                      target_policy <- sp
                      break
                    }
                  }
                  if (!is.null(target_policy)) {
                    target_agent <- self$agents$get(
                      target_policy@agent_id
                    )
                    # TODO: need to check if the on_failure depends on the current runtime when manifest is created
                    self$runtime_map$set(
                      target,
                      AgentRuntime$new(
                        agent = target_agent,
                        context = self$context,
                        policy = target_policy,
                        attempt = 0L
                      )
                    )
                  }
                }

                self$dispatch_event(
                  type = "runtime.redirect",
                  message = sprintf(
                    "State '%s' failed; redirecting to '%s'",
                    state_name, target
                  ),
                  runtime = runtime,
                  result = result,
                  target = target
                )
              } else {
                # Re-queue for retry
                self$retry_map$set(
                  key = state_name,
                  value = runtime_summary
                )

                self$dispatch_event(
                  type = "runtime.errored",
                  message = sprintf(
                    "State '%s' failed (attempt %d/%d); will retry",
                    state_name, runtime$attempt, max_retry
                  ),
                  runtime = runtime,
                  result = result
                )
              }
            }

            # Event-driven: this resolution triggers the next cycle
            self$advance()
          },
          onRejected = function(e) {
            # TODO: suspend as this is unexpected

            # Event-driven: this resolution triggers the next cycle
            self$advance()
          }
        )
        self$waiting_pool$set(runtime$policy@name, list(
          runtime = runtime,
          promise = promise
        ))
        return(runtime$policy@name)
      })

      return(invisible(available_slots))

    },

    retry_runtime = function() {
      if (self$retry_map$size() == 0) { return(invisible()) }

      retry_keys <- self$retry_map$keys()

      lapply(retry_keys, function(key) {

        # runtime_summary <- list(
        #   policy = runtime$policy,
        #   agent = runtime$agent,
        #   attempt = runtime$attempt,
        #   attachment_id = runtime$attachment_id
        # )
        #
        runtime_summary <- self$retry_map$get(key)

        runtime <- AgentRuntime$new(
          agent = runtime_summary$agent,
          context = self$context,
          policy = runtime_summary$policy,
          attempt = runtime_summary$attempt + 1L
        )

        self$runtime_map$set(runtime_summary$policy@name, runtime)

        self$retry_map$remove(key)

        return()

      })

      return(invisible(retry_keys))
    },

    start_stage = function(stage) {

      # stage <- "triage"

      if (missing(stage) || length(stage) != 1L || !nzchar(stage)) {
        stop("run_stage() requires an explicit stage name.")
      }
      all_stages <- c("ready", self$manifest@master@stages)
      stage <- match.arg(stage, all_stages, several.ok = FALSE)

      if (self$suspended) {
        stop(
          "Unable to run stage `", stage,
          "` because the progress is suspended."
        )
      }

      # Start the stage
      self$current_stage <- stage
      self$stage_started <- Sys.time()
      self$context$logger(
        "current stage: ", self$current_stage, caller = self
      )

      if (stage == "ready") {
        # Return an already-resolved promise for consistency
        return(invisible())
      }

      # Reset per-stage data structures
      self$runtime_map$reset()
      self$ready_queue$reset()
      self$waiting_pool$reset()
      self$completed_map$reset()
      self$retry_map$reset()
      self$draining <- FALSE

      # Initialize runtimes and seed the ready queue
      self$init_stage(stage)
      self$enqueue_runtime()

      return(invisible())
    },

    #' @description Count incomplete work items for the current stage
    #' @return integer, total items across runtime_map, ready_queue,
    #'   waiting_pool, and retry_map
    get_incomplete_size = function() {
      self$runtime_map$size() +
        self$ready_queue$size() +
        self$waiting_pool$size() +
        self$retry_map$size()
    },

    #' @description Drive the next dispatch cycle (event-driven)
    #' @details Called automatically when a runtime promise resolves.
    #'   Runs the cycle: \code{retry_runtime} -> \code{enqueue_runtime}
    #'   -> \code{execute_runtime}. If no incomplete work remains,
    #'   resolves the stage promise and emits \code{stage.completed}.
    advance = function() {
      # Don't drive if suspended — suspend() will call advance()
      # again if the user chooses resume or skip
      if (isTRUE(self$suspended)) {
        return(invisible())
      }

      # Check cancellation: if .run_flag changed since the stage
      # started, this stage was cancelled (stop() or new start())
      if (!identical(private$.run_flag, private$.stage_flag)) {
        reject <- private$.stage_reject
        private$.stage_resolve <- NULL
        private$.stage_reject <- NULL
        if (is.function(reject)) {
          reject("Scheduler cancelled; aborting stage.")
        }
        return(invisible())
      }

      # Drive the cycle
      self$retry_runtime()
      self$enqueue_runtime()
      self$execute_runtime()

      # Check completion: either all work is done, or we are
      # draining (final state fired) and all in-flight promises
      # have settled
      stage_done <- FALSE
      if (self$get_incomplete_size() == 0L) {
        stage_done <- TRUE
      } else if (isTRUE(self$draining) &&
                 self$waiting_pool$size() == 0L) {
        # Draining complete: abandon any items still queued
        # (they were blocked by the final gate)
        self$runtime_map$reset()
        self$ready_queue$reset()
        self$retry_map$reset()
        stage_done <- TRUE
      }

      if (stage_done && is.function(private$.stage_resolve)) {
        stage <- self$current_stage
        self$dispatch_event(
          type = "stage.completed",
          message = sprintf("Stage '%s' completed", stage),
          stage = stage
        )
        resolve <- private$.stage_resolve
        private$.stage_resolve <- NULL
        private$.stage_reject <- NULL
        resolve(invisible(self))
      }
    },

    #' @description Execute a single workflow stage using the
    #'   queue + waiting-pool dispatch model
    #' @param stage character, the stage name to execute
    #' @return A promise that resolves when all states in the stage
    #'   have completed
    run_stage = function(stage) {

      self$start_stage(stage)

      # "ready" stage has nothing to run
      if (stage == "ready") {
        return(promises::promise_resolve(invisible(self)))
      }

      # Snapshot the run flag so advance() can detect cancellation
      private$.stage_flag <- private$.run_flag

      # Return a promise; settlement is driven by advance(),
      # which is called from execute_runtime()'s promise callbacks
      promises::promise(function(resolve, reject) {
        private$.stage_resolve <- resolve
        private$.stage_reject <- reject

        # Kick off the first dispatch; subsequent cycles are
        # triggered by promise resolution -> advance()
        self$execute_runtime()

        # If everything resolved synchronously (empty stage)
        if (self$get_incomplete_size() == 0L) {
          self$dispatch_event(
            type = "stage.completed",
            message = sprintf("Stage '%s' completed", stage),
            stage = stage
          )
          private$.stage_resolve <- NULL
          private$.stage_reject <- NULL
          resolve(invisible(self))
        }
      })
    },

    #' @description Suspend the workflow execution
    #' @param error condition or character, the error that caused
    #'   suspension (default: \code{private$.last_error})
    #' @param state_name character, the state that caused suspension
    #' @param stage character, the stage in which suspension occurred
    #'   (default: \code{self$current_stage})
    #' @param runtime_summary list or \code{NULL}, lightweight summary
    #'   of the runtime that failed (policy, agent, attempt,
    #'   attachment_id). Used by \code{"resume"} to re-create the
    #'   runtime and by \code{"skip"} to record the skipped state.
    #' @return character, the chosen action (invisibly)
    suspend = function(error = NULL, state_name = NA_character_,
                       stage = self$current_stage,
                       runtime_summary = NULL) {
      self$suspended <- TRUE
      if (is.null(error)) {
        error <- private$.last_error
      }

      # Capture context for inspection / event handlers
      self$suspend_info <- list(
        state_name = state_name,
        stage = stage,
        error = error,
        timestamp = Sys.time()
      )

      self$context$logger(
        "scheduler suspended at `",
        stage, " -> ", state_name,
        "` due to a critical failure. ",
        "Human attention needed. \nLast error: \n",
        paste0(
          c(
            local({
              if (inherits(error, "condition")) {
                conditionMessage(error)
              } else {
                as.character(error)
              }
            }),
            utils::capture.output(traceback(error))
          ),
          collapse = "\n"
        ),
        caller = self
      )

      # Emit suspend event via direct-call dispatch; if a
      # registered listener returns an action string (e.g.
      # "resume", "skip", "abort", "restart_stage"), it is
      # captured as the first non-NULL handler return value.
      action <- self$dispatch_event(
        type = "suspend",
        message = sprintf(
          "Scheduler suspended at stage '%s', state '%s'",
          stage, state_name
        ),
        state_name = state_name,
        stage = stage,
        error = error
      )

      # If no listener returned an action, fall back to default
      # behavior: interactive menu or abort
      if (is.null(action)) {
        if (interactive()) {
          choice <- utils::menu(
            choices = c("Retry state", "Skip", "Abort",
                        "Restart stage"),
            title = paste0(
              "Scheduler suspended at stage '",
              stage, "', state '", state_name,
              "'. Choose action:"
            )
          )
          action <- c("resume", "skip", "abort", "restart_stage")[
            if (choice == 0L) 3L else choice
          ]
        } else {
          action <- "abort"
        }
      }

      # Execute the chosen action
      switch(
        action,
        "resume" = {
          self$suspended <- FALSE
          self$suspend_info <- NULL
          # Re-add the state to runtime_map with attempt=0 so it
          # gets retried in the next dispatch cycle
          if (!is.null(runtime_summary)) {
            runtime <- AgentRuntime$new(
              agent = runtime_summary$agent,
              context = self$context,
              policy = runtime_summary$policy,
              attempt = 0L
            )
            self$runtime_map$set(state_name, runtime)
          }
          self$advance()
        },
        "skip" = {
          self$suspended <- FALSE
          self$suspend_info <- NULL
          # Mark the failed state as skipped in completed_map
          if (!is.null(runtime_summary)) {
            runtime_summary$status <- "skipped"
            self$completed_map$set(
              key = state_name,
              value = runtime_summary
            )
          }
          # Transitively skip all same-stage dependents
          private$skip_dependents(state_name, stage)
          self$advance()
        },
        "restart_stage" = {
          self$suspended <- FALSE
          self$suspend_info <- NULL
          # Re-initialize the entire stage from scratch
          self$start_stage(stage)
          # Re-snapshot the run flag for the restarted stage
          private$.stage_flag <- private$.run_flag
          self$advance()
        },
        "abort" = {
          self$suspended <- FALSE
          self$suspend_info <- NULL
          # Reject the stage promise so start() surfaces the error
          reject <- private$.stage_reject
          private$.stage_resolve <- NULL
          private$.stage_reject <- NULL
          msg <- sprintf("Scheduler aborted at stage '%s', state '%s'",
                         stage, state_name)
          if (is.function(reject)) {
            reject(msg)
          } else {
            stop(msg)
          }
        }
      )

      invisible(action)
    }
  )
)
