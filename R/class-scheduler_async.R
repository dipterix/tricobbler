#' @include class-scheduler_sync.R
NULL

#' \verb{Async} Workflow Execution Scheduler
#'
#' @description R6 class that extends \code{\link{Scheduler}} with
#'   promise-based asynchronous execution. Within each stage,
#'   independent states are dispatched concurrently via
#'   \code{AgentRuntime$run_async()}, bounded by
#'   \code{max_concurrency}. Stages are executed sequentially.
#'
#' @details
#' ## \verb{Async} Dispatch Cycle
#'
#' Each stage follows a promise-driven dispatch loop:
#' \enumerate{
#'   \item \code{start()} validates the manifest, initializes resources,
#'     and iterates through stages sequentially using
#'     \code{coro::async}
#'   \item \code{run_stage(stage)} initializes runtimes for all state
#'     policies in the stage, creates a stage-level promise, and calls
#'     \code{advance()}
#'   \item \code{advance()} drives the cycle: retry failed runtimes,
#'     \verb{enqueue} ready runtimes, then dispatch up to
#'     \code{max_concurrency} concurrent executions
#'   \item When a runtime settles (fulfills or rejects), its promise
#'     callback processes the result (success, retry, redirect, or
#'     suspend) and calls \code{advance()} again
#'   \item When no incomplete work remains, \code{advance()} resolves
#'     the stage promise and the next stage begins
#' }
#'
#' ## Differences from \code{\link{Scheduler}}
#'
#' \itemize{
#'   \item \code{start()} returns a \code{promises::promise} instead
#'     of blocking
#'   \item \code{execute_runtime()} dispatches multiple runtimes
#'     concurrently (up to \code{max_concurrency})
#'   \item \code{stop()} rejects the active stage promise in addition
#'     to resetting state
#'   \item \code{suspend()} resolves actions via the event dispatcher
#'     and manipulates promise settlement functions
#' }
#'
#' @export
AsyncScheduler <- R6::R6Class(
  classname = "TricobblerAsyncScheduler",
  portable = TRUE,
  cloneable = FALSE,
  inherit = Scheduler,
  private = list(
    # Stage promise settlement functions; set by run_stage(),
    # called by advance() when the stage completes or fails
    .stage_resolve = NULL,
    .stage_reject = NULL
  ),
  public = list(

    #' @description Stop the workflow execution, clearing all
    #'   in-progress work and rejecting the active stage promise.
    #'   After calling \code{stop()}, the scheduler is in the
    #'   \code{"ready"} state and \code{start()} may be called
    #'   immediately.
    stop = function() {
      # Capture reject before parent clears state
      reject <- private$.stage_reject
      private$.stage_resolve <- NULL
      private$.stage_reject <- NULL

      # Parent handles: run_flag, current_stage, suspension,
      # data structure resets, stage_flag, event emission
      super$stop()

      # Reject the active stage promise so the async loop
      # surfaces the cancellation immediately
      if (is.function(reject)) {
        reject(simpleError("Scheduler stopped by user."))
      }

      invisible(self)
    },

    #' @description Start the workflow execution
    #' @return A \code{promise} that resolves when all stages
    #'   complete. Callers may block via \code{later::run_now()}
    #'   or \code{await()} inside a \code{coro::async} context.
    start = function() {
      if (!identical(self$current_stage, "ready")) {
        stop(
          "Current state is `",
          self$current_stage,
          "`. Please call `scheduler$stop()` to reset the ",
          "scheduler first."
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

      impl <- coro::async(function() {
        for (stage in stages) {
          if (!identical(private$.run_flag,
                         private$.stage_flag) &&
              !is.null(private$.stage_flag)) {
            stop("Scheduler cancelled; aborting.")
          }
          coro::await(self$run_stage(stage))
        }
        invisible()
      })

      impl()$then(
        onFulfilled = function(...) {
          self$current_stage <- "ready"
          self$dispatch_event(
            type = "scheduler.completed",
            message = "All stages completed"
          )
          invisible(self)
        },
        onRejected = function(e) {
          self$current_stage <- "ready"
          msg <- conditionMessage(e)
          self$dispatch_event(
            type = "scheduler.aborted",
            message = sprintf("Scheduler aborted: %s", msg),
            error = e
          )
          promises::promise_reject(e)
        }
      )
    },

    #' @description Dispatch queued runtimes as \verb{async} promises.
    #' @return integer, number of runtimes dispatched (invisibly)
    execute_runtime = function() {

      if (self$suspended || self$draining) {
        return(invisible(0L))
      }

      if (self$ready_queue$size() == 0) {
        return(invisible(0L))
      }

      available_slots <- self$max_concurrency -
        self$waiting_pool$size()
      if (available_slots <= 0) {
        return(invisible(0L))
      }

      # --- Critical-state priority barrier ---
      all_queued <- self$ready_queue$as_list()
      self$ready_queue$reset()

      critical_barrier <- -Inf
      for (rt in all_queued) {
        if (isTRUE(rt$policy@critical)) {
          critical_barrier <- max(
            critical_barrier, rt$policy@priority
          )
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
        if (length(dispatch_list)) {
          self$ready_queue$madd(.list = dispatch_list)
        }
        for (rt in hold_back) {
          self$runtime_map$set(rt$policy@name, rt)
        }
      } else {
        if (length(all_queued)) {
          self$ready_queue$madd(.list = all_queued)
        }
      }

      if (self$ready_queue$size() == 0) {
        return(invisible(0L))
      }

      available_slots <- min(
        available_slots, self$ready_queue$size()
      )

      scheduled_names <- lapply(
        seq_len(available_slots),
        function(ii) {
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
          dispatch_run_flag <- private$.run_flag

          promise <- promise$then(
            onFulfilled = function(result) {

              # Guard: stale callback after stop()/restart
              if (!identical(dispatch_run_flag,
                             private$.run_flag)) {
                return(invisible())
              }

              succeed <- result$succeed
              state_name <- runtime$policy@name
              policy <- runtime$policy

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
                    "State '%s' completed (success)",
                    state_name
                  ),
                  runtime = runtime,
                  result = result
                )

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
                    self$suspend(
                      error = result$error,
                      state_name = state_name,
                      stage = policy@stage,
                      runtime_summary = runtime_summary
                    )
                    return(invisible())
                  } else {
                    runtime_summary$status <- "errored"
                    self$completed_map$set(
                      key = state_name,
                      value = runtime_summary
                    )
                  }
                } else if (!is.na(policy@on_failure)) {
                  target <- policy@on_failure

                  if (!self$completed_map$has(state_name)) {
                    runtime_summary$status <- "errored"
                    self$completed_map$set(
                      key = state_name,
                      value = runtime_summary
                    )
                  }

                  if (!self$completed_map$has(target) &&
                      !self$waiting_pool$has(target)) {
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

              # Event-driven: trigger next dispatch cycle
              self$advance()
            },
            onRejected = function(e) {
              # Guard: stale callback after stop()/restart
              if (!identical(dispatch_run_flag,
                             private$.run_flag)) {
                return(invisible())
              }

              state_name <- runtime$policy@name
              self$waiting_pool$remove(state_name)

              runtime_summary <- list(
                policy = runtime$policy,
                agent = runtime$agent,
                attempt = runtime$attempt,
                attachment_id = runtime$attachment_id,
                status = "errored"
              )
              self$completed_map$set(
                key = state_name,
                value = runtime_summary
              )

              self$dispatch_event(
                type = "runtime.errored",
                message = sprintf(
                  "State '%s' failed unexpectedly: %s",
                  state_name, conditionMessage(e)
                ),
                state_name = state_name,
                stage = runtime$policy@stage,
                error = e
              )

              self$advance()
            }
          )
          self$waiting_pool$set(runtime$policy@name, list(
            runtime = runtime,
            promise = promise
          ))
          return(runtime$policy@name)
        }
      )

      return(invisible(available_slots))
    },

    #' @description Drive the next \verb{async} dispatch cycle.
    #' @details Called by promise callbacks when a runtime settles.
    #'   Runs: retry, \verb{enqueue}, then execute. If no incomplete work
    #'   remains, resolves the stage promise.
    advance = function() {
      if (isTRUE(self$suspended)) {
        return(invisible())
      }

      # Cancellation check
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

      # Check completion
      stage_done <- FALSE
      if (self$get_incomplete_size() == 0L) {
        stage_done <- TRUE
      } else if (isTRUE(self$draining) &&
                 self$waiting_pool$size() == 0L) {
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

    #' @description Execute a single workflow stage asynchronously.
    #' @param stage character, the stage name to execute
    #' @return A promise that resolves when all states in the stage
    #'   have completed
    run_stage = function(stage) {

      self$start_stage(stage)

      if (stage == "ready") {
        return(promises::promise_resolve(invisible(self)))
      }

      private$.stage_flag <- private$.run_flag

      promises::promise(function(resolve, reject) {
        private$.stage_resolve <- resolve
        private$.stage_reject <- reject
        self$advance()
      })
    },

    #' @description Suspend the workflow execution (\verb{async} version).
    #' @param error condition or character, the error that caused
    #'   suspension
    #' @param state_name character, the state that caused suspension
    #' @param stage character, the stage in which suspension occurred
    #' @param runtime_summary list or \code{NULL}, lightweight
    #'   summary of the failed runtime
    #' @return character, the chosen action (invisibly)
    suspend = function(error = NULL, state_name = NA_character_,
                       stage = self$current_stage,
                       runtime_summary = NULL) {
      self$suspended <- TRUE
      if (is.null(error)) {
        error <- private$.last_error
      }

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
          action <- c("resume", "skip", "abort",
                      "restart_stage")[
            if (choice == 0L) 3L else choice
          ]
        } else {
          action <- "abort"
        }
      }

      switch(
        action,
        "resume" = {
          self$suspended <- FALSE
          self$suspend_info <- NULL
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
          if (!is.null(runtime_summary)) {
            runtime_summary$status <- "skipped"
            self$completed_map$set(
              key = state_name,
              value = runtime_summary
            )
          }
          private$skip_dependents(state_name, stage)
          self$advance()
        },
        "restart_stage" = {
          self$suspended <- FALSE
          self$suspend_info <- NULL
          self$start_stage(stage)
          private$.stage_flag <- private$.run_flag
          self$advance()
        },
        "abort" = {
          self$suspended <- FALSE
          self$suspend_info <- NULL
          reject <- private$.stage_reject
          private$.stage_resolve <- NULL
          private$.stage_reject <- NULL
          msg <- sprintf(
            "Scheduler aborted at stage '%s', state '%s'",
            stage, state_name
          )
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
