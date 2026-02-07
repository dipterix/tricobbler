# Base condition constructor for lifecycle events
#
# Fields are spread flat on the condition object so handlers use
# cond$state_name, not cond$data$state_name.
#
# @param type character, condition type (e.g. "suspend", "state_completed")
# @param message character, human-readable message
# @param ... additional fields spread flat on the condition object
# @param signal logical, whether to signal the condition immediately
#   via \code{signalCondition()} (default: \code{TRUE}). Set to
#   \code{FALSE} to construct the condition without signaling.
# @param call call object or NULL
# @return The condition object (invisibly when \code{signal = TRUE}).
# @noRd
tricobbler_condition <- function(type, message, ..., signal = TRUE,
                                 call = NULL) {
  cond <- structure(
    class = c(
      paste0("tricobbler_", type),
      "tricobbler_lifecycle",
      "condition"
    ),
    list(message = message, ..., call = call)
  )
  if (signal) {
    signalCondition(cond)
    return(invisible(cond))
  }
  cond
}

# Lifecycle condition: suspend
#
# Signaled when a critical state has failed and retries are exhausted.
# Restarts: tricobbler_resume, tricobbler_skip, tricobbler_abort,
#   tricobbler_restart_stage
#
# @param scheduler Scheduler instance
# @param state_name character, the state that caused suspension
# @param stage character, the stage in which suspension occurred
# @param error condition or character, the error that caused suspension
# @param signal logical, whether to signal immediately (default: \code{TRUE})
# @return A condition of class \code{tricobbler_suspend}
# @noRd
tricobbler_suspend <- function(scheduler, state_name, stage, error,
                               signal = TRUE) {
  msg <- sprintf(
    "Scheduler suspended at stage '%s', state '%s': %s",
    stage, state_name,
    if (inherits(error, "condition")) {
      conditionMessage(error)
    } else {
      as.character(error)
    }
  )
  tricobbler_condition(
    "suspend",
    message = msg,
    scheduler = scheduler,
    state_name = state_name,
    stage = stage,
    error = error,
    signal = signal
  )
}

# Lifecycle condition: state_completed
#
# Signaled when a state finishes execution (success or failure).
# Informational only - no restarts.
#
# @param scheduler Scheduler instance
# @param state_name character, the completed state
# @param stage character, the stage containing the state
# @param succeed logical, whether the state succeeded
# @param attachment_id character, the attachment ID for this execution
# @param signal logical, whether to signal immediately (default: \code{TRUE})
# @return A condition of class \code{tricobbler_state_completed}
# @noRd
tricobbler_state_completed <- function(scheduler, state_name, stage,
                                       succeed, attachment_id,
                                       signal = TRUE) {
  msg <- sprintf(
    "State '%s' in stage '%s' completed (%s)",
    state_name, stage,
    if (succeed) "success" else "failure"
  )
  tricobbler_condition(
    "state_completed",
    message = msg,
    scheduler = scheduler,
    state_name = state_name,
    stage = stage,
    succeed = succeed,
    attachment_id = attachment_id,
    signal = signal
  )
}

# Lifecycle condition: stage_completed
#
# Signaled when all states in a stage have finished.
# Informational only - no restarts.
#
# @param scheduler Scheduler instance
# @param stage character, the completed stage
# @param signal logical, whether to signal immediately (default: \code{TRUE})
# @return A condition of class \code{tricobbler_stage_completed}
# @noRd
tricobbler_stage_completed <- function(scheduler, stage, signal = TRUE) {
  msg <- sprintf("Stage '%s' completed", stage)
  tricobbler_condition(
    "stage_completed",
    message = msg,
    scheduler = scheduler,
    stage = stage,
    signal = signal
  )
}

# Lifecycle condition: dispatch
#
# Signaled when a state is about to be dispatched for execution.
# Informational only - no restarts.
#
# @param scheduler Scheduler instance
# @param state_name character, the state being dispatched
# @param stage character, the current stage
# @param attempt integer, the attempt number (0-based)
# @param signal logical, whether to signal immediately (default: \code{TRUE})
# @return A condition of class \code{tricobbler_dispatch}
# @noRd
tricobbler_dispatch <- function(scheduler, state_name, stage, attempt,
                                signal = TRUE) {
  msg <- sprintf(
    "Dispatching state '%s' in stage '%s' (attempt %d)",
    state_name, stage, attempt
  )
  tricobbler_condition(
    "dispatch",
    message = msg,
    scheduler = scheduler,
    state_name = state_name,
    stage = stage,
    attempt = attempt,
    signal = signal
  )
}
