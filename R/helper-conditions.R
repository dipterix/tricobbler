# Event constructors for lifecycle events
#
# Each function builds a plain list (the "event object") that is passed
# to \code{EventDispatcher$emit(event)}. Fields are spread flat on
# the list so handlers use \code{event$state_name}, not
# \code{event$data$state_name}.
#
# The event retains a \code{class} attribute (e.g.
# \code{c("tricobbler_suspend", "tricobbler_lifecycle")}) so that
# \code{inherits()} checks still work in handler code if needed.
#
# Unlike the previous condition-based approach, these constructors
# never call \code{signalCondition()}. Dispatch is handled entirely
# by the direct function-call loop in
# \code{EventDispatcher$emit()}.
#
# @param type character, event type (e.g. "suspend",
#   "state_completed")
# @param message character, human-readable message
# @param ... additional fields spread flat on the event object
# @return A list with class
#   \code{c("tricobbler_<type>", "tricobbler_lifecycle")}.
# @noRd
tricobbler_event <- function(type, message, ...) {
  structure(
    list(type = type, message = message, ...),
    class = c(
      paste0("tricobbler_", type),
      "tricobbler_lifecycle"
    )
  )
}


# Format an error for human-readable logging
#
# Extracts the error message and, if available, an \pkg{rlang}
# backtrace captured at the point of failure. Falls back to
# a single-line \code{call} deparse when no trace is attached.
#
# @param error A condition object, character, or any value.
# @param max_frames integer, maximum number of backtrace frames
#   to display (default 20). Keeps output from becoming
#   overwhelming in deeply nested call stacks.
# @return A single character string ready for logging.
# @noRd
format_error_trace <- function(error, max_frames = 20L) {
  # --- Error message ---
  msg <- if (inherits(error, "condition")) {
    conditionMessage(error)
  } else {
    as.character(error)
  }

  # --- Backtrace ---
  trace_lines <- NULL

  # Prefer rlang trace captured at the error site
  if (!is.null(error$trace)) {
    trace_lines <- tryCatch(
      {
        # rlang::format_trace() returns a character vector;
        # drop = FALSE preserves the full tree
        formatted <- format(error$trace, simplify = "none")
        if (length(formatted) > max_frames) {
          formatted <- c(
            utils::head(formatted, max_frames),
            sprintf("... and %d more frames", length(formatted) - max_frames)
          )
        }
        formatted
      },
      error = function(e2) NULL
    )
  }

  # Fall back: deparse the call stored on the condition
  if (is.null(trace_lines) && inherits(error, "condition") &&
      !is.null(error$call)) {
    trace_lines <- paste("Call:", paste(deparse(error$call), collapse = " "))
  }

  paste(c(msg, trace_lines), collapse = "\n")
}
