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
