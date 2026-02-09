# Helper for scheduler tests: shared setup for sync and async scheduler tests

# Drain the async event loop (for async scheduler tests)
drain_async <- function(timeout = 5) {
  deadline <- Sys.time() + timeout
  while (!later::loop_empty()) {
    if (Sys.time() > deadline) {
      warning("drain_async: timeout reached")
      break
    }
    later::run_now(timeoutSecs = 0.2)
  }
}

# Sabotage record_attachment on a context for a given state_name
# so that .record_result() throws (simulating disk failure).
# Returns an environment with $count tracking how many times it fired.
sabotage_record_attachment <- function(scheduler, target_state) {
  ctx <- scheduler$context
  ctx_self <- ctx$.__enclos_env__$self
  original_record <- ctx$record_attachment
  tracker <- new.env(parent = emptyenv())
  tracker$count <- 0L

  unlockBinding("record_attachment", ctx_self)
  ctx_self$record_attachment <- function(runtime, ...) {
    if (identical(runtime$policy@name, target_state)) {
      tracker$count <- tracker$count + 1L
      stop("Simulated disk failure in record_attachment!")
    }
    original_record(runtime, ...)
  }
  tracker
}
