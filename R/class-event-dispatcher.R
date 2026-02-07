#' @include helper-conditions.R
NULL

# Internal event dispatcher for lifecycle conditions
#
# Provides JS-like ordered event handling. Handlers execute in registration
# order by default (\code{after = TRUE} appends, \code{after = FALSE}
# prepends), similar to \code{on.exit(after = TRUE)}.
#
# Uses \code{fastmap::fastmap()} for type-level and reverse-lookup maps, but
# each type's handlers are stored in a plain list to guarantee deterministic,
# sequential execution order.
#
# \code{digest::digest()} auto-generates deterministic IDs from handler
# functions, preventing duplicate registration of the same closure.
# User-supplied \code{id} enables explicit replacement (upsert semantics).
#
# Handlers registered with \code{auto_free = TRUE} are automatically removed
# at the end of \code{wrap()} (via \code{on.exit}), even if the wrapped
# expression throws an error. This prevents handler leaks from agents that
# register transient listeners during execution.
#
# @noRd
EventDispatcher <- R6::R6Class(
  classname = "TricobblerEventDispatcher",
  portable = TRUE,
  cloneable = FALSE,
  private = list(
    # Registry: fastmap of lists
    # Top-level keys = event types (e.g. "suspend", "state_completed")
    # Each value = list of entries in execution order
    # Entry: list(id, handler, auto_free)
    .listeners = NULL,
    # Reverse lookup: id -> character vector of types
    # (same handler can be registered to multiple types)
    .id_to_types = NULL,
    # Ensure a per-type list exists
    .ensure_type = function(type) {
      if (!private$.listeners$has(type)) {
        private$.listeners$set(type, list())
      }
    },
    # Release all handlers on GC
    finalize = function() {
      self$clear()
    }
  ),
  public = list(
    initialize = function() {
      private$.listeners <- fastmap::fastmap()
      private$.id_to_types <- fastmap::fastmap()
    },

    # Release all handlers (called by finalize and available publicly)
    # @param type character or NULL; if provided, clear only that type
    clear = function(type = NULL) {
      if (!is.null(type)) {
        if (private$.listeners$has(type)) {
          # Remove reverse-lookup entries for IDs in this type
          entries <- private$.listeners$get(type)
          for (entry in entries) {
            registered <- private$.id_to_types$get(entry$id)
            registered <- registered[registered != type]
            if (length(registered) > 0L) {
              private$.id_to_types$set(entry$id, registered)
            } else {
              private$.id_to_types$remove(entry$id)
            }
          }
          private$.listeners$set(type, list())
        }
      } else {
        private$.listeners$reset()
        private$.id_to_types$reset()
      }
      invisible(self)
    },

    # Register a listener for condition type
    # @param type character, event type
    # @param handler function, callback receiving the condition object
    # @param id character or NULL, optional listener ID (auto-generated
    #   via digest::digest(list(handler, type)) when NULL)
    # @param after logical, if TRUE (default) append the handler to the
    #   end of the list; if FALSE prepend it. Same semantics as
    #   on.exit(after = TRUE).
    # @param auto_free logical, if TRUE the handler is automatically
    #   removed at the end of wrap() (default: FALSE)
    # @return character, the listener ID (invisibly)
    on = function(type, handler, id = NULL, after = TRUE,
                  auto_free = FALSE) {
      stopifnot(is.function(handler))
      if (is.null(id) || is.na(id)) {
        id <- digest::digest(list(handler, type))
      }
      private$.ensure_type(type)

      entry <- list(
        id = id,
        handler = handler,
        auto_free = isTRUE(auto_free)
      )

      # Upsert: remove existing entry with same id in this type
      entries <- private$.listeners$get(type)
      existing_idx <- which(vapply(
        entries, function(e) identical(e$id, id), logical(1L)
      ))
      if (length(existing_idx) > 0L) {
        entries <- entries[-existing_idx]
      }

      # Append or prepend based on after
      if (isTRUE(after)) {
        entries <- c(entries, list(entry))
      } else {
        entries <- c(list(entry), entries)
      }
      private$.listeners$set(type, entries)

      # Update reverse lookup
      existing_types <- private$.id_to_types$get(id)
      private$.id_to_types$set(id, unique(c(existing_types, type)))
      invisible(id)
    },

    # Remove a listener by ID from specified types (default: all)
    off = function(id, types) {
      registered_types <- private$.id_to_types$get(id)
      if (missing(types)) {
        types <- registered_types
      }
      if (!is.null(types)) {
        for (type in types) {
          if (private$.listeners$has(type)) {
            entries <- private$.listeners$get(type)
            keep <- !vapply(
              entries, function(e) identical(e$id, id), logical(1L)
            )
            private$.listeners$set(type, entries[keep])
          }
        }
        registered_types <- registered_types[!registered_types %in% types]
        if (length(registered_types) > 0L) {
          private$.id_to_types$set(id, registered_types)
        } else {
          private$.id_to_types$remove(id)
        }
      }
      invisible(self)
    },

    # Signal a condition - registered listeners are invoked inline
    # For conditions with restarts (e.g. suspend), this is called
    # from within the withRestarts() block set up by wrap()
    emit = function(type, message = type, ...) {
      tricobbler_condition(type, message, ..., signal = TRUE)
    },

    # Wrap an expression with withCallingHandlers built from registry.
    # Handlers registered with auto_free = TRUE are removed at the end
    # (via on.exit), even if expr throws an error.
    wrap = function(expr, restarts = list()) {
      # Collect IDs of auto_free handlers to remove after execution
      auto_free_ids <- character(0L)
      for (type in private$.listeners$keys()) {
        entries <- private$.listeners$get(type)
        for (entry in entries) {
          if (isTRUE(entry$auto_free)) {
            auto_free_ids <- c(auto_free_ids, entry$id)
          }
        }
      }
      auto_free_ids <- unique(auto_free_ids)

      # Build handler list from registry (in registration order)
      handlers <- list()
      for (type in private$.listeners$keys()) {
        entries <- private$.listeners$get(type)
        if (length(entries) == 0L) next
        condition_class <- paste0("tricobbler_", type)
        # Capture the ordered handler functions
        handlers[[condition_class]] <- local({
          fns <- lapply(entries, `[[`, "handler")
          function(cond) {
            for (fn in fns) {
              fn(cond)
            }
          }
        })
      }

      # Install restarts, then handlers, then evaluate
      wrapped <- if (length(restarts) > 0L) {
        do.call(withRestarts, c(list(expr), restarts))
      } else {
        expr
      }

      if (length(handlers) > 0L) {
        result <- do.call(withCallingHandlers, c(list({
          # Schedule auto_free cleanup inside the handler frame so it
          # runs even if expr throws
          on.exit({
            for (af_id in auto_free_ids) {
              self$off(af_id)
            }
          }, add = TRUE)
          wrapped
        }), handlers))
      } else {
        result <- wrapped
      }

      result
    }
  )
)
