#' @include helper-conditions.R
NULL

# Internal event dispatcher using direct function-call dispatch
#
# Inspired by mrdoob/eventdispatcher.js. Unlike the condition-based
# approach (\code{signalCondition}/\code{withCallingHandlers}), this
# pattern invokes registered handlers by iterating a snapshot of the
# listener list and calling each \code{handler(event)} directly.
# This is compatible with \code{coro::async}, which does not support
# \code{withCallingHandlers()} inside coroutine bodies.
#
# Provides JS-like ordered event handling. Handlers execute in
# registration order by default (\code{after = TRUE} appends,
# \code{after = FALSE} prepends), similar to
# \code{on.exit(after = TRUE)}.
#
# Uses \code{fastmap::fastmap()} for type-level and reverse-lookup
# maps, but each type's handlers are stored in a plain list to
# guarantee deterministic, sequential execution order.
#
# \code{digest::digest()} auto-generates deterministic IDs from
# handler functions, preventing duplicate registration of the same
# closure. User-supplied \code{id} enables explicit replacement
# (upsert semantics).
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
    # Entry: list(id, handler)
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

    # Register a listener for an event type
    # @param type character, event type
    # @param handler function, callback receiving the event list
    # @param id character or NULL, optional listener ID (auto-generated
    #   via digest::digest(list(handler, type)) when NULL)
    # @param after logical, if TRUE (default) append the handler to the
    #   end of the list; if FALSE prepend it. Same semantics as
    #   on.exit(after = TRUE).
    # @return character, the listener ID (invisibly)
    on = function(type, handler, id = NULL, after = TRUE) {
      stopifnot(is.function(handler))
      if (is.null(id) || is.na(id)) {
        id <- digest::digest(list(deparse1(handler), type))
      }
      private$.ensure_type(type)

      entry <- list(
        id = id,
        handler = handler
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

    # Check whether a specific listener is registered
    # @param type character, event type
    # @param id character, listener ID
    # @return logical
    has = function(type, id) {
      if (!private$.listeners$has(type)) return(FALSE)
      entries <- private$.listeners$get(type)
      any(vapply(
        entries, function(e) identical(e$id, id), logical(1L)
      ))
    },

    # Dispatch an event to all registered listeners (direct-call)
    #
    # Iterates a snapshot of the listener list for \code{type} and
    # calls each \code{handler(event)} in registration order.
    # Returns the first non-NULL handler return value (first-wins
    # semantics), which is used by suspend to capture the chosen
    # action string.
    #
    # @param event list, the event object. Must contain a
    #   \code{$type} field.
    # @return The first non-NULL value returned by a handler, or
    #   \code{NULL} if no handler returns a value.
    emit = function(event) {
      type <- event$type
      if (is.null(type) || !private$.listeners$has(type)) {
        return(invisible(NULL))
      }

      # Snapshot: copy the list so mutations during iteration are safe
      entries <- private$.listeners$get(type)
      if (length(entries) == 0L) return(invisible(NULL))

      result <- NULL
      for (entry in entries) {
        rv <- entry$handler(event)
        if (is.null(result) && !is.null(rv)) {
          result <- rv
        }
      }
      invisible(result)
    }
  )
)
