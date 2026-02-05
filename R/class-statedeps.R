#' @include class-s7base.R
NULL

# ---------------------------------------------------------------------------
# StateDeps - Named list of dependency declarations for async execution
# ---------------------------------------------------------------------------
#
# This class validates dependency declarations used by StatePolicy@depends_on.
# Each dependency maps a parameter name to a prior state's output.
#
# Format: named list where each element is a dependency specification:
#   list(
#     param_name = list(state = "state_name", field = "result", stage = NULL),
#     other_param = list(state = "other_state", field = "description")
#   )
#
# - Names become parameter names passed to the agent function
# - state: (required) name of the state to depend on
# - field: (optional) "result" (default) or "description"
# - stage: (optional) stage of the dependency; NULL = same stage (higher
#          priority required), or a named stage that must appear earlier
#          in MasterPolicy@stages
# ---------------------------------------------------------------------------

#' @title Validated Dependency Declarations for State Policies
#' @description S7 class representing a named list of dependency declarations
#'   used by \code{StatePolicy@@depends_on}. Each entry maps a parameter name
#'   to a prior state's output field.
#' @details
#' ## Dependency Entry Format
#'
#' Each entry in a \code{StateDeps} object is a named list element where:
#' \itemize{
#'   \item \strong{Name}: The parameter name passed to the agent function
#'   \item \strong{state}: (required) Name of the state to depend on
#'   \item \strong{field}: (optional) What to extract - \code{"result"}
#'     (default) or \code{"description"}
#'   \item \strong{stage}: (optional) Stage of the dependency. If \code{NULL}
#'     or omitted, the dependency must be in the same stage with higher
#'     priority. If specified, must be an earlier stage in the workflow
#' }
#'
#' ## Same-Stage vs Cross-Stage Dependencies
#'
#' \itemize{
#'   \item \strong{Same-stage} (stage = NULL): The dependent state must have
#'     higher priority (lower number = runs later). This ensures the dependency

#'     executes before the dependent within parallel priority groups
#'   \item \strong{Cross-stage} (stage = "earlier_stage"): The dependency is
#'     in a previous stage. Since stages execute sequentially, the dependency
#'     is guaranteed to complete before the dependent stage begins
#' }
#'
#' ## Validation Rules (Property-Level)
#'
#' The property-level validator checks structural correctness:
#' \itemize{
#'   \item Must be a named list (names become parameter names)
#'   \item Each entry must be a list with required \code{state} field
#'   \item \code{state} must be a single non-blank character string
#'   \item \code{field} (if present) must be \code{"result"} or
#'     \code{"description"}
#'   \item \code{stage} (if present) must be a single character string or NULL
#'   \item Parameter names must be valid R identifiers
#' }
#'
#' ## Cross-Validation (Manifest-Level)
#'
#' Additional validation is performed at the \code{Manifest} level to check:
#' \itemize
#'   \item Referenced states exist in the manifest
#'   \item Same-stage dependencies have higher priority
#'   \item Cross-stage dependencies reference earlier stages
#' }
#'
#' @examples
#' # Same-stage dependencies (state must have higher priority)
#' deps <- StateDeps(
#'   validation_result = list(state = "validator"),
#'   parsed_data = list(state = "parser", field = "result")
#' )
#'
#' # Cross-stage dependency
#' deps2 <- StateDeps(
#'   plan = list(state = "planner", field = "result", stage = "planning")
#' )
#'
#' # Empty dependencies (valid)
#' empty_deps <- StateDeps()
#'
#' @keywords internal
#' @export
StateDeps <- S7::new_class(
  name = "StateDeps",
  parent = BaseClass,
  properties = list(
    deps = S7::new_property(
      class = S7::class_list,
      default = list(),
      setter = function(self, value) {
        nms <- names(value)
        value <- structure(
          names = nms,
          lapply(value, function(v) {
            if (is.character(v)) {
              v <- list(state = v)
            }
            v
          })
        )
        S7::prop(self, "deps") <- value
        self
      },
      validator = function(value) {
        if (length(value) == 0) {
          return()
        }

        # Must be a named list
        nms <- names(value)
        if (is.null(nms) || any(nms == "") || any(is.na(nms))) {
          return(
            "must be a named list where names are parameter names for the agent function." # nolint: line_length_linter.
          )
        }

        # Check for duplicate names
        if (anyDuplicated(nms)) {
          return("parameter names (list names) must be unique.")
        }

        # Validate each entry
        for (i in seq_along(value)) {
          nm <- nms[[i]]
          entry <- value[[i]]

          # Each entry must be a list
          if (!is.list(entry)) {
            return(sprintf(
              "entry %s must be a list with at least 'state' field.",
              sQuote(nm)
            ))
          }

          # Required: state
          if (is.null(entry$state)) {
            return(sprintf(
              "entry %s missing required 'state' field.",
              sQuote(nm)
            ))
          }
          if (
            !is.character(entry$state) ||
            length(entry$state) != 1 ||
            is.na(entry$state) ||
            !nzchar(trimws(entry$state))
          ) {
            return(sprintf(
              "entry %s: 'state' must be a single non-blank character string.",
              sQuote(nm)
            ))
          }

          # Optional: field (default "result")
          if (!is.null(entry$field)) {
            if (
              !is.character(entry$field) ||
              length(entry$field) != 1 ||
              !entry$field %in% c("result", "description")
            ) {
              return(sprintf(
                "entry %s: 'field' must be 'result' or 'description'.",
                sQuote(nm)
              ))
            }
          }

          # Optional: stage (NULL = same stage)
          if (!is.null(entry$stage)) {
            if (
              !is.character(entry$stage) ||
              length(entry$stage) != 1 ||
              is.na(entry$stage) ||
              !nzchar(trimws(entry$stage))
            ) {
              return(sprintf(
                "entry %s: 'stage' must be a single non-blank character string or NULL.", # nolint: line_length_linter.
                sQuote(nm)
              ))
            }
          }

          # Check for unknown fields
          known_fields <- c("state", "field", "stage")
          unknown <- setdiff(names(entry), known_fields)
          if (length(unknown) > 0) {
            return(sprintf(
              "entry %s has unknown fields: %s. Allowed: %s",
              sQuote(nm),
              paste(sQuote(unknown), collapse = ", "),
              paste(sQuote(known_fields), collapse = ", ")
            ))
          }
        }

        return()
      }
    )
  ),
  constructor = function(..., .list = list()) {
    deps <- c(list(...), as.list(.list))
    S7::new_object(S7::S7_object(), deps = deps)
  }
)

# as.list method for serialization
S7::method(as.list, StateDeps) <- function(x, ...) {
  # Normalize entries: add defaults for serialization clarity
  lapply(x@deps, function(entry) {
    re <- list(
      state = entry$state,
      field = entry$field %||% "result"
    )
    re$stage <- entry$stage  # NULL is fine, won't appear in YAML
    re
  })
}

# format method for printing
S7::method(format, StateDeps) <- function(x, ...) {
  if (length(x@deps) == 0) {
    return("StateDeps: (empty)")
  }
  lines <- vapply(names(x@deps), function(nm) {
    entry <- x@deps[[nm]]
    stage_str <- if (is.null(entry$stage)) ".same." else entry$stage
    sprintf(
      "  %s <- %s@%s (stage: %s)",
      nm,
      entry$state,
      entry$field %||% "result",
      stage_str
    )
  }, character(1))
  paste(c("StateDeps:", lines), collapse = "\n")
}

