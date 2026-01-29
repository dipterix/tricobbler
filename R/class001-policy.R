# Policy defines how a workflow should be carried out: different phase of the workflow
# For example, IDLE, TRIAGE, PLANNING, VALIDATING, EXECUTING, REVIEWING

#' @title Base Policy (abstract)
#' @description Internal abstract base class for all policy objects. It defines
#'   common properties `name` and `description` that are shared by concrete
#'   subclasses. This class is not exported and is intended for internal use
#'   only.
#' @field name Character. Must be a non‑blank single string.
#' @field description Character. Human‑readable description; multiple values are
#'   collapsed into a single space‑separated string.
#' @keywords internal
BasePolicy = S7::new_class(
  name = "Policy",
  abstract = TRUE,
  properties = list(
    name = S7::new_property(
      class = S7::class_character,
      validator = function(value) {
        if(length(value) != 1 || is.na(value) || !nzchar(trimws(value))) {
          return("cannot be blank value.")
        }
        return()
      }
    ),
    description = S7::new_property(
      class = S7::class_character,
      setter = function(self, value) {
        S7::prop(self, "description") <- paste(value, collapse = " ")
        self
      }
    )
  )
)

#' @title Master Policy
#' @description Concrete policy that defines the overall workflow version and the
#'   set of allowed states. Inherits from `BasePolicy`.
#' @field version Character. Version string validated by `property_version()`.
#' @field states Character vector. Non‑empty, lower‑cased, unique, and each
#'   element may contain only letters, digits, underscores or dashes.
#' @field parameters List. Additional free‑form parameters for the workflow.
#' @examples
#' # Valid MasterPolicy
#' mp <- MasterPolicy(
#'   name = "example",
#'   version = "1.0.0",
#'   states = c("idle", "triage", "planning"),
#'   parameters = list()
#' )
#' mp
#' # Invalid: duplicate state (case‑insensitive)
#' try(MasterPolicy(name = "bad", version = "1.0", states = c("idle", "IDLE")))
#' @export
MasterPolicy <- S7::new_class(
  name = "MasterPolicy",
  parent = BasePolicy,
  properties = list(
    version = property_version(),
    states = S7::new_property(
      class = S7::class_character,
      validator = function(value) {
        if(length(value) == 0) { return("number of states cannot be 0") }
        value <- tolower(value)
        if(anyNA(value) || anyDuplicated(value) || !all(nzchar(value))) {
          return("state string cannot be duplicated (case-insensitive), NA, or blank.")
        }
        if(any(grepl("[^a-zA-Z0-9_-]", value))) {
          return("state string can only contain letters (a-z), digits (0-9), underscore (_) or dash (-).")
        }
        return()
      }
    ),
    # Additional parameters
    parameters = S7::class_list
  )
)

# ---------------------------------------------------------------------------
# StagePolicy – describes *what* a workflow stage represents.  It contains the
# symbolic state name (must match a MasterPolicy state) and optional metadata
# such as a human‑readable purpose and stage‑specific parameters.
# ---------------------------------------------------------------------------
#' @title Stage Policy
#' @description Represents a single workflow stage. Inherits from `BasePolicy`
#'   and adds a mandatory `state` that must match one of the states defined in a
#'   `MasterPolicy`.
#' @field state Character. Must be a non‑blank single string.
#' @field purpose Character. Human‑readable description of the stage.
#' @field parameters List. Optional stage‑specific parameters.
#' @examples
#' # Valid StagePolicy
#' sp <- StagePolicy(
#'   name = "stage1",
#'   state = "idle",
#'   purpose = "initial idle stage",
#'   parameters = list()
#' )
#' sp
#' # Invalid: blank state
#' try(StagePolicy(name = "bad", state = "", purpose = "bad"))
#' @export
StagePolicy <- S7::new_class(
  name = "StagePolicy",
  parent = BasePolicy,
  properties = list(
    # The symbolic name of the stage – must correspond to a MasterPolicy state
    state = S7::new_property(
      class = S7::class_character,
      validator = function(value) {
        if (length(value) != 1 || is.na(value) || !nzchar(trimws(value))) {
          return("state must be a non‑blank single character string.")
        }
        return()
      }
    ),
    # Short description of what the stage does (e.g., "classify request")
    purpose = S7::new_property(class = S7::class_character),
    # Optional list of stage‑specific parameters (e.g., timeout, retries)
    parameters = S7::new_property(class = S7::class_list)
  )
)


#' @title Manifest
#' @description Container that ties a `MasterPolicy` together with a list of
#'   `StagePolicy` objects. The validator ensures that every state defined in
#'   the master policy is represented by at least one stage.
#' @field master `MasterPolicy` object.
#' @field stages List of `StagePolicy` objects.
#' @examples
#' # Create a valid manifest
#' mp <- MasterPolicy(name = "example", version = "1.0.0",
#'                    states = c("idle", "triage"), parameters = list())
#' sp1 <- StagePolicy(name = "stage1", state = "idle", purpose = "idle stage")
#' sp2 <- StagePolicy(name = "stage2", state = "triage", purpose = "triage stage")
#' mf <- Manifest(master = mp, stages = list(sp1, sp2))
#' mf
#' # Invalid: missing a required stage
#' try(Manifest(master = mp, stages = list(sp1)))
#' @export
Manifest <- S7::new_class(
  name = "Manifest",
  properties = list(
    master = MasterPolicy,
    # List of StagePolicy objects – one per workflow stage
    stages = S7::new_property(
      class = S7::class_list,
      validator = function(value) {
        ok <- vapply(value, function(x) S7::S7_inherits(x, StagePolicy), logical(1))
        if (!all(ok)) {
          return("all elements in 'stages' must be `StagePolicy` objects.")
        }
        return()
      }
    )
  ),
  validator = function(self) {
    # CRITICAL: Cross-check the Stage Policies against the Master Policy
    # Each state defined in MasterPolicy must be represented by at least one
    # StagePolicy.  The actual execution details live in the separate contract
    # class, so we only validate the presence of the stage here.
    stage_states <- vapply(self@stages, function(x) x@state, character(1))
    valid_states <- self@master@states

    missing_states <- setdiff(valid_states, stage_states)
    if (length(missing_states) > 0) {
      return(sprintf(
        "Each state defined in MasterPolicy must be implemented with one or more AgentPolicies. Missing states: %s",
        paste(sQuote(missing_states), collapse = ", ")
      ))
    }
    return()
  }
)
