
# ---------------------------------------------------------------------------
# DESIGN LOGIC: Two-Tier Policy System
# ---------------------------------------------------------------------------
# This file defines the immutable policy layer (Tier 1) of the package
# architecture. Policies are blueprints that specify WHAT a workflow should
# do, not HOW it executes (which is handled by the Runtime layer).
#
# The two-tier separation is:
#   1. Policy (this file) - Immutable workflow blueprints using S7
#   2. Runtime (R/class-scheduler.R) - Mutable execution orchestration using R6
#
# KEY DESIGN DECISIONS:
#
# 1. Stages vs States:
#    - STAGES are symbolic phase names (e.g., "triage", "planning") defined
#      in MasterPolicy. They represent the vocabulary of workflow phases.
#    - STATES are concrete policy implementations (StatePolicy objects) that
#      reference stages and add metadata (description, parameters).
#    - Multiple states can share the same stage, enabling fallback chains.
#    - Every stage MUST have at least one state (enforced by \code{Manifest}
#      validation).
#
# 2. Manifest as Validated Blueprint:
#    - Manifest ties MasterPolicy to StatePolicy list, ensuring completeness.
#    - The validation performs cross-validation: all master stages must have
#      at least one corresponding state policy.
#    - This ensures no "orphaned" stages exist in the workflow definition.
#    - Manifest is a policy container that defines the workflow blueprint,
#      which is then executed by the Scheduler in the Runtime layer.
#
# 3. Immutability via S7:
#    - Policies are immutable to prevent accidental modification during
#      execution.
#    - Once created, a policy serves as a stable reference for contracts.
#    - Use S7 @ accessor (e.g., policy@name) not R6 $ accessor.
#
# 4. Serialization (manifest_read/manifest_write):
#    - Policies can be saved as YAML for version control and sharing.
#    - YAML format enables human-readable workflow definitions.
#    - Reading YAML validates all constraints automatically.
# ---------------------------------------------------------------------------

#' @title Abstract Base Class for Policy Objects
#' @description Internal abstract base class for all policy objects. It defines
#'   common properties `name` and `description` that are shared by concrete
#'   subclasses. This class is not exported and is intended for internal use
#'   only.
#' @param name Character. Must be a non-blank single string.
#' @param description Character. Human-readable description; multiple values are
#'   collapsed into a single space-separated string.
#' @keywords internal
BasePolicy <- S7::new_class(
  name = "Policy",
  parent = BaseClass,
  abstract = TRUE,
  properties = list(
    name = S7::new_property(
      class = S7::class_character,
      validator = function(value) {
        if (length(value) != 1 || is.na(value) || !nzchar(trimws(value))) {
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



#' @title Master Workflow Policy Defining Stages and Version
#' @description Concrete policy that defines the overall workflow version and
#'   the set of allowed stages. Inherits from `BasePolicy`.
#' @details
#' ## Stages as Workflow Vocabulary
#'
#' The `stages` property defines the symbolic vocabulary of workflow phases
#' (e.g., "triage", "planning", "executing"). These are macro-level phase names
#' that must be implemented by at least one \code{StatePolicy} object in the
#' \code{Manifest}.
#'
#' ## Stage Naming Conventions
#'
#' - Stages are automatically converted to lowercase for consistency
#' - Must contain only letters (a-z), digits (0-9), underscores (_), or
#'   dashes (-)
#' - Must be unique (case-insensitive) within a workflow
#' - Cannot be blank or NA
#'
#' ## Immutability
#'
#' \code{MasterPolicy} objects are immutable (S7 value semantics). Once created,
#' they serve as a stable reference for \code{Contract} objects. Use the
#' \code{@@} accessor to read properties (e.g., `policy@stages`).
#'
#' @param name Character. Name of the policy (non-blank).
#' @param description Character. Human-readable description.
#' @param version Character. Version string validated by `property_version()`.
#' @param stages Character vector. Non-empty, lower-cased, unique, and each
#'   element may contain only letters, digits, underscores or dashes.
#' @param parameters List. Additional free-form parameters for the workflow.
#' @examples
#'
#' MasterPolicy(
#'   name = "example",
#'   version = "1.0.0",
#'   stages = c("idle", "triage", "planning"),
#'   parameters = list()
#' )
#'
#' @export
MasterPolicy <- S7::new_class(
  name = "MasterPolicy",
  parent = BasePolicy,
  properties = list(
    version = property_version(),
    stages = S7::new_property(
      class = S7::class_character,
      validator = function(value) {
        if (length(value) == 0) {
          return("number of stages cannot be 0")
        }
        value <- tolower(value)
        if (anyNA(value) || anyDuplicated(value) || !all(nzchar(value))) {
          return("stage string cannot be duplicated (case-insensitive), NA, or blank.") # nolint: line_length_linter.
        }
        if (any(grepl("[^a-zA-Z0-9_-]", value))) {
          return("stage string can only contain letters (a-z), digits (0-9), underscore (_) or dash (-).") # nolint: line_length_linter.
        }
        if (any(c("ready", "error", "human") %in% value)) {
          return("`ready`, `error`, and `human` are reserved stages. Please remove them.") # nolint: line_length_linter.
        }
        return()
      },
      default = c("triage", "planning", "executing")
    ),
    # Additional parameters
    parameters = S7::class_list
  )
)
