
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
#'   common properties \code{name} and \code{description} that are shared by
#'   concrete subclasses such as \code{\link{MasterPolicy}} and
#'   \code{\link{StatePolicy}}. This class is not exported and is intended
#'   for internal use only.
#' @param name character, must be a non-blank single string
#' @param description character, human-readable description; multiple values
#'   are collapsed into a single space-separated string
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
#'   the set of allowed stages. Inherits from \code{\link{BasePolicy}}.
#' @details
#' ## Stages as Workflow Vocabulary
#'
#' The \code{stages} property defines the symbolic vocabulary of workflow
#' phases (e.g., \code{"triage"}, \code{"planning"}, \code{"executing"}).
#' These are macro-level phase names that must be implemented by at least one
#' \code{\link{StatePolicy}} object in the \code{\link{Manifest}}.
#'
#' The reserved stage names \code{"ready"}, \code{"error"}, and
#' \code{"human"} cannot be used because they are managed internally by
#' the \code{\link{Scheduler}}.
#'
#' ## Stage Naming Conventions
#'
#' \itemize{
#'   \item Stages are automatically converted to lowercase for consistency
#'   \item Must contain only letters (a-z), digits (0-9), underscores
#'     (\verb{_}), or dashes (\verb{-})
#'   \item Must be unique (case-insensitive) within a workflow
#'   \item Cannot be blank or \code{NA}
#' }
#'
#' ## Immutability
#'
#' \code{MasterPolicy} objects are immutable (S7 value semantics). Once
#' created, they serve as a stable reference for the \code{\link{Manifest}}.
#' Use the \code{@@} accessor to read properties
#' (e.g., \code{policy@@stages}).
#'
#' @param name character, name of the policy (non-blank)
#' @param description character, human-readable description
#' @param version character, version string in \code{"major.minor.patch"}
#'   format (e.g., \code{"1.0.0"})
#' @param stages character, non-empty vector of unique, lowercase stage names.
#'   Each element may contain only letters, digits, underscores, or dashes.
#'   Defaults to \code{c("triage", "planning", "executing")}
#' @param parameters list, additional free-form parameters for the workflow.
#'   These values are available to agents via
#'   \code{runtime$get_parameter(key, levels = "global")}.
#' @examples
#'
#' MasterPolicy(
#'   name = "example",
#'   version = "1.0.0",
#'   stages = c("idle", "triage", "planning"),
#'   parameters = list(location = "NYC")
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
