#' @include class-s7base.R
NULL


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


# ---------------------------------------------------------------------------
# StatePolicy - describes *what* a workflow state represents.  It contains the
# symbolic stage name (must match a MasterPolicy stage) and optional metadata
# such as a human-readable description and state-specific parameters.
# ---------------------------------------------------------------------------
#' @title State-Level Policy Implementation for Workflow Stages
#' @description Represents a single workflow state. Inherits from `BasePolicy`
#'   and adds a mandatory `stage` that must match one of the stages defined in a
#'   \code{MasterPolicy}. Includes priority and criticality flags for execution
#'   ordering when multiple states share the same stage.
#' @details
#' ## Stages vs States
#'
#' - **Stage** (macro): The workflow phase name from `MasterPolicy@stages`
#'   (e.g., "executing")
#' - **State** (micro): A concrete \code{StatePolicy} implementation of that
#'   stage
#' - Multiple states can reference the same stage for different execution
#'   patterns
#'
#' ## Priority System and Execution Patterns
#'
#' When multiple states share the same stage, execution pattern depends on
#'   priority:
#'
#' - **Range**: 0 (lowest) to 999 (highest)
#' - **Default**: 100 (when `priority = NA` or `NULL`)
#' - **Execution order**: States run sequentially by priority (higher first)
#'
#' ## Critical Flag: Enforcing Sequential Execution
#'
#' The `critical` flag enforces **sequential** execution with fail-fast
#'   semantics:
#'
#' - If `critical = TRUE`, this state **must** execute and succeed before any
#'   lower-priority states in the same stage can run
#' - If a critical state fails, lower-priority states are skipped entirely
#' - Critical states must have unique priority (enforced by \code{Manifest})
#' - Critical states must have `priority >= 1` (cannot be lowest priority 0)
#' - Critical states cannot share their priority value with other states in the
#'   same stage (enforced by \code{Manifest} validation)
#' - **Use case**: Required validation gates that must pass before
#'   alternatives run
#'
#' ## When to Use Multiple States Per Stage
#'
#' Create multiple \code{StatePolicy} objects for the same stage when you need:
#'
#' 1. **Fallback chains**: Different priorities create ordered
#'    execution with alternative strategies
#' 2. **Alternative implementations**: Multiple states for the same stage
#'    execution with alternative strategies
#'    (e.g., primary approach -> fallback -> last resort)
#' 3. **Critical validation gates**: Critical state must succeed before
#'    lower-priority alternatives execute (enforces sequential, fail-fast
#'    semantics)
#' 4. **Phased deployment**: Gradually shift priority as new implementations
#'    mature
#'
#' ## Parameters for Different Agent Types
#'
#' The \code{parameters} list is interpreted differently based on the agent
#' type created via \code{\link{as_agent}}:
#'
#' \strong{Deterministic Agents} (from functions or MCP tools):
#' \itemize{
#'   \item \code{args}: list, function arguments passed via
#'     \code{do.call(fun, args)}. Kept separate from other parameters to
#'     avoid conflicts with functions that have their own reserved arguments
#' }
#'
#' The \code{accessibility} property affects how inputs are constructed:
#' \itemize{
#'   \item \code{"all"}: First argument is the previous agent's result
#'     (\code{last_attachment$result}), followed by \code{args}
#'   \item \code{"logs"}: First argument is the previous agent's description
#'     (\code{last_attachment$description}), followed by \code{args}
#'   \item \code{"none"}: Only \code{args} are passed (no context access)
#' }
#'
#' \strong{Debug Mode}: Set \code{context$debug <- TRUE} at runtime to enable
#' debug mode. In debug mode, agents print their calls/tools for inspection
#' but run as no-op (returning debug information instead of executing).
#'
#' \strong{AI Agents} (from \pkg{ellmer} Chat objects):
#' \itemize{
#'   \item \code{system_prompt}: character, additional system prompt appended
#'     to \code{@@description}
#'   \item \code{user_prompt}: character, the user message to send to the LLM
#'   \item \code{keep_turns}: logical, if \code{TRUE}, retains conversation
#'     history across executions (default \code{FALSE})
#'   \item \code{return_type}: an \pkg{ellmer} type indicator (e.g.,
#'     \code{ellmer::type_object()}). When provided, triggers
#'     \code{chat$chat_structured(type = return_type)} for structured output.
#'     Can also be specified in YAML via \code{map_type_to_ellmer()}
#' }
#'
#' @param name character, name of the state policy (non-blank)
#' @param description character, human-readable description
#' @param stage character, must be a non-blank single string
#' @param parameters list, state-specific parameters passed to the agent.
#'   See \\strong{Parameters for Different Agent Types} in Details
#' @param priority integer, execution priority (0-999, default 100). Higher
#'   values run first (999 = highest priority, 0 = lowest). Used when multiple
#'   states share the same stage. NA or NULL are treated as 100
#' @param critical logical, if TRUE, states with lower priority will not
#'   execute if this state fails (default FALSE). Critical states must have
#'   priority >= 1 and cannot share priority code with other states
#' @param agent_id character, unique identifier for the agent responsible for
#'   executing this state. Must contain only letters, digits, underscores, or
#'   dashes
#' @param resources character vector, tools that the agent may call during
#'   execution (default empty vector)
#' @param accessibility character, context accessibility level for the agent.
#'   Controls what context data the agent can read. One of \code{"all"}
#'   (full access), \code{"logs"} (logs only), or \code{"none"} (no access).
#'   Default is \code{"all"}
#' @param max_retry integer, maximum total attempts (initial + retries) for
#'   this state during stage execution (default 0, meaning single attempt).
#'   The \code{max_retry} limit applies globally across all re-entries to this
#'   state within the same stage. If \code{on_failure} is set, this state will
#'   not retry locally but will jump to the failure handler immediately. If
#'   \code{on_failure} is NA, local retries up to \code{max_retry} will be
#'   attempted before moving to next state
#' @param final logical, if TRUE and validation succeeds, skip remaining states
#'   in the workflow (default FALSE)
#' @param on_failure character, name of the state to jump to on first failure
#'   (default NA to retry locally up to \code{max_retry} times). When set,
#'   failures trigger immediate jump to the specified state without local
#'   retries. The \code{max_retry} limit still applies globally to prevent
#'   infinite loops: if this state is re-entered and total attempts exceed
#'   \code{max_retry}, execution stops with an error. Common patterns:
#'   validation loops (\code{on_failure = "executor"}), alternative strategies
#'   (\code{on_failure = "slower_alternative"}), or repair chains
#'   (\code{on_failure = "repair_step"})
#' @examples
#'
#' # Basic state
#' StatePolicy(
#'   name = "state1",
#'   stage = "idle",
#'   description = "initial idle state",
#'   agent_id = "agent1",
#'   parameters = list()
#' )
#'
#' # Critical high-priority state
#' StatePolicy(
#'   name = "validator",
#'   stage = "executing",
#'   description = "critical validation step",
#'   agent_id = "validator_agent",
#'   priority = 900,
#'   critical = TRUE
#' )
#'
#' # Deterministic agent with parameters
#' StatePolicy(
#'   name = "formatter",
#'   stage = "executing",
#'   description = "Format data to JSON",
#'   agent_id = "json_formatter",
#'   accessibility = "none",  # Only use args, no context
#'   parameters = list(
#'     args = list(x = list(a = 1, b = 2), pretty = TRUE)
#'   )
#' )
#'
#' # AI agent with structured output
#' StatePolicy(
#'   name = "planner",
#'   stage = "planning",
#'   description = "Break down the task into steps",
#'   agent_id = "llm_planner",
#'   parameters = list(
#'     system_prompt = "You are a task planning expert.",
#'     user_prompt = "Plan the following task: ...",
#'     keep_turns = FALSE,
#'     return_type = ellmer::type_object(
#'       steps = ellmer::type_array(items = ellmer::type_string())
#'     )
#'   )
#' )
#'
#'
#' @export
StatePolicy <- S7::new_class(
  name = "StatePolicy",
  parent = BasePolicy,
  properties = list(
    # @name must be unique within the same MasterPolicy

    # The symbolic name of the state - must correspond to a MasterPolicy stage
    stage = S7::new_property(
      class = S7::class_character,
      validator = function(value) {
        if (length(value) != 1 || is.na(value) || !nzchar(trimws(value))) {
          return("stage must be a non-blank single character string.")
        }
        return()
      }
    ),

    # Short description of what the state does (e.g., "classify request")
    # For AI agents, this "should" be the system prompt (although the impl might
    # put the prompts in the parameters and I'm not yet sure about this)
    description = S7::new_property(class = S7::class_character),

    # WHO: Agent name to help us find the agent class (R6)
    # The agent should implement $run(context, policy)
    # $validate(output) -> TRUE/error
    agent_id = S7::new_property(
      class = S7::class_character,
      validator = function(value) {
        if (
          length(value) != 1 ||
            is.na(value) ||
            !nzchar(value) ||
            grepl("[^a-zA-Z0-9_-]", value)
        ) {
          return(
            "Agent ID must not be empty and must only contain letters, digits, underscores, or dashes" # nolint: line_length_linter.
          )
        }
        return()
      }
    ),

    # WHAT: Tools available to this stage
    resources = S7::new_property(
      class = S7::class_character,
      default = character(0L)
    ),

    # What context the agent may access: this is a safe-check
    # in case the agent calls the other tools to read attachments
    # accessibility != "all" will lock them out
    accessibility = S7::new_property(
      class = S7::class_character,
      validator = function(value) {
        if (length(value) != 1 || is.na(value)) {
          return("accessibility must be a single character value.")
        }
        valid_choices <- c("all", "logs", "none")
        if (!value %in% valid_choices) {
          return(sprintf(
            "accessibility must be one of %s.",
            paste(sQuote(valid_choices), collapse = ", ")
          ))
        }
        return()
      },
      default = "all"
    ),

    # Optional list of state-specific parameters (e.g., timeout, retries, or
    # other constraints)
    parameters = S7::new_property(class = S7::class_list),
    max_retry = S7::new_property(class = S7::class_integer, default = 0L),

    # Execution priority: higher values run first (0-999)
    priority = S7::new_property(
      class = S7::class_integer,
      validator = function(value) {
        if (length(value) != 1 || is.na(value)) {
          return("priority must be a single integer value.")
        }
        if (value < 0 || value > 999) {
          return("priority must be between 0 and 999 (inclusive).")
        }
        return()
      },
      setter = function(self, value) {
        # Treat NA or NULL as default 100L
        if (is.null(value) || (length(value) == 1 && is.na(value))) {
          S7::prop(self, "priority") <- 100L
        } else {
          S7::prop(self, "priority") <- as.integer(value)
        }
        self
      },
      default = 100L
    ),

    # Critical flag: if TRUE, suspend and wait for human
    critical = S7::new_property(
      class = S7::class_logical,
      default = FALSE
    ),

    # final flag: if TRUE and validated, then skip the states
    final = S7::new_property(
      class = S7::class_logical,
      default = FALSE
    ),

    # Bailout state: if max try reached, jump to a state to handle it
    # default is NA, jump to the next one;
    # This field is respected either critical or not
    # must not be @name
    on_failure = S7::new_property(
      class = S7::class_character,
      default = NA_character_
    )
  ),
  validator = function(self) {
    # Critical states must have priority >= 1 (cannot be lowest priority)
    if (self@critical && self@priority < 1) {
      return("critical states must have priority >= 1 (cannot be 0).")
    }
    return()
  }
)

#' @title Validated Container Linking Master Policy to State Policies
#' @description Container that ties a \code{MasterPolicy} together with a list
#'   of \code{StatePolicy} objects. The validation ensures that every stage
#'   defined in the master policy is represented by at least one state.
#' @details
#' ## Two-Tier Architecture
#'
#' The \code{Manifest} class represents the validated blueprint layer (Tier 1)
#'   in the package two-tier design:
#'
#' 1. **Policy Layer (Tier 1 - Immutable S7)**: Blueprint definitions
#'    - \code{Manifest}: Validated container linking \code{MasterPolicy} to
#'      \code{StatePolicy} list
#'    - \code{MasterPolicy}: Workflow version + allowed stages (macro-level
#'      phases)
#'    - \code{StatePolicy}: Individual state metadata (stage, description,
#'      parameters, priority)
#'
#' 2. **Runtime Layer (Tier 2 - Mutable R6)**: Execution orchestration
#'    - \code{Scheduler}: Orchestrates all stage/state progression and
#'      workflow execution
#'    - \code{Context}: Manages execution environment (logging, storage,
#'      attachments)
#'    - \code{Agent}: Executes state-specific logic
#'
#' ## Stages vs States: Critical Distinction
#'
#' - **Stages** (symbolic vocabulary): Workflow phase names defined in
#'   \code{MasterPolicy@@stages}
#'   (e.g., "triage", "planning", "executing")
#' - **States** (concrete implementations): \code{StatePolicy} objects that
#'   reference stages and add execution metadata (description, parameters,
#'   priority)
#' - **Multiple states per stage**: Enables different execution patterns:
#'   - **Sequential fallback**: Different priorities create ordered execution
#'   - **Critical gates**: Critical states enforce fail-fast validation
#'     semantics
#' - **Validation rule**: Every stage in \code{MasterPolicy@@stages} MUST have
#'     at least one corresponding \code{StatePolicy} (enforced by validation)
#'
#' ## Validation Rules
#'
#' The \code{Manifest} validation performs critical cross-checks:
#'
#' 1. **Completeness**: Every \code{MasterPolicy} stage has at least one
#'      \code{StatePolicy}
#'    - Prevents "orphaned" stages with no implementation
#'    - Error message: "Missing stages: ..."
#'
#' 2. **Critical priority uniqueness**: Critical states cannot share priorities
#'    - If `StatePolicy@critical = TRUE`, no other state in the same stage can
#'      have the same `priority` value
#'    - Prevents ambiguity about which critical state blocks lower-priority
#'      states
#'    - Error message: "Critical state ... cannot share its priority with ..."
#'
#' ## Immutability and Serialization
#'
#' Once created, \code{Manifest} objects are immutable (S7 value semantics),
#' providing a stable reference for runtime execution. Manifests can be
#' serialized to/from YAML for version control:
#'
#' - \code{manifest_write(manifest, file)}: Save to human-readable YAML
#' - \code{manifest_read(file)}: Load with full validation
#' - All validation rules apply when reading YAML
#'
#' ## Policy vs Runtime Separation
#'
#' The \code{Manifest} is a policy-level blueprint that defines WHAT the
#' workflow should do (stages and states), not HOW it executes. The actual
#' execution (orchestration, logging, state management) is handled by
#' \code{Scheduler} and \code{Context} in the Runtime layer.
#'
#' @param master \code{MasterPolicy} object
#' @param states list of \code{StatePolicy} objects
#' @examples
#' # Create a valid manifest
#' mp <- MasterPolicy(
#'   name = "example", version = "1.0.0",
#'   stages = c("idle", "triage"), parameters = list()
#' )
#' sp1 <- StatePolicy(
#'   name = "state1", stage = "idle",
#'   description = "idle state", agent_id = "agent1"
#' )
#' sp2 <- StatePolicy(
#'   name = "state2", stage = "triage",
#'   description = "triage state", agent_id = "agent2"
#' )
#' mf <- Manifest(master = mp, states = list(sp1, sp2))
#' mf
#' @export
Manifest <- S7::new_class(
  name = "Manifest",
  parent = BaseClass,
  properties = list(
    name = S7::new_property(
      S7::class_character,
      getter = function(self) {
        self@master@name
      }
    ),
    master = MasterPolicy,
    # List of StatePolicy objects - one per workflow state
    states = S7::new_property(
      class = S7::class_list,
      validator = function(value) {
        ok <- vapply(
          value,
          function(x) S7::S7_inherits(x, StatePolicy),
          logical(1)
        )
        if (!all(ok)) {
          return(
            "all elements in 'states' must be \\code{StatePolicy} objects."
          )
        }
        return()
      }
    )
  ),
  validator = function(self) {
    # CRITICAL: Cross-check the State Policies against the Master Policy
    # Each stage defined in MasterPolicy must be represented by at least one
    # StatePolicy.  The actual execution details live in the separate contract
    # class, so we only validate the presence of the state here.
    state_stages <- vapply(self@states, function(x) x@stage, character(1))
    valid_stages <- self@master@stages

    missing_stages <- setdiff(valid_stages, state_stages)
    if (length(missing_stages) > 0) {
      return(sprintf(
        "Each stage defined in MasterPolicy must be implemented with one or more StatePolicies. Missing stages: %s", # nolint: line_length_linter.
        paste(sQuote(missing_stages), collapse = ", ")
      ))
    }

    # CRITICAL: Validate priority uniqueness for critical states
    # If a state is critical, no other state with the same stage can share
    # its priority value.
    for (i in seq_along(self@states)) {
      state <- self@states[[i]]
      if (state@critical) {
        # Find other states with the same stage
        same_stage_idx <- which(state_stages == state@stage)
        same_stage_idx <- same_stage_idx[same_stage_idx != i]

        if (length(same_stage_idx) > 0) {
          # Check if any share the same priority
          other_priorities <- vapply(
            self@states[same_stage_idx],
            function(s) s@priority,
            integer(1)
          )

          if (any(other_priorities == state@priority)) {
            conflicting_names <- vapply(
              self@states[same_stage_idx][other_priorities == state@priority],
              function(s) s@name,
              character(1)
            )
            return(sprintf(
              "Critical state %s (stage=%s, priority=%d) cannot share its priority with other states in the same stage: %s", # nolint: line_length_linter
              sQuote(state@name),
              sQuote(state@stage),
              state@priority,
              paste(sQuote(conflicting_names), collapse = ", ")
            ))
          }
        }
      }
    }

    return()
  }
)

extract_manifest_state <- function(x, stage) {
  stages <- x@master@stages
  stage <- match.arg(stage, choices = stages)

  priorities <- vapply(x@states, function(state) {
    # state <- x@states[[1]]
    ifelse(state@stage == stage, state@priority, NA_integer_)
  }, NA_integer_)

  n <- sum(!is.na(priorities))

  odr <- order(priorities, decreasing = TRUE, na.last = TRUE)[seq_len(n)]

  x@states[odr]
}


S7::method(format, Manifest) <- function(x, ...) {
  stages <- x@master@stages

  stage_str <- lapply(stages, function(stage) {
    states <- extract_manifest_state(x, stage)
    state_detail_str <- lapply(states, function(state) {
      priority_str <- sprintf("priority: %d", state@priority)
      critial_str <- ifelse(state@critical, ", critical", "")
      sprintf("  - State: `%s` (%s%s)", state@name, priority_str, critial_str)
    })
    c(
      sprintf("State `%s`", stage),
      unlist(state_detail_str)
    )
  })

  concatern(
    c(
      sprintf("Manifest (S7 class) - `%s` (%s)", x@name, x@master@version),
      "",
      sprintf(
        "Master policy stages: \n  %s",
        paste(x@master@stages, collapse = ", ")
      ),
      "",
      unlist(stage_str)
    )
  )
}

#' @name manifest-file
#' @title Read or Write Manifest from or to a YAML File
#' @description
#' Serialize and read \code{Manifest} objects to and from YAML files. This
#' enables version control and sharing of workflow policy definitions. The
#' resulting YAML files are human-readable and can be edited manually, though
#' changes must still pass validation when read back.
#'
#' \code{manifest_write()} serializes a \code{Manifest} object to a YAML file.
#'
#' \code{manifest_read()} reads a YAML file back into a validated
#' \code{Manifest} object, reconstructing the \code{MasterPolicy} and
#' \code{StatePolicy} objects and enforcing all validation rules (e.g.,
#' every stage must have a corresponding state).
#' @param x a \code{Manifest} object to serialize (for \code{manifest_write})
#' @param file Character. Path to the YAML file (input for `manifest_read`,
#'   output for `manifest_write`).
#' @param ... Additional arguments passed to `yaml::read_yaml()` or
#'   `yaml::write_yaml()`.
#' @return
#' - `manifest_write()`: Invisibly returns the path to the written file.
#' - \code{manifest_read()}: A validated \code{Manifest} object
#' @examples
#' # Create a manifest
#' mp <- MasterPolicy(
#'   name = "demo-workflow",
#'   version = "1.0.0",
#'   stages = c("idle", "working"),
#'   parameters = list(timeout = 300)
#' )
#' sp1 <- StatePolicy(
#'   name = "init", stage = "idle",
#'   description = "Initial state", agent_id = "agent_init"
#' )
#' sp2 <- StatePolicy(
#'   name = "process", stage = "working",
#'   description = "Processing state", agent_id = "agent_process"
#' )
#' manifest <- Manifest(master = mp, states = list(sp1, sp2))
#'
#' # Write to temporary file
#' tmp <- tempfile(fileext = ".yaml")
#' manifest_write(manifest, tmp)
#'
#' # Verify file exists
#' file.exists(tmp)
#'
#' # Read it back
#' manifest2 <- manifest_read(tmp)
#'
#' # Verify the name matches
#' manifest2@name == "demo-workflow"
#'
#' # Clean up
#' unlink(tmp)
#' @export
manifest_write <- function(x, file, ...) {
  li <- as.list(x, recursive = TRUE)
  txt <- yaml::as.yaml(x = li, ...)
  writeLines(txt, con = file)
  invisible(file)
}

#' @rdname manifest-file
#' @export
manifest_read <- function(file, ...) {
  li <- yaml::read_yaml(file = file, ..., readLines.warn = FALSE)
  master_policy <- MasterPolicy(
    name = li$master$name,
    description = concatern(
      li$master$description,
      trim_lines = TRUE,
      trim_collapsed = TRUE
    ),
    version = li$master$version,
    stages = li$master$stages,
    parameters = as.list(li$master$parameters)
  )
  state_policies <- lapply(li$states, function(state) {
    state$description <- concatern(
      state$description,
      trim_lines = TRUE,
      trim_collapsed = TRUE
    )
    state$parameters <- as.list(state$parameters)
    # Fix resources field: YAML may read empty arrays as list(),
    # convert to character()
    if (
      is.null(state$resources) ||
        (is.list(state$resources) && length(state$resources) == 0)
    ) {
      state$resources <- character(0L)
    } else {
      state$resources <- as.character(state$resources)
    }
    do.call(StatePolicy, state)
  })
  Manifest(master = master_policy, states = state_policies)
}
