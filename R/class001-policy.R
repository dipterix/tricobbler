# ---------------------------------------------------------------------------
# DESIGN LOGIC: Three-Tier Policy System
# ---------------------------------------------------------------------------
# This file defines the immutable policy layer (Tier 1) of TriCobbler's
# architecture. Policies are blueprints that specify WHAT a workflow should
# do, not HOW it executes (which is handled by Contract/ContractState layers).
#
# The three-tier separation is:
#   1. Policy (this file) - Immutable workflow blueprints using S7
#   2. Contract (class002) - Immutable execution agreements using S7
#   3. ContractState (class003) - Mutable runtime tracking using R6
#
# KEY DESIGN DECISIONS:
#
# 1. Stages vs States:
#    - STAGES are symbolic phase names (e.g., "triage", "planning") defined
#      in MasterPolicy. They represent the vocabulary of workflow phases.
#    - STATES are concrete policy implementations (StatePolicy objects) that
#      reference stages and add metadata (description, parameters).
#    - Multiple states can share the same stage, enabling parallel workflows.
#    - Every stage MUST have at least one state (enforced by Manifest validator).
#
# 2. Manifest as Validated Blueprint:
#    - Manifest ties MasterPolicy to StatePolicy list, ensuring completeness.
#    - The validator performs cross-validation: all master stages must have
#      at least one corresponding state policy.
#    - This ensures no "orphaned" stages exist in the workflow definition.
#    - Note: Manifest is NOT a Contract - it's a policy container. The actual
#      execution contract (with executors, validators, etc.) is defined in
#      the Contract class (class002-contract.R).
#
# 3. Immutability via S7:
#    - Policies are immutable to prevent accidental modification during execution.
#    - Once created, a policy serves as a stable reference for contracts.
#    - Use S7 @ accessor (e.g., policy@name) not R6 $ accessor.
#
# 4. Serialization (manifest_read/manifest_write):
#    - Policies can be saved as YAML for version control and sharing.
#    - YAML format enables human-readable workflow definitions.
#    - Deserialization validates all constraints automatically.
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
BasePolicy = S7::new_class(
  name = "Policy",
  parent = BaseClass,
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

#' @title Master Workflow Policy Defining Stages and Version
#' @description Concrete policy that defines the overall workflow version and the
#'   set of allowed stages. Inherits from `BasePolicy`.
#' @details
#' ## Stages as Workflow Vocabulary
#'
#' The `stages` property defines the symbolic vocabulary of workflow phases
#' (e.g., "triage", "planning", "executing"). These are macro-level phase names
#' that must be implemented by at least one `StatePolicy` object in the `Manifest`.
#'
#' ## Stage Naming Conventions
#'
#' - Stages are automatically lowercased for consistency
#' - Must contain only letters (a-z), digits (0-9), underscores (_), or dashes (-)
#' - Must be unique (case-insensitive) within a workflow
#' - Cannot be blank or NA
#'
#' ## Immutability
#'
#' `MasterPolicy` objects are immutable (S7 value semantics). Once created, they
#' serve as a stable reference for `Contract` objects. Use the `@` accessor to
#' read properties (e.g., `policy@stages`).
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
        if(length(value) == 0) { return("number of stages cannot be 0") }
        value <- tolower(value)
        if(anyNA(value) || anyDuplicated(value) || !all(nzchar(value))) {
          return("stage string cannot be duplicated (case-insensitive), NA, or blank.")
        }
        if(any(grepl("[^a-zA-Z0-9_-]", value))) {
          return("stage string can only contain letters (a-z), digits (0-9), underscore (_) or dash (-).")
        }
        return()
      }
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
#'   `MasterPolicy`. Includes priority and criticality flags for execution ordering
#'   when multiple states share the same stage.
#' @details
#' ## Stages vs States
#'
#' - **Stage** (macro): The workflow phase name from `MasterPolicy@stages` (e.g., "executing")
#' - **State** (micro): A concrete `StatePolicy` implementation of that stage
#' - Multiple states can reference the same stage for different execution patterns
#'
#' ## Priority System and Execution Patterns
#'
#' When multiple states share the same stage, execution pattern depends on priority:
#'
#' - **Range**: 0 (lowest) to 999 (highest)
#' - **Default**: 100 (when `priority = NA` or `NULL`)
#' - **Equal priority**: States run in parallel (truly concurrent execution)
#' - **Different priorities**: States run sequentially (higher priority first)
#'
#' ## Critical Flag: Enforcing Sequential Execution
#'
#' The `critical` flag enforces **sequential** execution with fail-fast semantics:
#'
#' - If `critical = TRUE`, this state **must** execute and succeed before any
#'   lower-priority states in the same stage can run
#' - If a critical state fails, lower-priority states are skipped entirely
#' - Critical states **prevent parallel execution** - you cannot have parallel
#'   states when one is marked critical (enforced by priority uniqueness)
#' - Critical states must have `priority >= 1` (cannot be lowest priority 0)
#' - Critical states cannot share their priority value with other states in the
#'   same stage (enforced by `Manifest` validator)
#' - **Use case**: Required validation gates that must pass before alternatives run
#'
#' ## When to Use Multiple States Per Stage
#'
#' Create multiple `StatePolicy` objects for the same stage when you need:
#'
#' 1. **Parallel alternatives**: Multiple states with equal priority for concurrent
#'    execution (e.g., A/B testing, redundant processing)
#' 2. **Sequential fallback chains**: Different priorities create ordered execution
#'    with fallbacks (e.g., primary approach → fallback → last resort)
#' 3. **Critical validation gates**: Critical state must succeed before lower-priority
#'    alternatives execute (enforces sequential, fail-fast semantics)
#' 4. **Staged rollout**: Gradually shift priority as new implementations mature
#'
#' @param name Character. Name of the state policy (non-blank).
#' @param description Character. Human-readable description.
#' @param stage Character. Must be a non-blank single string.
#' @param parameters List. Optional state-specific parameters.
#' @param priority Integer. Execution priority (0-999, default 100). Higher values
#'   run first (999 = highest priority, 0 = lowest). Used when multiple states
#'   share the same stage. NA or NULL are treated as 100.
#' @param critical Logical. If `TRUE`, states with lower priority won't
#'   execute if this state fails (default `FALSE`). Critical states must have
#'   `priority >= 1` and cannot share priority code with other states.
#' @examples
#'
#' # Basic state
#' StatePolicy(
#'   name = "state1",
#'   stage = "idle",
#'   description = "initial idle state",
#'   parameters = list()
#' )
#'
#' # Critical high-priority state
#' StatePolicy(
#'   name = "validator",
#'   stage = "executing",
#'   description = "critical validation step",
#'   priority = 900,
#'   critical = TRUE
#' )
#'
#' @export
StatePolicy <- S7::new_class(
  name = "StatePolicy",
  parent = BasePolicy,
  properties = list(
    # The symbolic name of the state – must correspond to a MasterPolicy stage
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
    description = S7::new_property(class = S7::class_character),
    # Optional list of state-specific parameters (e.g., timeout, retries)
    parameters = S7::new_property(class = S7::class_list),
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
    # Critical flag: if TRUE, lower-priority stages won't run if this fails
    critical = S7::new_property(
      class = S7::class_logical,
      default = FALSE
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
#' @description Container that ties a `MasterPolicy` together with a list of
#'   `StatePolicy` objects. The validator ensures that every stage defined in
#'   the master policy is represented by at least one state.
#' @details
#' ## TriCobbler's Three-Tier Architecture
#'
#' The `Manifest` class represents the validated blueprint layer (Tier 1) in
#' TriCobbler's three-tier design:
#'
#' 1. **Policy Layer (Tier 1 - Immutable S7)**: Blueprint definitions
#'    - `Manifest`: Validated container linking `MasterPolicy` to `StatePolicy` list
#'    - `MasterPolicy`: Workflow version + allowed stages (macro-level phases)
#'    - `StatePolicy`: Individual state metadata (stage, description, parameters, priority)
#'
#' 2. **Contract Layer (Tier 2 - Immutable S7)**: Execution agreements
#'    - `Contract`: Master agreement linking `Manifest` to `StageContract` list
#'    - `StageContract`: Per-stage execution spec (executor, tools, validators)
#'    - `ContractExecutor`: Callable function wrapping LLM/script execution logic
#'
#' 3. **State Layer (Tier 3 - Mutable R6)**: Runtime tracking
#'    - `ContractState`: Orchestrates all stage states and workflow progression
#'    - `SubContractState`: Tracks one stage's execution (status, I/O, timing)
#'
#' ## Stages vs States: Critical Distinction
#'
#' - **Stages** (symbolic vocabulary): Workflow phase names defined in `MasterPolicy@stages`
#'   (e.g., "triage", "planning", "executing")
#' - **States** (concrete implementations): `StatePolicy` objects that reference
#'   stages and add execution metadata (description, parameters, priority)
#' - **Multiple states per stage**: Enables different execution patterns:
#'   - **Parallel**: Multiple states with equal priority (concurrent execution)
#'   - **Sequential**: Different priorities create ordered execution chains
#'   - **Critical gates**: Critical states enforce sequential fail-fast semantics
#' - **Validation rule**: Every stage in `MasterPolicy@stages` MUST have at least
#'   one corresponding `StatePolicy` (enforced by validator)
#'
#' ## Validation Rules
#'
#' The `Manifest` validator performs critical cross-checks:
#'
#' 1. **Completeness**: Every `MasterPolicy` stage has at least one `StatePolicy`
#'    - Prevents "orphaned" stages with no implementation
#'    - Error message: "Missing stages: ..."
#'
#' 2. **Critical priority uniqueness**: Critical states cannot share priorities
#'    - If `StatePolicy@critical = TRUE`, no other state in the same stage can
#'      have the same `priority` value
#'    - Prevents ambiguity about which critical state blocks lower-priority states
#'    - Error message: "Critical state ... cannot share its priority with ..."
#'
#' ## Immutability and Serialization
#'
#' Once created, `Manifest` objects are immutable (S7 value semantics), providing
#' a stable reference for `Contract` creation. Manifests can be serialized to/from
#' YAML for version control:
#'
#' - `manifest_write(manifest, file)`: Save to human-readable YAML
#' - `manifest_read(file)`: Load with full validation
#' - All validation rules apply on deserialization
#'
#' ## Manifest vs Contract
#'
#' **Important**: `Manifest` is NOT a `Contract`. It's purely a policy-level
#' blueprint that defines WHAT the workflow should do (stages and states), not
#' HOW it executes. The actual execution contract (with executors, validators,
#' tools, SLAs) is defined separately in the `Contract` class.
#'
#' @param master `MasterPolicy` object.
#' @param states List of `StatePolicy` objects.
#' @examples
#' # Create a valid manifest
#' mp <- MasterPolicy(name = "example", version = "1.0.0",
#'                    stages = c("idle", "triage"), parameters = list())
#' sp1 <- StatePolicy(name = "state1", stage = "idle", description = "idle state")
#' sp2 <- StatePolicy(name = "state2", stage = "triage", description = "triage state")
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
    # List of StatePolicy objects – one per workflow state
    states = S7::new_property(
      class = S7::class_list,
      validator = function(value) {
        ok <- vapply(value, function(x) S7::S7_inherits(x, StatePolicy), logical(1))
        if (!all(ok)) {
          return("all elements in 'states' must be `StatePolicy` objects.")
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
        "Each stage defined in MasterPolicy must be implemented with one or more StatePolicies. Missing stages: %s",
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
              "Critical state %s (stage=%s, priority=%d) cannot share its priority with other states in the same stage: %s",
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
      sprintf("Manifest (S7 class) - `%s` (%s)", x@name, x@master@version), "",
      sprintf("Master policy stages: \n  %s", paste(x@master@stages, collapse = ", ")), "",
      unlist(stage_str)
    )
  )
}

#' @name manifest-file
#' @title Read or Write Manifest from or to a YAML File
#' @description
#' Serialize and deserialize `Manifest` objects to and from YAML files. This
#' enables version control and sharing of workflow policy definitions. The
#' resulting YAML files are human-readable and can be edited manually, though
#' changes must still pass validation when read back.
#'
#' `manifest_write()` serializes a `Manifest` object to a YAML file.
#'
#' `manifest_read()` deserializes a YAML file back into a validated `Manifest`
#' object, reconstructing the `MasterPolicy` and `StatePolicy` objects and
#' enforcing all validation rules (e.g., every stage must have a corresponding
#' state).
#' @param x A `Manifest` object to serialize (for `manifest_write`).
#' @param file Character. Path to the YAML file (input for `manifest_read`,
#'   output for `manifest_write`).
#' @param ... Additional arguments passed to `yaml::read_yaml()` or
#'   `yaml::write_yaml()`.
#' @return
#' - `manifest_write()`: Invisibly returns the path to the written file.
#' - `manifest_read()`: A validated `Manifest` object.
#' @examples
#' # Create a manifest
#' mp <- MasterPolicy(
#'   name = "demo-workflow",
#'   version = "1.0.0",
#'   stages = c("idle", "working"),
#'   parameters = list(timeout = 300)
#' )
#' sp1 <- StatePolicy(name = "init", stage = "idle",
#'                    description = "Initial state")
#' sp2 <- StatePolicy(name = "process", stage = "working",
#'                    description = "Processing state")
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
    do.call(StatePolicy, state)
  })
  Manifest(master = master_policy, states = state_policies)
}
