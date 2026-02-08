#' @include class-policystate.R
NULL

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

    invalid_stages <- setdiff(state_stages, valid_stages)
    if (length(invalid_stages) > 0) {
      return(sprintf(
        "StatePolicies reference stages not defined in MasterPolicy. Invalid stages: %s",
        paste(sQuote(invalid_stages), collapse = ", ")
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

    # Validate explicit dependencies (depends_on)
    # 1. Check for duplicate state names (required for unambiguous dependency resolution)
    state_names <- vapply(self@states, function(s) s@name, character(1))
    if (anyDuplicated(state_names)) {
      dups <- unique(state_names[duplicated(state_names)])
      return(sprintf(
        "stat policies in manifest must have unique names. Duplicates found: %s",
        paste(sQuote(dups), collapse = ", ")
      ))
    }

    # Map names to policy objects for O(1) lookup
    state_map <- stats::setNames(self@states, state_names)
    stages <- self@master@stages

    for (state in self@states) {
      deps <- state@depends_on@deps
      if (length(deps) == 0) next

      # Resolve strictly by index in master@stages
      idx_current <- match(state@stage, stages)

      for (param_nm in names(deps)) {
        entry <- deps[[param_nm]]
        target_name <- entry$state

        # Check 1: Existence
        if (!target_name %in% state_names) {
          return(sprintf(
            "state %s depends on non-existent state %s (parameter %s).",
            sQuote(state@name), sQuote(target_name), sQuote(param_nm)
          ))
        }

        target_state <- state_map[[target_name]]
        idx_target <- match(target_state@stage, stages)

        # Check 2: Stage Mismatch (if specified)
        if (!is.null(entry$stage)) {
          if (entry$stage != target_state@stage) {
            return(sprintf(
              "state %s expects dependency %s to be in stage %s, but it is in %s.",
              sQuote(state@name),
              sQuote(target_name),
              sQuote(entry$stage),
              sQuote(target_state@stage)
            ))
          }
        }

        # Check 3: Execution Order
        # Target must run strictly before current state
        if (idx_target > idx_current) {
          return(sprintf(
            "invalid future dependency: state %s (stage %s) depends on %s (stage %s).", # nolint: line_length_linter.
            sQuote(state@name), state@stage,
            sQuote(target_name), target_state@stage
          ))
        } else if (idx_target == idx_current) {
          # Same stage: Target must have HIGHER priority
          if (target_state@priority < state@priority) {
            return(sprintf(
              "invalid same-stage dependency: state %s depends on %s, but dependency priority (%d) is not higher than dependent priority (%d).", # nolint: line_length_linter.
              sQuote(state@name), sQuote(target_name),
              target_state@priority, state@priority
            ))
          }
        }
        # Implicitly: if idx_target < idx_current, it's valid (past stage)
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
    # Fix depends_on field: YAML may read empty/missing as NULL,
    # convert to StateDeps object
    if (is.null(state$depends_on) || length(state$depends_on) == 0) {
      state$depends_on <- StateDeps()
    } else {
      # YAML reads as named list - convert to StateDeps
      state$depends_on <- StateDeps(.list = state$depends_on)
    }
    do.call(StatePolicy, state)
  })
  Manifest(master = master_policy, states = state_policies)
}
