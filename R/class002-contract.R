# Contract system for tricobbler
# Defines execution agreements (who, tools, validators) for workflow stages
# 
# Architecture:
# - S7 classes: Contract, StageContract, ContractExecutor (immutable definitions)
# - R6 classes: ContractState, SubContractState (mutable execution state)
#   See class-contractstate.R for R6 state classes

# ===========================================================================
# S7 CONTRACT DEFINITIONS (immutable)
# ===========================================================================

# ---------------------------------------------------------------------------
# ContractExecutor – callable function that executes a stage
# Wraps the actual execution logic (LLM call, script, etc.) as a function.
# It inherits from `S7::class_function` so the object itself is callable
# while still carrying additional attributes (name, description, metadata).
# ---------------------------------------------------------------------------
ContractExecutor <- S7::new_class(
  name = "ContractExecutor",
  parent = S7::class_function,
  properties = list(
    # Human‑readable name for this executor
    name = S7::new_property(
      class = S7::class_character,
      default = "",
      validator = function(value) {
        if (length(value) != 1) return("name must be a single string")
        return()
      }
    ),
    # Description of what this executor does
    description = S7::new_property(
      class = S7::class_character,
      default = ""
    ),
    # Arbitrary metadata (e.g., model name, command path, etc.)
    metadata = S7::new_property(
      class = S7::class_list,
      default = list()
    )
  )
)

# ---------------------------------------------------------------------
# OutputSchema – describes the expected shape of a stage's output.
# For simplicity we store a named list where each element is a character
# string describing the R type (e.g., "character", "numeric", "list").
# ---------------------------------------------------------------------
OutputSchema <- S7::new_class(
  name = "OutputSchema",
  properties = list(
    schema = S7::new_property(
      class = S7::class_list,
      default = list(),
      validator = function(value) {
        # Ensure we have a named list of character type descriptors
        if (!is.list(value)) return("schema must be a list")
        if (length(value) == 0) return()
        if (is.null(names(value)) || any(names(value) == "")) {
          return("all schema entries must be named")
        }
        ok <- vapply(value, function(v) is.character(v) && length(v) == 1, logical(1))
        if (!all(ok)) return("each schema entry must be a single character string")
        return()
      }
    )
  )
)

# ---------------------------------------------------------------------
# OutputValidator – encapsulates a validation function for stage output.
# The function should accept the stage output (and optionally the input) and
# return TRUE on success or a character error message.
# ---------------------------------------------------------------------
OutputValidator <- S7::new_class(
  name = "OutputValidator",
  properties = list(
    validate = S7::new_property(
      class = S7::class_function,
      default = function(output, input = NULL) TRUE,
      validator = function(value) {
        if (!is.function(value)) return("validator must be a function")
        return()
      }
    )
  )
)

# ---------------------------------------------------------------------------
# StageContract – defines the agreement for ONE workflow stage
# Specifies: who (executor), what tools, acceptance criteria
# ---------------------------------------------------------------------------
StageContract <- S7::new_class(
  name = "StageContract",
  properties = list(
    # Which stage this contract covers (must match a StagePolicy)
    stage_policy = S7::new_property(class = StagePolicy),
    
    # ========== WHO: The executor ==========
    # A callable ContractExecutor function that performs the work
    executor = S7::new_property(class = ContractExecutor),
    
    # ========== WITH WHAT: Tools & resources ==========
    # List of tools available to this stage (MCP tools, R functions, etc.)
    tools = S7::new_property(class = S7::class_list, default = list()),
    
    # Additional parameters for the executor
    parameters = S7::new_property(class = S7::class_list, default = list()),
    
    # ========== ACCEPTANCE CRITERIA: Quality gates ==========
    # Expected output schema (e.g., list(steps = "character", timeline = "Date"))
    output_schema = S7::new_property(class = S7::class_any, default = NULL),
    
    # Validation function: function(output, input) -> TRUE or error message
    # Note: validators can choose whether to use 'input' parameter
    validator = S7::new_property(class = S7::class_any, default = NULL),
    
    # ========== OPTIONAL: SLAs (orchestrator enforces) ==========
    max_retries = S7::new_property(class = S7::class_integer, default = 3L),
    timeout_seconds = S7::new_property(class = S7::class_numeric, default = NULL),
    
    # Fallback executor if this contract fails after all retries
    fallback = S7::new_property(class = S7::class_any, default = NULL)
  )
)

# ---------------------------------------------------------------------------
# Contract – the master user-orchestrator agreement
# Contains all stage contracts and validates completeness
# ---------------------------------------------------------------------------
Contract <- S7::new_class(
  name = "Contract",
  properties = list(
    # The workflow blueprint (defines all stages)
    manifest = S7::new_property(class = Manifest),
    
    # Stage contracts – one per workflow stage
    stage_contracts = S7::new_property(
      class = S7::class_list,
      validator = function(value) {
        ok <- vapply(value, function(x) S7::S7_inherits(x, StageContract), logical(1))
        if (!all(ok)) {
          return("all elements in 'stage_contracts' must be StageContract objects.")
        }
        return()
      }
    ),
    
    # Global resources available to all stages
    global_tools = S7::new_property(class = S7::class_list, default = list()),
    global_context = S7::new_property(class = S7::class_list, default = list()),
    
    # Master-level constraints (orchestrator enforces these)
    total_timeout_seconds = S7::new_property(class = S7::class_numeric, default = NULL),
    total_budget_dollars = S7::new_property(class = S7::class_numeric, default = NULL)
  ),
  validator = function(self) {
    # Every stage in the manifest must have a corresponding contract
    stage_states <- vapply(self@manifest@stages, function(s) s@state, character(1))
    contract_states <- vapply(self@stage_contracts, function(c) c@stage_policy@state, character(1))
    
    missing <- setdiff(stage_states, contract_states)
    if (length(missing) > 0) {
      return(sprintf(
        "Missing contracts for stages: %s",
        paste(sQuote(missing), collapse = ", ")
      ))
    }
    
    # No extra contracts for non-existent stages
    extra <- setdiff(contract_states, stage_states)
    if (length(extra) > 0) {
      return(sprintf(
        "Contracts reference non-existent stages: %s",
        paste(sQuote(extra), collapse = ", ")
      ))
    }
    
    return()
  }
)
