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
# ContractExecutor - callable function that executes a stage
# Wraps the actual execution logic (LLM call, script, etc.) as a function.
# It inherits from `S7::class_function` so the object itself is callable
# while still carrying additional attributes (name, description, metadata).
# ---------------------------------------------------------------------------
#' @title ContractExecutor
#' @description Callable function that executes a stage. It wraps the actual execution
#'   logic (LLM call, script, etc.) as a function. Inherits from `S7::class_function`
#'   so the object itself is callable while still carrying additional attributes.
#' @param .data Internal data argument injected by S7 (unused).
#' @param name Name of the executor (character, length 1).
#' @param description Description of the executor (character).
#' @param metadata List of arbitrary metadata for the executor.
#' @export
ContractExecutor <- S7::new_class(
  name = "ContractExecutor",
  parent = S7::class_function,
  properties = list(
    # Human-readable name for this executor
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
# OutputSchema - describes the expected shape of a stage's output.
# For simplicity we store a named list where each element is a character
# string describing the R type (e.g., "character", "numeric", "list").
# ---------------------------------------------------------------------
#' @title OutputSchema
#' @description Describes the expected shape of a stage's output. It stores a named list
#'   where each element is a character string describing the R type (e.g., "character",
#'   "numeric", "list").
#' @param schema Named list where each element is a single character string describing the R type of the corresponding output.
#' @export
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
# OutputValidator - encapsulates a validation function for stage output.
# The function should accept the stage output (and optionally the input) and
# return TRUE on success or a character error message.
# ---------------------------------------------------------------------
#' @title OutputValidator
#' @description Encapsulates a validation function for stage output. The function should
#'   accept the stage output (and optionally the input) and return `TRUE` on success or a
#'   character error message.
#' @param validate Function that validates the stage output; should return TRUE or an error message.
#' @export
OutputValidator <- S7::new_class(
  name = "OutputValidator",
  properties = list(
    validate = S7::new_property(
      class = S7::class_function,
      validator = function(value) {
        if (!is.function(value)) return("validator must be a function")
        return()
      }
    )
  )
)

# ---------------------------------------------------------------------------
# StageContract - defines the agreement for ONE workflow stage
# Specifies: who (executor), what tools, acceptance criteria
# ---------------------------------------------------------------------------
#' @title StageContract
#' @description Defines the agreement for a single workflow stage, specifying who (executor),
#'   what tools, and acceptance criteria.
#' @param state_policy `StatePolicy` object defining the state.
#' @param executor `ContractExecutor` object that runs the stage.
#' @param tools List of tools available to the stage.
#' @param parameters List of additional parameters for the executor.
#' @param output_schema `OutputSchema` describing expected output.
#' @param validator Function to validate output (optional).
#' @param max_retries Integer, max retry attempts.
#' @param timeout_seconds Numeric, timeout for the stage.
#' @param fallback Optional fallback `ContractExecutor`.
#' @export
StageContract <- S7::new_class(
  name = "StageContract",
  properties = list(
    # Which state this contract covers (must match a StatePolicy)
    state_policy = S7::new_property(class = StatePolicy),
    
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
# Contract - the master user-orchestrator agreement
# Contains all stage contracts and validates completeness
# ---------------------------------------------------------------------------
#' @title Contract
#' @description Master user-orchestrator agreement that contains all stage contracts and
#'   validates completeness against the manifest.
#' @param manifest `Manifest` object describing the workflow.
#' @param stage_contracts List of `StageContract` objects.
#' @param global_tools List of tools available globally.
#' @param global_context List of global context items.
#' @param total_timeout_seconds Numeric total timeout for the contract.
#' @param total_budget_dollars Numeric total budget for the contract.
#' @export
Contract <- S7::new_class(
  name = "Contract",
  properties = list(
    # The workflow blueprint (defines all stages)
    manifest = S7::new_property(class = Manifest),
    
    # Stage contracts - one per workflow stage
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
    state_stages <- vapply(self@manifest@states, function(s) s@stage, character(1))
    contract_stages <- vapply(self@stage_contracts, function(c) c@state_policy@stage, character(1))
    
    missing <- setdiff(state_stages, contract_stages)
    if (length(missing) > 0) {
      return(sprintf(
        "Missing contracts for stages: %s",
        paste(sQuote(missing), collapse = ", ")
      ))
    }
    
    # No extra contracts for non-existent stages
    extra <- setdiff(contract_stages, state_stages)
    if (length(extra) > 0) {
      return(sprintf(
        "Contracts reference non-existent stages: %s",
        paste(sQuote(extra), collapse = ", ")
      ))
    }
    
    return()
  }
)
