# Contract state management for tricobbler
# R6 classes that track mutable execution state during workflow execution
# 
# Architecture:
# - ContractState: Grand state holder for entire contract execution
# - SubContractState: Tracks execution state for one stage
# 
# These R6 classes are mutable and manage runtime state, while the Contract
# and StageContract S7 classes (in class-contract.R) are immutable definitions.

# ===========================================================================
# R6 STATE CLASSES (mutable execution state)
# ===========================================================================

# ---------------------------------------------------------------------------
# SubContractState – tracks execution state for one workflow stage
# ---------------------------------------------------------------------------
SubContractState <- R6::R6Class(
  classname = "SubContractState",
  public = list(
    # Reference to the stage contract definition (S7 StageContract)
    stage_contract = NULL,
    
    # Link back to the grand ContractState
    parent_state = NULL,
    
    # Execution state
    status = "pending",  # pending, running, completed, failed, retrying
    attempts = 0L,
    max_attempts = 3L,
    
    # Input/output data
    input = NULL,
    output = NULL,
    error = NULL,
    
    # Timing information
    start_time = NULL,
    end_time = NULL,
    
    # Validation results
    validation_passed = NULL,
    validation_message = NULL,
    
    initialize = function(stage_contract, parent_state = NULL) {
      self$stage_contract <- stage_contract
      self$parent_state <- parent_state
      self$max_attempts <- stage_contract@max_retries
      invisible(self)
    },
    
    start = function(input) {
      self$status <- "running"
      self$input <- input
      self$start_time <- Sys.time()
      self$attempts <- self$attempts + 1L
      invisible(self)
    },
    
    complete = function(output) {
      self$output <- output
      self$end_time <- Sys.time()
      
      # Run validation if validator exists
      if (!is.null(self$stage_contract@validator)) {
        result <- tryCatch({
          self$stage_contract@validator(output, self$input)
        }, error = function(e) {
          paste("Validator error:", e$message)
        })
        
        if (isTRUE(result)) {
          self$validation_passed <- TRUE
          self$status <- "completed"
        } else {
          self$validation_passed <- FALSE
          self$validation_message <- as.character(result)
          self$status <- "failed"
        }
      } else {
        # No validator – assume success
        self$validation_passed <- TRUE
        self$status <- "completed"
      }
      
      invisible(self)
    },
    
    fail = function(error) {
      self$error <- error
      self$end_time <- Sys.time()
      
      if (self$attempts < self$max_attempts) {
        self$status <- "retrying"
      } else {
        self$status <- "failed"
      }
      
      invisible(self)
    },
    
    can_retry = function() {
      self$status == "retrying" && self$attempts < self$max_attempts
    },
    
    duration = function() {
      if (!is.null(self$start_time) && !is.null(self$end_time)) {
        difftime(self$end_time, self$start_time, units = "secs")
      } else {
        NULL
      }
    }
  )
)

# ---------------------------------------------------------------------------
# ContractState – grand state holder for entire contract execution
# Manages execution state for all stages and coordinates workflow
# ---------------------------------------------------------------------------
ContractState <- R6::R6Class(
  classname = "ContractState",
  public = list(
    # Reference to the contract definition (S7 Contract)
    contract = NULL,
    
    # Sub-states for each stage (named list: state_name -> SubContractState)
    stage_states = NULL,
    
    # Overall execution state
    status = NULL,
    current_stage = NULL,
    
    # Timing
    start_time = NULL,
    end_time = NULL,
    
    # Results accumulator
    results = NULL,
    
    initialize = function(contract, input = NULL, metadata = NULL) {
      self$contract <- contract
      self$status <- "pending"
      self$results <- list()
      
      # Create SubContractState for each stage
      self$stage_states <- list()
      for (sc in contract@stage_contracts) {
        state_name <- sc@state_policy@stage
        self$stage_states[[state_name]] <- SubContractState$new(sc, parent_state = self)
      }
      
      # Store initial input/metadata in global context
      if (!is.null(input)) {
        self$contract@global_context$input <- input
      }
      if (!is.null(metadata)) {
        self$contract@global_context$metadata <- metadata
      }
      
      invisible(self)
    },
    
    start = function() {
      self$status <- "running"
      self$start_time <- Sys.time()
      invisible(self)
    },
    
    get_stage_state = function(stage_name) {
      self$stage_states[[stage_name]]
    },
    
    set_current_stage = function(stage_name) {
      self$current_stage <- stage_name
      invisible(self)
    },
    
    total_duration = function() {
      if (!is.null(self$start_time) && !is.null(self$end_time)) {
        difftime(self$end_time, self$start_time, units = "secs")
      } else {
        NULL
      }
    },
    
    summary = function() {
      list(
        status = self$status,
        current_stage = self$current_stage,
        duration = self$total_duration(),
        stage_summary = lapply(self$stage_states, function(s) {
          list(
            status = s$status,
            attempts = s$attempts,
            validation_passed = s$validation_passed,
            duration = s$duration()
          )
        })
      )
    },
    
    complete = function(status = "completed") {
      self$status <- status
      self$end_time <- Sys.time()
      invisible(self)
    }
  )
)
