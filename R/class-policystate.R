#' @include class-s7base.R
#' @include class-statedeps.R
#' @include class-policymaster.R
#'
NULL



# ---------------------------------------------------------------------------
# StatePolicy - describes *what* a workflow state represents.  It contains the
# symbolic stage name (must match a MasterPolicy stage) and optional metadata
# such as a human-readable description and state-specific parameters.
# ---------------------------------------------------------------------------
#' @title State-Level Policy Implementation for Workflow Stages
#' @description Represents a single workflow state. Inherits from
#'   \code{\link{BasePolicy}} and adds a mandatory \code{stage} that must
#'   match one of the stages defined in a \code{\link{MasterPolicy}}.
#'   Includes priority and criticality flags for execution ordering when
#'   multiple states share the same stage.
#' @details
#' ## Stages vs States
#'
#' \itemize{
#'   \item \strong{Stage} (macro): The workflow phase name from
#'     \code{MasterPolicy@@stages} (e.g., \code{"executing"})
#'   \item \strong{State} (micro): A concrete \code{StatePolicy}
#'     implementation of that stage
#'   \item Multiple states can reference the same stage for different
#'     execution patterns
#' }
#'
#' ## Priority System and Execution Patterns
#'
#' When multiple states share the same stage, execution pattern depends on
#'   priority:
#'
#' \itemize{
#'   \item \strong{Range}: 0 (lowest) to 999 (highest)
#'   \item \strong{Default}: 100 (when \code{priority = NA} or \code{NULL})
#'   \item \strong{Execution order}: States run sequentially by priority
#'     (higher first)
#' }
#'
#' ## Critical Flag: Enforcing Sequential Execution
#'
#' The \code{critical} flag enforces \strong{sequential} execution with
#'   fail-fast semantics:
#'
#' \itemize{
#'   \item If \code{critical = TRUE}, this state \strong{must} execute and
#'     succeed before any lower-priority states in the same stage can run
#'   \item If a critical state fails, lower-priority states are skipped
#'     entirely
#'   \item Critical states must have unique priority (enforced by
#'     \code{\link{Manifest}})
#'   \item Critical states must have \code{priority >= 1} (cannot be lowest
#'     priority 0)
#'   \item Critical states cannot share their priority value with other
#'     states in the same stage (enforced by \code{\link{Manifest}}
#'     validation)
#'   \item \strong{Use case}: Required validation gates that must pass
#'     before alternatives run
#' }
#'
#' ## When to Use Multiple States Per Stage
#'
#' Create multiple \code{StatePolicy} objects for the same stage when you
#'   need:
#'
#' \enumerate{
#'   \item \strong{Fallback chains}: Different priorities create ordered
#'     execution with alternative strategies
#'   \item \strong{Alternative implementations}: Multiple states for the
#'     same stage (e.g., primary approach, then fallback, then last resort)
#'   \item \strong{Critical validation gates}: A critical state must
#'     succeed before lower-priority alternatives execute (enforces
#'     sequential, fail-fast semantics)
#'   \item \strong{Phased deployment}: Gradually shift priority as new
#'     implementations mature
#' }
#'
#' ## Parameters for Different Agent Types
#'
#' The \code{parameters} list is interpreted differently based on the agent
#' type created via \code{\link{as_agent}}:
#'
#' \strong{Deterministic Agents} (from functions or \verb{MCP} tools):
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
#' \strong{AI Agents} (from \pkg{ellmer} \code{Chat} objects):
#' \itemize{
#'   \item \code{system_prompt}: character, additional system prompt
#'     appended to \code{@@description}
#'   \item \code{user_prompt}: character, the user message
#'     to send to the \verb{LLM}
#'   \item \code{keep_turns}: logical, if \code{TRUE}, retains conversation
#'     history across executions (default \code{FALSE})
#'   \item \code{return_type}: an \pkg{ellmer} type indicator (e.g.,
#'     \code{ellmer::type_object()}). When provided, triggers
#'     \code{chat$chat_structured(type = return_type)} for structured output.
#'     Can also be specified in \verb{YAML} via
#'     \code{\link{map_type_to_ellmer}()}
#' }
#'
#' @param name character, name of the state policy (non-blank)
#' @param description character, human-readable description
#' @param stage character, must be a non-blank single string
#' @param parameters list, state-specific parameters passed to the agent.
#'   See **Parameters for Different Agent Types** in Details
#' @param priority integer, execution priority (0-999, default 100). Higher
#'   values run first (999 = highest priority, 0 = lowest). Used when multiple
#'   states share the same stage. \code{NA} or \code{NULL} are treated as 100
#' @param critical logical, if \code{TRUE}, states with lower priority will not
#'   execute if this state fails (default \code{FALSE}). Critical states must
#'   have \code{priority >= 1} and cannot share priority with other states
#'   in the same stage
#' @param agent_id character, unique identifier for the agent responsible for
#'   executing this state. Must contain only letters, digits, underscores, or
#'   dashes
#' @param resources character vector, tools that the agent may call during
#'   execution (default empty vector)
#' @param accessibility character, context accessibility level for the agent.
#'   Controls what context data the agent can read. One of \code{"all"}
#'   (full access to previous results and logs), \code{"logs"} (log
#'   descriptions only, no raw results), \code{"none"} (no access to
#'   previous context), or \code{"explicit"} (use \code{depends_on} to
#'   specify inputs; if \code{depends_on} is empty, behaves like
#'   \code{"logs"}).
#'   Default is \code{"all"}
#' @param depends_on \code{StateDeps} object or named list specifying explicit
#'   dependencies on prior state outputs. Each entry maps a parameter name to
#'   a dependency: \code{list(param = list(state = "state_name", field =
#'   "result", stage = NULL))}. Used with \code{accessibility = "explicit"}
#'   for \verb{async} execution. See \code{\link{StateDeps}} for format details
#' @param max_retry integer, maximum total attempts (initial + retries) for
#'   this state during stage execution (default 0, meaning single attempt).
#'   The \code{max_retry} limit applies globally across all re-entries to this
#'   state within the same stage. If \code{on_failure} is set, this state will
#'   not retry locally but will jump to the failure handler immediately. If
#'   \code{on_failure} is \code{NA}, local retries up to \code{max_retry}
#'   will be attempted before moving to next state
#' @param final logical, if \code{TRUE} and the agent succeeds, skip remaining
#'   states in the stage (default \code{FALSE})
#' @param on_failure character, name of the state to jump to on first failure
#'   (default \code{NA} to retry locally up to \code{max_retry} times). When
#'   set, failures trigger immediate jump to the specified state without local
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
    # "explicit" means use depends_on to specify inputs; if depends_on is
    # empty, behaves like "logs"
    accessibility = S7::new_property(
      class = S7::class_character,
      validator = function(value) {
        if (length(value) != 1 || is.na(value)) {
          return("accessibility must be a single character value.")
        }
        valid_choices <- c("all", "logs", "none", "explicit")
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
    ),

    # Explicit dependencies for async execution
    # Named list mapping parameter names to prior state outputs
    # See StateDeps for format details
    depends_on = S7::new_property(
      class = StateDeps,
      default = quote(StateDeps())
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

