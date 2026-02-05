#' @include mcp-describe.R
#' @include class-s7base.R
NULL

# ---------------------------------------------------------------------------
# Helper: map_type_to_ellmer
# ---------------------------------------------------------------------------
#' Convert Type Definitions to ellmer Type Indicators
#'
#' @description Converts JSON Schema type definitions (from YAML or lists) to
#'   \pkg{ellmer} type indicator objects for use with
#'   \code{chat$chat_structured()}.
#'
#' @param type_def A list representing a JSON Schema type definition, or a
#'   character string specifying a simple type (e.g., "string", "integer").
#'   Supported types: "string", "integer", "number", "boolean", "array",
#'   "object". Arrays can specify \code{items} for element types. Objects can
#'   specify \code{properties} for nested structure.
#'
#' @return An \pkg{ellmer} type indicator object (e.g.,
#'   \code{ellmer::type_string()}, \code{ellmer::type_object()}).
#'
#' @examples
#' \dontrun{
#' # Simple string type
#' map_type_to_ellmer("string")
#'
#' # Complex object type from YAML-like definition
#' map_type_to_ellmer(list(
#'   type = "object",
#'   properties = list(
#'     name = list(type = "string", description = "User name"),
#'     age = list(type = "integer", description = "User age")
#'   )
#' ))
#' }
#'
#' @seealso \code{\link{StatePolicy}} for how \code{return_type} uses this
#'   function, \code{\link[ellmer]{type_string}} and related functions for
#'   \pkg{ellmer} type indicators
#' @export
map_type_to_ellmer <- function(type_def) {
  # Handle character shorthand (e.g., "string", "integer")
  if (is.character(type_def)) {
    type_def <- list(type = type_def)
  }

  if (!is.list(type_def)) {
    stop("`type_def` must be a list or character string")
  }

  type <- type_def$type %||% "string"
  desc <- type_def$description %||% ""

  # Handle enum types first (special case)
  if (!is.null(type_def$enum)) {
    return(ellmer::type_enum(
      values = as.character(type_def$enum),
      description = desc
    ))
  }

  # Handle required field (defaults to TRUE)
  required <- type_def$required %||% TRUE

  # Map JSON Schema types to ellmer types
  switch(
    type,
    "string" = ellmer::type_string(description = desc, required = required),
    "integer" = ellmer::type_integer(description = desc, required = required),
    "number" = ellmer::type_number(description = desc, required = required),
    "boolean" = ellmer::type_boolean(description = desc, required = required),
    "array" = {
      args <- list(description = desc, required = required)
      if (!is.null(type_def$items)) {
        item_def <- type_def$items
        if (is.null(item_def$description)) {
          item_def$description <- ""
        }
        args$items <- map_type_to_ellmer(item_def)
      } else {
        # Default to string items if not specified

        args$items <- ellmer::type_string()
      }
      do.call(ellmer::type_array, args)
    },
    "object" = {
      # Build named properties for type_object
      props <- list()
      if (!is.null(type_def$properties)) {
        for (prop_name in names(type_def$properties)) {
          prop_def <- type_def$properties[[prop_name]]
          if (is.null(prop_def$description)) {
            prop_def$description <- ""
          }
          props[[prop_name]] <- map_type_to_ellmer(prop_def)
        }
      }
      # type_object uses .description and ... for properties
      do.call(ellmer::type_object, c(
        list(.description = desc, .required = required),
        props
      ))
    },
    # Fallback to string for unknown types
    ellmer::type_string(description = desc, required = required)
  )
}


#' @title Agent Function Wrapper for State Execution
#' @description Creates an executable agent that can be registered with a
#'   \code{Scheduler} to execute state-specific logic. The agent wraps a
#'   user-defined function with metadata (id, description) and result
#'   formatting. Agents are the execution units in the Runtime Layer
#'   (Tier 2) that implement the logic defined in \code{StatePolicy}
#'   objects from the Policy Layer (Tier 1).
#' @details
#' ## Agent Function Signature
#'
#' The wrapped function must have the following signature:
#' \preformatted{
#' function(self, policy, context, ...) {
#'   # Agent implementation
#'   # Return value will be logged to context via @describe
#' }
#' }
#'
#' **Required arguments:**
#' - \code{self}: Reference to the agent object itself
#' - \code{policy}: The \code{StatePolicy} object being executed
#' - \code{context}: The \code{Context} object for logging and state access
#' - \code{...}: Additional arguments (optional)
#'
#' ## Agent Execution Flow
#'
#' When a \code{Scheduler} executes a state:
#' 1. Looks up the agent by \code{StatePolicy@@agent_id}
#' 2. Calls the agent function with \code{(self, policy, context)}
#' 3. Captures the return value
#' 4. Uses \code{describe} function to format result for logging
#' 5. Logs formatted result to \code{Context}
#'
#' ## Result Description
#'
#' The \code{describe} property controls how results are formatted for
#' AI-readable logs. By default, uses \code{\link{mcp_describe}} which
#' captures the printed results. The \code{describe} property serves
#' three purposes: first, the function allows the agent to format the
#' output for better AI-readability. The formatted results will be logged
#' into a public log file along with the scheduling messages;
#' second, the agent can choose to redact sensitive information and avoid
#' insecure agents to access those data - reading attachments
#' requires permissions and those agents (according to how they are implemented)
#' should not access the attachments; finally, because the output description
#' will be recorded into the public log file so the other agents can still refer
#' to the context without reading the corresponding attachment file. For example
#' If a local agent's the output contains user-sensitive data, its
#' \code{describe} function may hide those information by string replacement,
#' or simply show the data format and schema. Other online-agents may still see
#' the redacted outputs from the log files and work on the output (such as
#' writing code or executing the tools).
#'
#' @param .data function, the agent implementation with signature
#'   \code{function(self, policy, context, ...)}
#' @param id character, unique identifier for the agent (letters, digits,
#'   underscores, dashes only)
#' @param description character, human-readable description of what the
#'   agent does
#' @param describe function or NULL, result formatting function for logging;
#'   defaults to \code{\link{mcp_describe}}. If provided, should take result
#'   as first argument and return a character string
#' @returns An \code{Agent} object (S7 class inheriting from \code{function})
#' @examples
#'
#'
#' # Basic agent
#' simple_agent <- Agent(
#'   function(self, policy, context) {
#'     context$logger("Executing simple task")
#'     return("Task completed")
#'   },
#'   id = "simple_agent",
#'   description = "A simple demonstration agent"
#' )
#'
#'
#' # Agent with custom result formatting function
#' analysis_agent <- Agent(
#'   function(self, policy, context) {
#'     result <- list(
#'       status = "success",
#'       items_processed = 42,
#'       timestamp = Sys.time(),
#'       long_result = rnorm(100),
#'       sensitive_data = "my password"
#'     )
#'     return(result)
#'   },
#'   id = "analysis_agent",
#'   description = "Performs data analysis",
#'   describe = function(result) {
#'     result$sensitive_data <- "<password redacted...>"
#'     c(
#'       sprintf("Processed %d items at %s.\n",
#'               result$items_processed, result$timestamp),
#'       "Here are the snapshots with R str():",
#'       utils::capture.output(str(result))
#'     )
#'   }
#' )
#'
#' # run these agents
#' manifest <- Manifest(
#'   master = MasterPolicy(
#'     name = "example",
#'     version = "0.0.1",
#'     stages = "executing"
#'   ),
#'   states = list(
#'     StatePolicy(name = "simply_policy",
#'                 stage = "executing",
#'                 agent_id = "simple_agent"),
#'     StatePolicy(name = "analysis_policy",
#'                 stage = "executing",
#'                 agent_id = "analysis_agent")
#'   )
#' )
#'
#' scheduler <- Scheduler$new(manifest = manifest,
#'                            agents = list(simple_agent, analysis_agent))
#'
#' if(interactive()) {
#'   scheduler$start()
#'   # TRACE 10:53:15 [scheduler]: Initializing resources
#'   # TRACE 10:53:15 [scheduler]: current stage: executing
#'   # TRACE 10:53:15 [scheduler]: starting state: simply_policy with
#'   #  agent simple_agent (attempt 0)
#'   # INFO 10:53:15 [Agent simple_agent]: Executing simple task
#'   # INFO 10:53:15 [context]: Following result recorded:
#'   #  Agent=simple_agent, stage=executing,
#'   #  state=simply_policy, attempt=0,
#'   #  identifier=[executing][simply_policy][simple_agent]_20T105315_0
#'   # INFO 10:53:15 [context]: "Task completed"
#'   # TRACE 10:53:15 [scheduler]: starting state: analysis_policy with
#'   #  agent analysis_agent (attempt 0)
#'   # INFO 10:53:15 [context]: Following result recorded:
#'   #  Agent=analysis_agent, stage=executing,
#'   #  state=analysis_policy, attempt=0,
#'   #  identifier=[executing][analysis_policy][analysis_agent]_20T105315_0
#'   # INFO 10:53:15 [context]: Processed 42 items at 10:53:15.
#'   # INFO 10:53:15 [context]: Here are the snapshots with R str():
#'   #  List of 5 $ status :
#'   #    chr "success" $ items_processed: num 42
#'   #    $ timestamp : POSIXct[1:1], format: "2026-02-03 10:53:15"
#'   #    $ long_result : num [1:100] 0.372 0.504 -1.584 -0.728 0.18 ...
#'   #    $ sensitive_data : chr "<password redacted...>"
#'   # TRACE 10:53:15 [scheduler]: current stage: ready
#' }
#'
#'
#' @export
Agent <- S7::new_class(
  name = "TricobblerAgent",
  parent = S7::class_function,
  properties = list(
    id = S7::new_property(
      class = S7::class_character,
      validator = function(value) {
        if (length(value) != 1 || is.na(value) || !nzchar(value)) {
          return("Agent `id` must be length of one, non-empty string")
        }
        if (grepl("[^a-zA-Z0-9_-]", value)) {
          return("Agent `id` must only contain letters, digits, underscores, and dashes") # nolint: line_length_linter.
        }
        return()
      }
    ),
    description = S7::new_property(class = S7::class_character),

    # pretty way to describe the results as short string/paragraph so AI
    # can read it without opening up the attachment
    # The results will be automatically logged to the context
    describe = S7::new_property(
      class = S7::class_function | NULL,
      default = quote(tricobbler::mcp_describe)
    )
  ),
  validator = function(self) {
    nms <- names(formals(self))
    if (length(nms) < 1 || nms[[1]] != "self") {
      return("The first argument of the agent function should be `self`")
    }
    if (length(nms) < 2 || nms[[2]] != "policy") {
      return("The second argument of the agent function should be `policy`")
    }
    if (length(nms) < 3 || nms[[3]] != "context") {
      return("The third argument of the agent function should be `context`")
    }
  }
)


# ---------------------------------------------------------------------------
# as_agent Generic: Convert Various Objects to Agent
# ---------------------------------------------------------------------------
# Enables flexible agent creation from:
#   - Agent objects (identity)
#   - ellmer Chat objects (LLM integration)
#   - Functions (simple wrapper)
#   - Character strings (MCP tool specs or package::function references)
# ---------------------------------------------------------------------------

#' Convert Objects to Agent
#'
#' @description Generic function to convert various object types into
#'   \code{\link{Agent}} objects for use with the \code{\link{Scheduler}}.
#'   Supports conversion from ellmer Chat objects, functions, MCP tool
#'   specifications, and package function references.
#'
#' @param x Object to convert. Can be:
#'   \itemize{
#'     \item An \code{Agent} object (returned as-is)
#'     \item An ellmer \code{Chat} object (creates an LLM-backed agent)
#'     \item A function with signature \code{function(self, policy, context)}
#'     \item A character string referencing a package function
#'       (\code{"pkg::fun"})
#'     \item An MCP tool definition (class \code{tricobbler_mcp_tool})
#'   }
#' @param id Character. Unique identifier for the agent. Required for
#'   non-Agent inputs. For Chat objects, defaults to
#'   \code{"chat_<provider>_<model>"}.
#' @param description Character. Human-readable description. For Chat objects,
#'   auto-generated from model info if not provided.
#' @param describe Function or NULL. Result formatting function for logging.
#'   Defaults to \code{\link{mcp_describe}}.
#' @param ... Additional arguments passed to \code{\link{Agent}}.
#'
#' @return An \code{Agent} object.
#'
#' @seealso \code{\link{StatePolicy}} for details on how to set additional
#'   arguments via \code{parameters} for deterministic agents (functions,
#'   MCP tools) and AI agents (ellmer Chat objects).
#'
#' @examples
#'
#' # From a simple function
#' my_func <- function(self, policy, context) {
#'   context$logger("Hello from my agent!")
#'   return("done")
#' }
#' agent <- as_agent(my_func)
#'
#'
#' \dontrun{
#' # From an ellmer Chat (requires API credentials)
#' chat <- ellmer::chat_openai(model = "gpt-4o-mini")
#' agent <- as_agent(chat)
#'
#' # From a package function reference
#' agent <- as_agent("utils::read.csv", id = "mean_agent")
#' }
#'
#' @export
as_agent <- S7::new_generic("as_agent", dispatch_args = "x")

S7::method(as_agent, Agent) <- function(x, ...) {
  x
}

S7::method(as_agent, S7::new_S3_class("Chat")) <- function(
    x,
    id = NULL,
    description = NULL,
    describe = mcp_describe,
    ...) {

  as_agent_from_chat(chat = x, id = id, description = description, describe = describe, ...)
}

S7::method(as_agent, S7::new_S3_class("tricobbler_mcp_tool")) <- function(x,
                                                                          id = NULL,
                                                                          description = NULL,
                                                                          describe = mcp_describe,
                                                                          ...) {

  as_agent_from_mcp_tool(tool = x, id = id, description = description, describe = describe, ...)
}

S7::method(as_agent, S7::class_function) <- function(
    x,
    id = substitute(x),
    description = "",
    describe = mcp_describe,
    ...) {

  id <- as.character(id)

  wrapper_fun <- function(self, policy, context) {
    # Extract parameters from policy if available
    params <- as.list(policy@parameters)
    debug <- isTRUE(context$debug)

    switch(
      policy@accessibility,
      "all" = {
        # Obtain the last result
        last_attachment <- context$last_results(items = 1, simplify = TRUE)
        input <- c(list(last_attachment$result), as.list(params$args))
      },
      "logs" = {
        last_attachment <- context$last_results(items = 1, simplify = TRUE)
        input <- c(list(last_attachment$description), as.list(params$args))
      },
      {
        input <- as.list(params$args)
      }
    )

    # Debug mode: log call info for inspection
    if (debug) {
      call_obj <- as.call(c(list(x), input))
      context$logger("Function call: ", deparse(call_obj), level = "DEBUG")
    }

    do.call(x, input)
  }

  Agent(.data = wrapper_fun, id = id, description = description, describe = describe)
}

S7::method(as_agent, S7::class_character) <- function(
    x,
    id = NULL,
    description = "",
    describe = mcp_describe,
    ...) {

  # Check if it's a package::function reference
  if (grepl("::", x, fixed = TRUE)) {
    split_str <- strsplit(x, "[:]+", perl = TRUE)[[1]]
    pkg_name <- split_str[[1]]
    fun_name <- split_str[[2]]
    ns <- asNamespace(pkg_name)
    fun <- ns[[fun_name]]

    if (!is.function(fun)) {
      stop(x, " is not a valid function")
    }

    if (length(id) != 1) {
      id <- sprintf("%s--%s", pkg_name, fun_name)
    }

    agent <- as_agent(x = fun, id = id,
             description = description, describe = describe, ...)
    return(agent)
  }

  # Otherwise treat as MCP tool path/name
  tool <- mcptool_read(x)
  as_agent_from_mcp_tool(
    tool = tool,
    id = id,
    description = description,
    describe = describe,
    ...
  )
}


# ---------------------------------------------------------------------------
# Internal: as_agent_from_chat - Create Agent from ellmer Chat
# ---------------------------------------------------------------------------
#' @keywords internal
#' @noRd
as_agent_from_chat <- function(
    chat,
    id = NULL,
    description = NULL,
    describe = mcp_describe,
    ...) {

  # chat = ellmer::chat_anthropic()
  if (!inherits(chat, "Chat")) {
    stop("'chat' must be an ellmer `Chat` object")
  }

  model <- chat$get_model()
  if (is.null(id)) {
    # Clean model name for ID (replace special chars)
    model_clean <- gsub("[^a-zA-Z0-9]", "_", model)
    suffix <- digest::digest(chat$get_provider(), algo = "crc32")
    id <- sprintf("chat_%s_%s", model_clean, suffix)
  }

  # Generate description if not provided
  if (is.null(description)) {
    description <- sprintf("LLM agent powered by %s", model)
  }

  agent_fun <- function(self, policy, context, ...) {
    # Extract AI agent parameters from policy@parameters
    params <- as.list(policy@parameters)
    system_prompt <- paste(params$system_prompt %||% policy@description, collapse = "\n")
    user_prompt <- params$user_prompt %||% ""
    keep_turns <- params$keep_turns %||% FALSE
    return_type <- params$return_type  # NULL means unstructured chat
    # Convert return_type to ellmer type if it's a list or character (from YAML)
    if (!is.null(return_type) && !inherits(return_type, "ellmer::Type")) {
      return_type <- map_type_to_ellmer(return_type)
    }
    debug <- isTRUE(context$debug)

    # build tools
    resources <- policy@resources
    switch(
      policy@accessibility,
      "all" = {
        context_tools <- mcptool_list("tricobbler", "context")
        context_banned <- NULL
        sys_prompt2 <- paste0(
          "Context tools available (full access):\n",
          paste("  -", context_tools, collapse = "\n")
        )
      },
      "logs" = {
        context_banned <- mcptool_list("tricobbler", "context_attachment_get")
        context_tools <- mcptool_list("tricobbler", "context")
        context_tools <- context_tools[!context_tools %in% context_banned]
        sys_prompt2 <- paste0(
          "Context tools available (logs only, attachments restricted):\n",
          paste("  -", context_tools, collapse = "\n")
        )
      },
      {
        # None
        context_tools <- NULL
        context_banned <- mcptool_list("tricobbler", "context")
        sys_prompt2 <- NULL
      }
    )
    resources <- c(resources, context_tools)
    resources <- resources[!resources %in% context_banned]
    resources <- unique(resources)

    # Build system prompt
    chat$set_system_prompt(paste(c(system_prompt, sys_prompt2), collapse = "\n"))

    # Build MCP tools
    tools <- mcptool_instantiate(lapply(resources, mcptool_read))
    chat$set_tools(tools)

    # Read logs
    if (policy@accessibility != "none") {
      log_content <- mcp_attach(
        "```json",
        mcp_tool_context_logs_tail(max_lines = 10L, skip_lines = 0L),
        "```",
        .header = "## last 10 lines of the logging content"
      )
    } else {
      log_content <- NULL
    }

    user_prompt <- trimws(user_prompt)
    if (!nzchar(user_prompt)) {
      user_prompt <- "Please proceed with the task defined in your system prompt."
    }

    # Clear conversation history unless keep_turns is TRUE
    if (!isTRUE(keep_turns)) {
      chat$set_turns(list())
    }

    # Debug mode: log prompts and tools for inspection
    if (debug) {
      full_sys_prompt <- paste(c(system_prompt, sys_prompt2), collapse = "\n")
      context$logger("Model: ", model, level = "DEBUG")
      context$logger("System prompt:\n", full_sys_prompt, level = "DEBUG")
      context$logger("User prompt:\n", user_prompt, level = "DEBUG")
      context$logger("Tools: ", paste(resources, collapse = ", "), level = "DEBUG")
    }

    if (!is.null(return_type)) {
      chat_impl <- function(...) {
        chat$chat_structured(..., type = return_type)
      }
    } else {
      chat_impl <- chat$chat
    }

    # Return the response
    chat_impl(
      log_content,
      mcp_attach(sprintf("Stage: %s --> State: %s", policy@stage, policy@name),
                 .header = "## Current Execution"),
      mcp_attach(user_prompt, .header = "## Task")
    )
  }

  Agent(
    .data = agent_fun,
    id = id,
    description = description,
    describe = describe,
    ...
  )
}


# ---------------------------------------------------------------------------
# Internal: as_agent_from_mcp_tool - Create Agent from MCP Tool Definition
# ---------------------------------------------------------------------------
#' @keywords internal
#' @noRd
as_agent_from_mcp_tool <- function(
    tool,
    id = NULL,
    description = NULL,
    describe = mcp_describe,
    ...) {

  if (!inherits(tool, "tricobbler_mcp_tool")) {
    stop("'tool' must be a tricobbler_mcp_tool object")
  }

  # Use tool name as ID if not provided
  tool_id <- id %||% tool$name
  if (is.null(tool_id)) {
    stop("Tool definition missing 'name' and no 'id' provided")
  }
  # Clean the ID
  tool_id <- gsub("[^a-zA-Z0-9_-]", "_", tool_id)

  # Use tool description if not provided
  tool_desc <- description %||% sprintf("MCP tool `%s` as an Agent", tool$name)

  # Get the underlying function
  impl <- mcptool_seek_function(tool)

  agent_fun <- function(self, policy, context, ...) {
    # Extract parameters from policy if available
    params <- as.list(policy@parameters)
    debug <- isTRUE(context$debug)

    switch(
      policy@accessibility,
      "all" = {
        # Obtain the last result
        last_attachment <- context$last_results(items = 1, simplify = TRUE)
        input <- c(list(last_attachment$result), as.list(params$args))
      },
      "logs" = {
        last_attachment <- context$last_results(items = 1, simplify = TRUE)
        input <- c(list(last_attachment$description), as.list(params$args))
      },
      {
        input <- as.list(params$args)
      }
    )

    # Debug mode: log call info for inspection
    if (debug) {
      call_obj <- as.call(c(list(impl), input))
      context$logger("Tool call: ", deparse(call_obj), level = "DEBUG")
    }

    do.call(impl, input)
  }

  # tool results are processed by mcp_describe
  if (identical(describe, mcp_describe)) {
    describe <- function(x) {
      x
    }
  }
  Agent(
    .data = agent_fun,
    id = tool_id,
    description = tool_desc,
    describe = describe,
    ...
  )
}

