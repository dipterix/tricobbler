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
