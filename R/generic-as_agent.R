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
#'   Supports conversion from \pkg{ellmer} Chat objects,
#'   functions, \verb{MCP} tool
#'   specifications, and package function references.
#'
#' @param x Object to convert. Can be:
#'   \itemize{
#'     \item An \code{Agent} object (returned as-is)
#'     \item An \pkg{ellmer} \code{Chat} object
#'       (creates an \verb{LLM}-backed agent)
#'     \item A function with signature \code{function(runtime)}, where
#'           `runtime` is an `AgentRuntime` object
#'     \item A character string referencing a package function
#'       (\code{"pkg::fun"})
#'     \item An \verb{MCP} tool definition (class \code{tricobbler_mcp_tool})
#'   }
#' @param ... Additional arguments passed to methods. Common arguments
#'   include \code{id} (character, unique agent identifier),
#'   \code{description} (character, human-readable description), and
#'   \code{describe} (function, result formatting for logging;
#'   defaults to \code{\link{mcp_describe}}).
#'
#' @return An \code{Agent} object.
#'
#' @seealso \code{\link{StatePolicy}} for details on how to set additional
#'   arguments via \code{parameters} for deterministic agents (functions,
#'   \verb{MCP} tools) and AI agents (\pkg{ellmer} Chat objects).
#'
#' @section Chat Agent Parameters:
#' When converting an \pkg{ellmer} Chat object, the following
#' \code{parameters} (set in \code{\link{StatePolicy}} or
#' \code{\link{MasterPolicy}}) are recognised:
#' \describe{
#'   \item{\code{max_tokens}}{integer, maximum tokens for the
#'     \verb{LLM} response.
#'     Applied to the provider before each call.  Useful for
#'     preventing output truncation with long tool-calling
#'     workflows (e.g. set to \code{16384}).}
#'   \item{\code{max_chat_errors}}{integer, number of consecutive
#'     \verb{LLM} call errors (e.g. truncated \verb{JSON},
#'     \verb{API} timeouts) tolerated before the agent gives up.
#'     Defaults to \code{Inf}.  Within this budget the error message
#'     is fed back to the \verb{LLM} so it can self-correct.}
#'   \item{\code{system_prompt}}{character, overrides the policy
#'     description.}
#'   \item{\code{user_prompt}}{character, task prompt sent to the
#'     \verb{LLM}.}
#'   \item{\code{keep_turns}}{logical, preserve conversation
#'     history across retries (default \code{FALSE}).}
#'   \item{\code{return_type}}{an \pkg{ellmer} type specification
#'     for structured output.}
#' }
#'
#' @examples
#'
#' # From a simple function
#' my_func <- function(runtime) {
#'   runtime$logger("Hello from my agent!")
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

  as_agent_from_chat(
    chat = x, id = id, description = description,
    describe = describe, ...
  )
}

S7::method(as_agent, S7::new_S3_class("tricobbler_mcp_tool")) <- function(
  x,
  id = NULL,
  description = NULL,
  describe = mcp_describe,
  ...
) {
  as_agent_from_mcp_tool(
    tool = x,
    id = id,
    description = description,
    describe = describe,
    ...
  )
}

S7::method(as_agent, S7::class_function) <- function(
    x,
    id = substitute(x),
    description = "",
    describe = mcp_describe,
    ...) {

  id <- as.character(id)
  force(x)
  fun_params <- names(formals(x))

  wrapper_fun <- function(runtime) {
    # Extract parameters from policy if available
    policy <- runtime$policy
    context <- runtime$context

    params <- as.list(policy@parameters)
    debug <- isTRUE(context$debug)

    # Determine input arguments based on accessibility policy
    # "all" and "explicit": Use explicit dependencies defined in policy
    # "logs" and "none": Use only static arguments (no dependencies)

    # 1. Start with static args, cascading global params as defaults
    #    Precedence: dependency > local (state) > global (master)
    input <- list()
    master <- runtime$master_policy
    if (!is.null(master)) {
      input <- as.list(master@parameters)
    }
    local_args <- as.list(params$args)
    if (length(local_args)) {
      input <- modifyList(input, local_args)
    }
    if (policy@accessibility %in% c("all", "explicit")) {

      # 2. Iterate over dependencies
      deps <- policy@depends_on@deps

      for (param_name in names(deps)) {
        dep_spec <- deps[[param_name]]

        # 3. Resolve defaults
        target_stage <- if (is.null(dep_spec$stage)) {
          policy@stage
        } else {
          dep_spec$stage
        }
        target_field <- if (is.null(dep_spec$field)) {
          "result"
        } else {
          dep_spec$field
        }

        # 4. Fetch attachment
        attachment <- context$get_attachment_by_state(
          state = dep_spec$state,
          stage = target_stage
        )

        # 5. Fail-fast if missing
        if (is.null(attachment)) {
          stop(sprintf(
            paste0(
              "Dependency missing: State '%s' ",
              "(Stage '%s') required for ",
              "parameter '%s'."
            ),
            dep_spec$state, target_stage, param_name
          ))
        }

        # 6. Extract and assign
        input[[param_name]] <- attachment[[target_field]]
      }
    }

    # filter the inputs
    if (!"..." %in% fun_params) {
      input <- input[names(input) %in% fun_params]
    }

    # Inject .runtime if the function accepts it
    if (".runtime" %in% fun_params) {
      input[[".runtime"]] <- quote(runtime)
    }

    # Debug mode: log call info for inspection
    if (debug) {
      call_obj <- as.call(c(list(x), input))
      runtime$logger("Function call: ", deparse(call_obj), level = "DEBUG")
    }

    do.call(x, input)
  }

  Agent(
    .data = wrapper_fun, id = id,
    description = description, describe = describe
  )
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

  agent_fun <- function(runtime, ...) {

    chat <- chat$clone(deep = TRUE)

    policy <- runtime$policy
    context <- runtime$context
    is_async <- identical(runtime$status, "running (async)")

    # Extract AI agent parameters from policy@parameters
    params <- as.list(policy@parameters)

    # Cascade global (master) parameters as defaults
    master <- runtime$master_policy
    if (!is.null(master)) {
      master_params <- as.list(master@parameters)
      params <- modifyList(master_params, params)
    }

    system_prompt <- paste(
      params$system_prompt %||% policy@description,
      collapse = "\n"
    )
    user_prompt <- params$user_prompt %||% ""
    keep_turns <- params$keep_turns %||% FALSE
    return_type <- params$return_type  # NULL means unstructured chat
    max_tokens <- params$max_tokens  # NULL = provider default
    # Convert return_type to ellmer type if it's a list or character (from YAML)
    if (!is.null(return_type) && !inherits(return_type, "ellmer::Type")) {
      return_type <- map_type_to_ellmer(return_type)
    }

    # Apply max_tokens to the Chat provider if specified
    if (!is.null(max_tokens)) {
      max_tokens <- as.integer(max_tokens)
      tryCatch(
        {
          # Provider is S7 (value semantics), so we must modify
          # inside the R6 private environment directly.
          priv <- chat$.__enclos_env__$private
          priv$provider@params$max_tokens <- max_tokens
        },
        error = function(e) {
          # Best-effort: warn rather than fail
          warning(
            "Could not set max_tokens on provider: ",
            conditionMessage(e),
            call. = FALSE
          )
        }
      )
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
          "Context tools available ",
          "(logs only, attachments restricted):\n",
          paste("  -", context_tools, collapse = "\n")
        )
      },
      "explicit" = {
        # Explicit access: same tools as logs (read logs, list attachments),
        # but cannot read attachment content arbitrarily via tools.
        context_banned <- mcptool_list("tricobbler", "context_attachment_get")
        context_tools <- mcptool_list("tricobbler", "context")
        context_tools <- context_tools[!context_tools %in% context_banned]
        sys_prompt2 <- paste0(
          "Context tools available ",
          "(restricted access):\n",
          "You can read execution logs and list ",
          "attachments, but you cannot read ",
          "arbitrary attachment content.\n",
          "Specific attachments declared as ",
          "dependencies will be provided in ",
          "the user prompt.\n",
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
    chat$set_system_prompt(
      paste(c(system_prompt, sys_prompt2),
            collapse = "\n")
    )

    # Build MCP tools
    tools <- mcptool_instantiate(lapply(resources, mcptool_read))
    chat$set_tools(tools)

    # Also append dynamically generated tools
    dyn_tools <- context$get_tools()
    if (length(dyn_tools)) {
      chat$register_tools(dyn_tools)
    }

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
      user_prompt <- paste(
        "Please proceed with the task",
        "defined in your system prompt."
      )
    }

    # Clear conversation history unless keep_turns is TRUE
    if (!isTRUE(keep_turns)) {
      chat$set_turns(list())
    }

    # Debug mode: log prompts and tools for inspection
    if (debug) {
      full_sys_prompt <- paste(
        c(system_prompt, sys_prompt2),
        collapse = "\n"
      )
      runtime$logger(
        "Model: ", model, level = "DEBUG"
      )
      runtime$logger(
        "System prompt:\n", full_sys_prompt,
        level = "DEBUG"
      )
      runtime$logger(
        "User prompt:\n", user_prompt,
        level = "DEBUG"
      )
      runtime$logger(
        "Tools: ",
        paste(resources, collapse = ", "),
        level = "DEBUG"
      )
    }

    if (!is.null(return_type)) {
      chat_impl <- function(..., .list = list()) {
        args <- c(as.list(.list), list(..., type = return_type))
        if (is_async) {
          do.call(chat$chat_structured_async, args)
        } else {
          do.call(chat$chat_structured, args)
        }
      }
    } else {
      chat_impl <- function(..., .list = list()) {
        args <- c(as.list(.list), list(...))

        if (is_async) {
          do.call(chat$chat_async, args)
        } else {
          do.call(chat$chat, args)
        }
      }
    }

    # Prepare dependencies for prompt injection
    dependency_attachments <- list()
    if (policy@accessibility %in% c("explicit", "all")) {
      deps <- policy@depends_on@deps

      for (param_name in names(deps)) {
        dep_spec <- deps[[param_name]]

        target_stage <- if (is.null(dep_spec$stage)) {
          policy@stage
        } else {
          dep_spec$stage
        }

        target_field <- if (is.null(dep_spec$field)) {
          "result"
        } else {
          dep_spec$field
        }

        attachment <- context$get_attachment_by_state(
          state = dep_spec$state,
          stage = target_stage
        )

        if (is.null(attachment)) {
          stop(sprintf(
            paste0(
              "Dependency missing: State '%s' ",
              "(Stage '%s') required for ",
              "parameter '%s'."
            ),
            dep_spec$state, target_stage, param_name
          ))
        }

        val <- attachment[[target_field]]
        if (identical(target_field, "result")) {
          val <- mcp_describe(val)
        }
        dep_idx <- length(dependency_attachments) + 1
        dependency_attachments[[dep_idx]] <- mcp_attach(
          sprintf(
            "Dependency Input: %s (from %s, field %s)",
            param_name,
            dep_spec$state,
            target_field
          ),
          "",
          "```",
          val,
          "```",
          .header = sprintf("## Attachment ID: %s", attachment$id)
        )
      }
    }

    # Build content list, filtering out NULLs (e.g. log_content
    # when accessibility = "none") to avoid empty Content elements
    contents <- list(
      log_content,
      mcp_attach(sprintf("Stage: %s --> State: %s", policy@stage, policy@name),
                 .header = "## Current Execution"),
      mcp_attach(user_prompt, .header = "## Task")
    )
    contents <- Filter(Negate(is.null), contents)

    # Call the LLM, catching errors gracefully so the scheduler
    # records them as failed attachments and can retry.
    max_chat_errors <- as.numeric(
      params$max_chat_errors %||% Inf
    )
    chat_error_count <- 0L
    last_error <- NULL

    while (chat_error_count < max_chat_errors) {
      result <- tryCatch(
        {
          chat_impl(.list = c(contents, dependency_attachments))
        },
        error = function(e) {
          e
        }
      )

      if (!inherits(result, "error")) {
        if (is.null(result)) {
          result <- list()
        }
        attr(result, "turns") <- chat$get_turns()
        return(result)
      }

      # An error occurred (e.g. truncated JSON, API timeout)
      chat_error_count <- chat_error_count + 1L
      last_error <- result
      err_msg <- conditionMessage(result)

      runtime$logger(
        sprintf(
          "chat error (%.0f/%.0f): %s",
          chat_error_count, max_chat_errors, err_msg
        ),
        level = "WARN"
      )
      traceback(result)

      if (chat_error_count >= max_chat_errors) {
        break
      }

      # Feed the error back to the LLM so it can self-correct
      # on the next iteration (e.g. generate shorter output)
      error_prompt <- mcp_attach(
        paste0(
          "Your previous response caused an error:\n\n",
          err_msg, "\n\n",
          "Please try again. If the error is about ",
          "output length, produce shorter output or ",
          "split your work across multiple tool calls."
        ),
        .header = "## Error Recovery"
      )
      contents <- list(error_prompt)
      dependency_attachments <- list()
    }

    # All retries exhausted - propagate the last error
    stop(last_error)
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

  agent_fun <- function(runtime, ...) {

    policy <- runtime$policy
    context <- runtime$context

    # Extract parameters from policy if available
    params <- as.list(policy@parameters)
    debug <- isTRUE(context$debug)

    # Determine input arguments based on accessibility policy
    # "all" and "explicit": Use explicit dependencies defined in policy
    # "logs" and "none": Use only static arguments (no dependencies)
    # 1. Start with static args, cascading global params as defaults
    #    Precedence: dependency > local (state) > global (master)
    input <- list()
    master <- runtime$master_policy
    if (!is.null(master)) {
      input <- as.list(master@parameters)
    }
    local_args <- as.list(params$args)
    if (length(local_args)) {
      input <- modifyList(input, local_args)
    }
    if (policy@accessibility %in% c("all", "explicit")) {

      # 2. Iterate over dependencies
      deps <- policy@depends_on@deps

      for (param_name in names(deps)) {
        dep_spec <- deps[[param_name]]

        # 3. Resolve defaults
        target_stage <- if (is.null(dep_spec$stage)) {
          policy@stage
        } else {
          dep_spec$stage
        }
        target_field <- if (is.null(dep_spec$field)) {
          "result"
        } else {
          dep_spec$field
        }

        # 4. Fetch attachment
        attachment <- context$get_attachment_by_state(
          state = dep_spec$state,
          stage = target_stage
        )

        # 5. Fail-fast if missing
        if (is.null(attachment)) {
          stop(sprintf(
            paste0(
              "Dependency missing: State '%s' ",
              "(Stage '%s') required for ",
              "parameter '%s'."
            ),
            dep_spec$state, target_stage, param_name
          ))
        }

        # 6. Extract and assign
        input[[param_name]] <- attachment[[target_field]]
      }
    }

    # Debug mode: log call info for inspection
    if (debug) {
      call_obj <- as.call(c(list(impl), input))
      runtime$logger("Tool call: ", deparse(call_obj), level = "DEBUG")
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
