
#' Check Context Accessibility from Active Policy
#'
#' @description
#' Internal helper to check if the current policy allows the requested
#' access level. Returns the accessibility level or an error message.
#'
#' @param required_level character, the minimum accessibility level required.
#'   One of \code{"logs"} (logs only) or \code{"all"} (full access).
#' @return A list with \code{allowed} (logical) and \code{level} (character)
#'   if access is granted, or \code{allowed = FALSE} and \code{error}
#'   (character) if denied.
#' @noRd
check_context_accessibility <- function(
    required_level = c("logs", "all")
) {
  required_level <- match.arg(required_level)

  # Get active policy from globals, default to "logs" if missing
  policy <- get_globals("active_policy", missing = NULL, simplify = TRUE)

  if (is.null(policy)) {
    # No active policy - use default accessibility "logs"
    current_level <- "logs"
  } else if (S7::S7_inherits(policy, StatePolicy)) {
    current_level <- policy@accessibility
    # Validate accessibility value
    if (!current_level %in% c("all", "logs", "none")) {
      current_level <- "logs"
    }
  } else {
    # Invalid policy object - use default
    current_level <- "logs"
  }

  # Check access based on current level
  if (current_level == "none") {
    return(list(
      allowed = FALSE,
      level = current_level,
      error = "Context access denied. Policy accessibility is set to 'none'."
    ))
  }

  if (required_level == "all" && current_level != "all") {
    return(list(
      allowed = FALSE,
      level = current_level,
      error = paste0(
        "Attachment access denied. Policy accessibility is '",
        current_level,
        "' but 'all' is required for attachment operations."
      )
    ))
  }

  # Access granted
  list(allowed = TRUE, level = current_level)
}


# ---- MCP Tools for Context Log Access ------------------------------------

#' Read Log Entries from Head (Beginning)
#'
#' @description
#' Read and parse log file contents from the beginning of the log file.
#' Returns structured log entries with level, time, caller, and content.
#' This tool is useful for reviewing the start of a workflow execution
#' or debugging early-stage issues.
#'
#' @param max_lines Integer, maximum number of lines to read (default 300).
#' @param skip_lines Integer, number of lines to skip from the beginning
#'   (default 0).
#' @param pattern Character, optional regex pattern to filter log content.
#'   Only entries matching the pattern in their content will be returned.
#' @param levels Character vector, log levels to include. Defaults to all
#'   levels: \code{TRACE}, \code{DEBUG}, \code{INFO}, \code{WARN},
#'   \code{ERROR}, \code{FATAL}.
#'
#' @return A list containing:
#' \describe{
#'   \item{success}{Logical, whether the operation completed successfully}
#'   \item{context_id}{Character, the context identifier}
#'   \item{count}{Integer, number of log entries returned}
#'   \item{entries}{List of log entries, each with \code{line_no},
#'     \code{level}, \code{time}, \code{caller}, \code{content}}
#'   \item{error}{Character, error message if success is \code{FALSE}}
#' }
#'
#' @keywords mcp-tool mcp-category-info mcp-output-json
#' @noRd
mcp_tool_context_logs_head <- function(
    max_lines = 300L,
    skip_lines = 0L,

    pattern = NULL,
    levels = c("TRACE", "DEBUG", "INFO", "WARN", "ERROR", "FATAL")
) {

  # Check accessibility - logs require "logs" or "all"
  access <- check_context_accessibility("logs")
  if (!access$allowed) {
    return(jsonlite::toJSON(
      list(success = FALSE, error = access$error),
      auto_unbox = TRUE, pretty = FALSE
    ))
  }

  context <- get_active_context()

  if (is.null(context)) {
    return(jsonlite::toJSON(
      list(
        success = FALSE,
        error = "No active context. Start a workflow execution first."
      ),
      auto_unbox = TRUE, pretty = FALSE
    ))
  }

  if (!R6::is.R6(context) || !inherits(context, "TricobblerAgentContext")) {
    return(jsonlite::toJSON(
      list(success = FALSE, error = "Invalid context object."),
      auto_unbox = TRUE, pretty = FALSE
    ))
  }

  result <- tryCatch(
    {
      logs <- context$read_logs(
        method = "head",
        max_lines = max_lines,
        skip_lines = skip_lines,
        pattern = pattern,
        levels = levels
      )

      jsonlite::toJSON(
        list(
          success = TRUE,
          context_id = context$id,
          count = nrow(logs),
          entries = logs
        ),
        auto_unbox = TRUE, dataframe = "rows", pretty = FALSE
      )
    },
    error = function(e) {
      jsonlite::toJSON(
        list(
          success = FALSE,
          context_id = context$id,
          error = conditionMessage(e)
        ),
        auto_unbox = TRUE, pretty = FALSE
      )
    }
  )

  result
}


#' Read Log Entries from Tail (End)
#'
#' @description
#' Read and parse log file contents from the end of the log file.
#' Returns structured log entries with level, time, caller, and content.
#' This tool is useful for reviewing recent workflow activity or
#' debugging the latest issues.
#'
#' @param max_lines Integer, maximum number of lines to read (default 300).
#' @param skip_lines Integer, number of lines to skip from the end
#'   (default 0). Use this for pagination when reading older entries.
#' @param pattern Character, optional regex pattern to filter log content.
#'   Only entries matching the pattern in their content will be returned.
#' @param levels Character vector, log levels to include. Defaults to all
#'   levels: \code{TRACE}, \code{DEBUG}, \code{INFO}, \code{WARN},
#'   \code{ERROR}, \code{FATAL}.
#'
#' @return A list containing:
#' \describe{
#'   \item{success}{Logical, whether the operation completed successfully}
#'   \item{context_id}{Character, the context identifier}
#'   \item{count}{Integer, number of log entries returned}
#'   \item{entries}{List of log entries, each with \code{line_no},
#'     \code{level}, \code{time}, \code{caller}, \code{content}}
#'   \item{error}{Character, error message if success is \code{FALSE}}
#' }
#'
#' @keywords mcp-tool mcp-category-info mcp-output-json
#' @noRd
mcp_tool_context_logs_tail <- function(
    max_lines = 300L,
    skip_lines = 0L,
    pattern = NULL,
    levels = c("TRACE", "DEBUG", "INFO", "WARN", "ERROR", "FATAL")
) {
  # Check accessibility - logs require "logs" or "all"
  access <- check_context_accessibility("logs")
  if (!access$allowed) {
    return(jsonlite::toJSON(
      list(success = FALSE, error = access$error),
      auto_unbox = TRUE, pretty = FALSE
    ))
  }

  context <- get_active_context()

  if (is.null(context)) {
    return(jsonlite::toJSON(
      list(
        success = FALSE,
        error = "No active context. Start a workflow execution first."
      ),
      auto_unbox = TRUE, pretty = FALSE
    ))
  }

  if (!R6::is.R6(context) || !inherits(context, "TricobblerAgentContext")) {
    return(jsonlite::toJSON(
      list(success = FALSE, error = "Invalid context object."),
      auto_unbox = TRUE, pretty = FALSE
    ))
  }

  result <- tryCatch(
    {
      logs <- context$read_logs(
        method = "tail",
        max_lines = max_lines,
        skip_lines = skip_lines,
        pattern = pattern,
        levels = levels
      )

      jsonlite::toJSON(
        list(
          success = TRUE,
          context_id = context$id,
          count = nrow(logs),
          entries = logs
        ),
        auto_unbox = TRUE, dataframe = "rows", pretty = FALSE
      )
    },
    error = function(e) {
      jsonlite::toJSON(
        list(
          success = FALSE,
          context_id = context$id,
          error = conditionMessage(e)
        ),
        auto_unbox = TRUE, pretty = FALSE
      )
    }
  )

  result
}


#' Search Log Entries by Pattern
#'
#' @description
#' Search log file contents using a regex pattern. Reads the entire log
#' (up to \code{max_lines}) and returns entries matching the pattern.
#' This tool is useful for finding specific events, errors, or agent
#' activities across the entire log history.
#'
#' @param pattern Character, regex pattern to search for in log content.
#'   This parameter is required.
#' @param max_lines Integer, maximum number of lines to search through
#'   (default 1000). Reads from the end of the log.
#' @param levels Character vector, log levels to include. Defaults to all
#'   levels: \code{TRACE}, \code{DEBUG}, \code{INFO}, \code{WARN},
#'   \code{ERROR}, \code{FATAL}.
#'
#' @return A list containing:
#' \describe{
#'   \item{success}{Logical, whether the operation completed successfully}
#'   \item{context_id}{Character, the context identifier}
#'   \item{pattern}{Character, the search pattern used}
#'   \item{count}{Integer, number of matching log entries}
#'   \item{entries}{List of matching log entries, each with \code{line_no},
#'     \code{level}, \code{time}, \code{caller}, \code{content}}
#'   \item{error}{Character, error message if success is \code{FALSE}}
#' }
#'
#' @keywords mcp-tool mcp-category-info mcp-output-json
#' @noRd
mcp_tool_context_logs_search <- function(
    pattern,
    max_lines = 1000L,
    levels = c("TRACE", "DEBUG", "INFO", "WARN", "ERROR", "FATAL")
) {
  # Check accessibility - logs require "logs" or "all"
  access <- check_context_accessibility("logs")
  if (!access$allowed) {
    return(jsonlite::toJSON(
      list(success = FALSE, error = access$error),
      auto_unbox = TRUE, pretty = FALSE
    ))
  }

  # Validate pattern
  if (missing(pattern) || !is.character(pattern) ||
      length(pattern) != 1 || !nzchar(pattern)) {
    return(jsonlite::toJSON(
      list(
        success = FALSE,
        error = "pattern parameter is required and must be a non-empty string" # nolint: line_length_linter.
      ),
      auto_unbox = TRUE, pretty = FALSE
    ))
  }

  context <- get_active_context()

  if (is.null(context)) {
    return(jsonlite::toJSON(
      list(
        success = FALSE,
        error = "No active context. Start a workflow execution first."
      ),
      auto_unbox = TRUE, pretty = FALSE
    ))
  }

  if (!R6::is.R6(context) || !inherits(context, "TricobblerAgentContext")) {
    return(jsonlite::toJSON(
      list(success = FALSE, error = "Invalid context object."),
      auto_unbox = TRUE, pretty = FALSE
    ))
  }

  result <- tryCatch(
    {
      logs <- context$read_logs(
        method = "tail",
        max_lines = max_lines,
        skip_lines = 0L,
        pattern = pattern,
        levels = levels
      )

      jsonlite::toJSON(
        list(
          success = TRUE,
          context_id = context$id,
          pattern = pattern,
          count = nrow(logs),
          entries = logs
        ),
        auto_unbox = TRUE, dataframe = "rows", pretty = FALSE
      )
    },
    error = function(e) {
      jsonlite::toJSON(
        list(
          success = FALSE, context_id = context$id, pattern = pattern,
          error = conditionMessage(e)
        ),
        auto_unbox = TRUE, pretty = FALSE
      )
    }
  )

  result
}


# ---- MCP Tools for Context Attachment Access -----------------------------

#' List All Attachments in Context
#'
#' @description
#' List all attachments (recorded results) in the current workflow context.
#' Returns metadata for each attachment including stage, state, agent ID,
#' attempt number, and the attachment identifier needed for retrieval.
#'
#' This tool works at any accessibility level. When accessibility is not
#' \code{"all"}, a note is included indicating the agent cannot read
#' attachment content.
#'
#' @param reindex Logical, whether to reload the index from disk
#'   (default \code{FALSE}). Set to \code{TRUE} if attachments may have been
#'   modified externally.
#'
#' @return A list containing:
#' \describe{
#'   \item{success}{Logical, whether the operation completed successfully}
#'   \item{context_id}{Character, the context identifier}
#'   \item{count}{Integer, number of attachments found}
#'   \item{attachments}{List of attachment metadata, each with \code{stage},
#'     \code{state}, \code{agent_id}, \code{current_attempt}, \code{filename}}
#'   \item{note}{Character, present when accessibility is not \code{"all"},
#'     indicating the agent cannot read attachment content}
#'   \item{error}{Character, error message if success is \code{FALSE}}
#' }
#'
#' @keywords mcp-tool mcp-category-info mcp-output-json
#' @noRd
mcp_tool_context_attachment_list <- function(reindex = FALSE) {
  # Check accessibility - list works but note if content is restricted
  access <- check_context_accessibility("logs")
  access_note <- NULL
  if (access$allowed && access$level != "all") {
    access_note <- paste(
      "Note: Agent does not have privilege to read attachment content.",
      "Only metadata is accessible."
    )
  }

  context <- get_active_context()

  if (is.null(context)) {
    return(jsonlite::toJSON(
      list(
        success = FALSE,
        error = "No active context. Start a workflow execution first."
      ),
      auto_unbox = TRUE, pretty = FALSE
    ))
  }

  if (!R6::is.R6(context) || !inherits(context, "TricobblerAgentContext")) {
    return(jsonlite::toJSON(
      list(success = FALSE, error = "Invalid context object."),
      auto_unbox = TRUE, pretty = FALSE
    ))
  }

  result <- tryCatch(
    {
      attachments <- context$list_attachments(reindex = isTRUE(reindex))

      if (is.data.frame(attachments) && nrow(attachments) > 0) {
        count <- nrow(attachments)
      } else {
        attachments <- data.frame(
          stage = character(0),
          state = character(0),
          agent_id = character(0),
          current_attempt = integer(0),
          filename = character(0),
          stringsAsFactors = FALSE
        )
        count <- 0L
      }

      response <- list(
        success = TRUE,
        context_id = context$id,
        count = count,
        attachments = attachments
      )
      if (!is.null(access_note)) {
        response$note <- access_note
      }

      jsonlite::toJSON(
        response,
        auto_unbox = TRUE, dataframe = "rows", pretty = FALSE
      )
    },
    error = function(e) {
      jsonlite::toJSON(
        list(
          success = FALSE,
          context_id = context$id,
          error = conditionMessage(e)
        ),
        auto_unbox = TRUE, pretty = FALSE
      )
    }
  )

  result
}


#' Check if Attachment Exists
#'
#' @description
#' Check whether a specific attachment exists in the current workflow context.
#' This is a lightweight check that validates the identifier format and
#' verifies the file exists on disk without loading the attachment data.
#'
#' This tool works at any accessibility level. When accessibility is not
#' \code{"all"}, a note is included indicating the agent cannot read
#' attachment content.
#'
#' @param attachment_id Character, the attachment identifier. This is the
#'   \code{filename} value from \code{mcp_tool_context_attachment_list} or
#'   the \code{identifier} logged when a result is recorded. Format:
#'   \code{[stage][state][agent_id]_YYMMDDTHHMMSS_attempt}.
#'
#' @return A list containing:
#' \describe{
#'   \item{success}{Logical, whether the operation completed successfully}
#'   \item{context_id}{Character, the context identifier}
#'   \item{attachment_id}{Character, the queried attachment identifier}
#'   \item{exists}{Logical, whether the attachment exists}
#'   \item{note}{Character, present when accessibility is not \code{"all"},
#'     indicating the agent cannot read attachment content}
#'   \item{error}{Character, error message if success is \code{FALSE}}
#' }
#'
#' @keywords mcp-tool mcp-category-info mcp-output-json
#' @noRd
mcp_tool_context_attachment_exists <- function(attachment_id) {
  # Check accessibility - exists check works but note if content is restricted
  access <- check_context_accessibility("logs")
  access_note <- NULL
  if (access$allowed && access$level != "all") {
    access_note <- paste(
      "Note: Agent does not have privilege",
      "to read attachment content."
    )
  }

  # Validate attachment_id
  if (missing(attachment_id) || !is.character(attachment_id) ||
      length(attachment_id) != 1 || is.na(attachment_id) ||
      !nzchar(attachment_id)) {
    return(jsonlite::toJSON(
      list(
        success = FALSE,
        error = "attachment_id is required and must be a non-empty string"
      ),
      auto_unbox = TRUE, pretty = FALSE
    ))
  }

  context <- get_active_context()

  if (is.null(context)) {
    return(jsonlite::toJSON(
      list(
        success = FALSE,
        error = "No active context. Start a workflow execution first."
      ),
      auto_unbox = TRUE, pretty = FALSE
    ))
  }

  if (!R6::is.R6(context) || !inherits(context, "TricobblerAgentContext")) {
    return(jsonlite::toJSON(
      list(success = FALSE, error = "Invalid context object."),
      auto_unbox = TRUE, pretty = FALSE
    ))
  }

  result <- tryCatch(
    {
      exists <- context$has_attachment(attachment_id)

      response <- list(
        success = TRUE,
        context_id = context$id,
        attachment_id = attachment_id,
        exists = exists
      )
      if (!is.null(access_note)) {
        response$note <- access_note
      }

      jsonlite::toJSON(
        response,
        auto_unbox = TRUE, pretty = FALSE
      )
    },
    error = function(e) {
      jsonlite::toJSON(
        list(
          success = FALSE,
          context_id = context$id,
          attachment_id = attachment_id,
          error = conditionMessage(e)
        ),
        auto_unbox = TRUE, pretty = FALSE
      )
    }
  )

  result
}


#' Get Attachment by Identifier
#'
#' @description
#' Retrieve a specific attachment (recorded result) from the current workflow
#' context. Returns the full attachment data including the result object,
#' description, and metadata.
#'
#' Requires policy accessibility level \code{"all"}.
#' Returns an error if accessibility is \code{"logs"} or \code{"none"}.
#'
#' @param attachment_id Character, the attachment identifier. This is the
#'   \code{filename} value from \code{mcp_tool_context_attachment_list} or
#'   the \code{identifier} logged when a result is recorded. Format:
#'   \verb{[stage][state][agent_id]_YYMMDDTHHMMSS_attempt}.
#'
#' @return A list containing:
#' \describe{
#'   \item{success}{Logical, whether the operation completed successfully}
#'   \item{context_id}{Character, the context identifier}
#'   \item{attachment_id}{Character, the queried attachment identifier}
#'   \item{attachment}{List, the attachment data including \code{result},
#'     \code{description}, \code{stage}, \code{state}, \code{agent_id},
#'     \code{current_attempt}, and \code{._timestamp}}
#'   \item{error}{Character, error message if success is \code{FALSE}}
#' }
#'
#' @keywords mcp-tool mcp-category-info mcp-output-json
#' @noRd
mcp_tool_context_attachment_get <- function(attachment_id) {
  # Check accessibility - attachments require "all"
  access <- check_context_accessibility("all")
  if (!access$allowed) {
    return(jsonlite::toJSON(
      list(success = FALSE, error = access$error),
      auto_unbox = TRUE, pretty = FALSE
    ))
  }

  # Validate attachment_id
  if (missing(attachment_id) || !is.character(attachment_id) ||
      length(attachment_id) != 1 || is.na(attachment_id) ||
      !nzchar(attachment_id)) {
    return(jsonlite::toJSON(
      list(
        success = FALSE,
        error = "attachment_id is required and must be a non-empty string"
      ),
      auto_unbox = TRUE, pretty = FALSE
    ))
  }

  context <- get_active_context()

  if (is.null(context)) {
    return(jsonlite::toJSON(
      list(
        success = FALSE,
        error = "No active context. Start a workflow execution first."
      ),
      auto_unbox = TRUE, pretty = FALSE
    ))
  }

  if (!R6::is.R6(context) || !inherits(context, "TricobblerAgentContext")) {
    return(jsonlite::toJSON(
      list(success = FALSE, error = "Invalid context object."),
      auto_unbox = TRUE, pretty = FALSE
    ))
  }

  result <- tryCatch(
    {
      attachment <- context$get_attachment(attachment_id)

      # Convert timestamp to ISO format for JSON
      if (!is.null(attachment$`._timestamp`)) {
        attachment$`._timestamp` <- format(
          attachment$`._timestamp`,
          "%Y-%m-%dT%H:%M:%S"
        )
      }

      jsonlite::toJSON(
        list(
          success = TRUE,
          context_id = context$id,
          attachment_id = attachment_id,
          attachment = attachment
        ),
        auto_unbox = TRUE, pretty = FALSE
      )
    },
    error = function(e) {
      jsonlite::toJSON(
        list(
          success = FALSE,
          context_id = context$id,
          attachment_id = attachment_id,
          error = conditionMessage(e)
        ),
        auto_unbox = TRUE, pretty = FALSE
      )
    }
  )

  result
}
