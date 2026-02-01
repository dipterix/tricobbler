#' Configure \verb{MCP} Tool Output Settings
#'
#' @description
#' (This function is intended for \verb{MCP} calls directly, not for end-users).
#' Adjusts runtime settings for \verb{MCP} tool output formatting. Controls when
#' results are returned as JSON versus formatted text. When JSON serialization
#' fails (due to size or complexity), tools automatically fall back to
#' human-readable text output via \code{\link{mcp_describe}}.
#'
#' ## When to Use
#' Need to adjust output format for large results or token optimization.
#' Typically not needed unless outputs fail serialization or are too verbose.
#'
#' ## Best Practices
#' - DO: Increase max_size_for_json_output if results too large 
#'       (try 51200 or 102400)
#' - DO: Adjust max_print_lines based on verbosity needs (50-500)
#' - DON'T: Execute this tool immediately after dangerous operations
#' - DON'T: Accept default limits when results are truncated or fail
#'
#' @param max_size_for_json_output Integer. Maximum object size (in bytes)
#'   for JSON serialization attempt. Default is \code{NULL} (no change).
#'   When set, objects larger than this threshold will use text formatting.
#'   Recommended range: 5120-102400 bytes. Set to \code{Inf} to always
#'   attempt JSON serialization (not recommended).
#' @param max_print_lines Integer. Maximum number of lines for text output
#'   when using \code{\link{mcp_describe}}. Default is \code{NULL} (no change).
#'   Controls verbosity of fallback text formatting. Typical range: 50-200.
#' @param .state_env Environment. Internal state container (automatically
#'   provided by \verb{MCP} framework).
#'
#' @return List containing:
#' \describe{
#'   \item{success}{Logical, always \code{TRUE}}
#'   \item{message}{Character, confirmation message}
#'   \item{settings}{List, all current output settings with their values}
#' }
#'
#' @examples
#'
#' # This function is intended for MCP calls directly, not for end-users
#' tricobbler:::mcp_tool_config_set_outputs()
#'
#' @keywords mcp-tool mcp-category-configuration
#' @noRd
mcp_tool_config_set_outputs <- function(
    max_size_for_json_output = NULL,
    max_print_lines = NULL,
    .state_env = fastmap::fastmap()
) {
  updated <- character(0)

  # Update max_size_for_json_output if provided
  if (!is.null(max_size_for_json_output)) {
    if (
      !is.numeric(max_size_for_json_output) ||
        length(max_size_for_json_output) != 1
    ) {
      return(list(
        success = FALSE,
        error = "max_size_for_json_output must be a single numeric value"
      ))
    }
    if (
      !is.infinite(max_size_for_json_output) && max_size_for_json_output < 1024
    ) {
      return(list(
        success = FALSE,
        error = "max_size_for_json_output must be >= 1024 bytes or Inf"
      ))
    }
    .state_env$.max_size_for_json_output <- as.numeric(max_size_for_json_output)
    updated <- c(updated, "max_size_for_json_output")
  }

  # Update max_print_lines if provided
  if (!is.null(max_print_lines)) {
    if (!is.numeric(max_print_lines) || length(max_print_lines) != 1) {
      return(list(
        success = FALSE,
        error = "max_print_lines must be a single numeric value"
      ))
    }
    if (max_print_lines < 10) {
      return(list(
        success = FALSE,
        error = "max_print_lines must be >= 10"
      ))
    }
    .state_env$.max_print_lines <- as.integer(max_print_lines)
    updated <- c(updated, "max_print_lines")
  }

  # Get current settings (with defaults)
  current_max_size <- .state_env$.max_size_for_json_output
  if (
    is.null(current_max_size) ||
      length(current_max_size) != 1 ||
      is.na(current_max_size)
  ) {
    current_max_size <- getOption(
      "tricobbler.mcp.max_size_for_json_output",
      20480
    )
  }

  current_max_lines <- .state_env$.max_print_lines
  if (
    is.null(current_max_lines) ||
      length(current_max_lines) != 1 ||
      is.na(current_max_lines)
  ) {
    current_max_lines <- 100
  }

  # Build response
  message <- if (length(updated) > 0) {
    sprintf("Output settings updated: %s", paste(updated, collapse = ", "))
  } else {
    "No settings changed (returning current configuration)"
  }

  list(
    success = TRUE,
    message = message,
    settings = list(
      max_size_for_json_output = as.numeric(current_max_size),
      max_print_lines = as.integer(current_max_lines)
    )
  )
}
