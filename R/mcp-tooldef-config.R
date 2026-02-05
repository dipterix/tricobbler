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
#' @param tricobbler.mcp_describe.max_size Integer. Maximum number of lines
#'   for text output when using \code{\link{mcp_describe}}.
#'   Default is \code{NULL} (no change).
#'   Controls verbosity of fallback text formatting. Typical range: 50-200.
#' @param .runtime AgentRuntime object (injected automatically by
#'   \code{\link{mcptool_instantiate}} when tools are used within agent
#'   execution). Should not be set manually.
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
    tricobbler.mcp_describe.max_size = NULL, # nolint: object_name_linter.
    .runtime = NULL
) {
  updated <- list()
  settings <- list()

  # Get context from runtime (may be NULL)
  ctx <- if (!is.null(.runtime)) .runtime$context else NULL

  # Update tricobbler.mcp_describe.max_size if provided
  if (!is.null(tricobbler.mcp_describe.max_size)) {
    tricobbler.mcp_describe.max_size <- as.integer( # nolint
      tricobbler.mcp_describe.max_size
    )
    if (
      !is.numeric(tricobbler.mcp_describe.max_size) ||
        length(tricobbler.mcp_describe.max_size) != 1 ||
        !isTRUE(tricobbler.mcp_describe.max_size > 0L)
    ) {
      return(list(
        success = FALSE,
        error = "`tricobbler.mcp_describe.max_size` must be a single positive integer" # nolint: line_length_linter.
      ))
    }
    if (!is.null(ctx)) {
      ctx$cache$set(
        "tricobbler.mcp_describe.max_size",
        tricobbler.mcp_describe.max_size
      )
      updated <- c(updated, "tricobbler.mcp_describe.max_size")
    }
  }
  settings$tricobbler.mcp_describe.max_size <- if (!is.null(ctx)) {
    ctx$cache$get("tricobbler.mcp_describe.max_size", 100L)
  } else {
    100L
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
    settings = settings
  )
}
