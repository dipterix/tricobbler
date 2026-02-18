#' Attach Context to Chat Messages
#'
#' @description
#' Formats R objects and combines them with optional text to attach context
#' to chat messages. Similar to \code{btw::btw()}, this function wraps
#' formatted output in a structured format that chat models can easily
#' understand.
#'
#' This function uses \code{\link{mcp_describe}} internally to format objects
#' into human-readable text, then wraps the output in code blocks and returns
#' an \code{ellmer::ContentText} object suitable for use with
#' \code{ellmer::Chat$chat()}.
#'
#' @param ... R objects to format, or character strings to include.
#'   Each argument will be formatted using \code{mcp_describe()} unless it's
#'   a plain text string.
#' @param .header Character string for the context header.
#'   Default: \code{"## Context"}
#' @param .wrap_code Logical. Whether to wrap non-text objects in code blocks.
#'   Default: \code{TRUE}
#'
#' @return An \code{ellmer::ContentText} object containing the formatted
#'   context. This object can be passed directly to \code{chat$chat()}.
#'
#' @details
#' The function processes each argument as follows:
#' \itemize{
#'   \item Package references like \code{"{packagename}"} are expanded to show
#'     all help topics from that package in JSON format
#'   \item Plain text strings (single character vectors) are used as-is
#'   \item Other R objects are formatted using \code{mcp_describe()}
#'   \item Formatted objects are wrapped in code blocks if
#'     \code{.wrap_code = TRUE}
#'   \item All parts are combined with the header into a single text block
#' }
#'
#' The resulting \code{ContentText} object is compatible with \code{ellmer}
#' chat functions and will be automatically processed when passed to
#' \code{chat$chat()}.
#'
#' @examples
#' # Single object
#' mcp_attach(sessionInfo())
#'
#' # Package help topics
#' mcp_attach("{tricobbler}")
#'
#' # Multiple objects with question
#' mcp_attach(sessionInfo(), mtcars, "What packages and data are available?")
#'
#' \dontrun{
#' # Use in chat
#' chat <- ellmer::chat_anthropic()
#' response <- chat$chat(
#'   mcp_attach(sessionInfo(), "What R version am I using?")
#' )
#' }
#'
#' @seealso
#' \code{\link{mcp_describe}} for the underlying formatting function,
#'
#' @export
mcp_attach <- function(..., .header = "## Context", .wrap_code = TRUE) {
  args <- list(...)

  if (length(args) == 0) {
    if (requireNamespace("ellmer", quietly = TRUE)) {
      return(ellmer::ContentText(""))
    }
    return("")
  }

  # Format each argument
  formatted <- lapply(args, function(x) {
    # Check if it's a package reference like "{packagename}"
    if (
      is.character(x) && length(x) == 1 &&
      grepl("^\\{[a-zA-Z][a-zA-Z0-9.]*\\}$", x)
    ) {
      # Extract package name
      pkg_name <- gsub("[\\{\\}]", "", x)

      # Try to get package help topics
      topics <- tryCatch({
        help_db <- utils::help.search(".", package = pkg_name, rebuild = TRUE)
        if (length(help_db$matches) > 0 && nrow(help_db$matches) > 0) {
          matches <- help_db$matches
          topic_list <- lapply(seq_len(nrow(matches)), function(i) {
            list(
              topic_id = as.character(matches[i, "Topic"]),
              title = as.character(matches[i, "Title"]),
              aliases = list(as.character(matches[i, "Topic"]))
            )
          })

          # Format as JSON
          json_str <- jsonlite::toJSON(
            topic_list,
            pretty = FALSE,
            auto_unbox = FALSE
          )
          paste0(x, "\n```json\n", json_str, "\n```")
        } else {
          # No help found, just return the package reference
          x
        }
      }, error = function(e) {
        # If package not found or error, just return the original string
        x
      })

      return(topics)
    }

    if (is.character(x) && length(x) == 1 && !grepl("^\n", x)) {
      # Plain text string - use as-is
      x
    } else {
      # Use mcp_describe to format objects
      desc <- mcp_describe(x)

      # Wrap in code block if requested
      if (.wrap_code) {
        paste0(c("````r", desc, "````"), collapse = "\n")
      } else {
        desc
      }
    }
  })

  # Combine with header
  parts <- c(.header, "", unlist(formatted))
  result_text <- paste(parts, collapse = "\n")

  # Return as ContentText if ellmer is available
  if (requireNamespace("ellmer", quietly = TRUE)) {
    return(ellmer::ContentText(result_text))
  }

  # Fallback to plain string
  result_text
}
