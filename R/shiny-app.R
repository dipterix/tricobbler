#' @name tricobbler-app
#' @title Launch \pkg{tricobbler} Chat Application
#'
#' @description
#' Launches a standalone \pkg{shiny} application wrapping the
#' \pkg{tricobbler} chat module. This is a convenience function
#' similar to \code{shinychat::chat_app()} but with the full
#' \pkg{tricobbler} sidebar for resource discovery and workflow
#' orchestration.
#'
#' @param client an \code{ellmer::Chat} object or \code{NULL}.
#'   If provided, the chat session is already configured. If
#'   \code{NULL}, users select a provider and model from the
#'   \verb{UI}.
#' @param tools,skills,workflows passed to
#'   \code{\link{tricobbler_chat_server}()}; control resource
#'   discovery scope. See \code{\link{discover_resources}()} for
#'   details.
#' @param system_prompt character or \code{NULL}, default system
#'   prompt for the chat.
#' @param title character, window and sidebar title
#'   (default: \code{"TriCobbler"}).
#' @param ... additional arguments passed to
#'   \code{shiny::shinyApp()}.
#'
#' @returns A \code{shiny::shinyApp} object (launched interactively
#'   or returned for deployment).
#'
#' @examples
#' \dontrun{
#' # With existing client
#' chat <- ellmer::chat_openai(model = "gpt-4o")
#' tricobbler_app(chat)
#'
#' # Without client (select provider in UI)
#' tricobbler_app()
#'
#' # Restrict to specific resources
#' tricobbler_app(
#'   tools = "tricobbler",
#'   skills = "/path/to/skills",
#'   workflows = FALSE
#' )
#' }
#'
#' @export
tricobbler_app <- function(client = NULL,
                           tools = TRUE,
                           skills = TRUE,
                           workflows = TRUE,
                           system_prompt = NULL,
                           title = "TriCobbler",
                           ...) {
  ui <- tricobbler_chat_ui("app", title = title)

  server <- function(input, output, session) {
    tricobbler_chat_server(
      "app",
      client = client,
      tools = tools,
      skills = skills,
      workflows = workflows,
      system_prompt = system_prompt
    )
  }

  shiny::shinyApp(ui = ui, server = server, ...)
}
