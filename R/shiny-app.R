#' @name tricobbler_demo_app
#' @title Launch \pkg{tricobbler} Demo Chat Application
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
#' @param tools,skills,workflows logical or character vector;
#'   controls resource discovery scope. Passed to
#'   \code{\link{discover_resources}()}.
#' @param system_prompt character or \code{NULL}, default system
#'   prompt for the chat.
#' @param title character, window and sidebar title
#'   (default: \code{"TriCobbler Demo App"}).
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
#' tricobbler_demo_app(chat)
#'
#' # Without client (select provider in UI)
#' tricobbler_demo_app()
#'
#' # Restrict to specific resources
#' tricobbler_demo_app(
#'   tools = "tricobbler",
#'   skills = "/path/to/skills",
#'   workflows = FALSE
#' )
#' }
#'
#' @export
tricobbler_demo_app <- function(
    client = NULL, tools = TRUE, skills = TRUE, workflows = TRUE,
    system_prompt = NULL, title = "TriCobbler Demo App", ...) {

  # ui <- tricobbler_chat_ui("app", title = title)
  ui <- shiny::fluidPage(
    tricobbler_chat_button("app", label = "Launch Chat")
  )

  chat <- client
  if (!is.null(system_prompt)) {
    chat$set_system_prompt(value = paste(system_prompt, collapse = "\n"))
  }

  rs <- discover_resources(tools = tools, skills = skills, workflows = workflows)

  # Initialize
  tools <- structure(
    names = gsub("-mcp_tool_", "::", rs$tools$name),
    lapply(rs$tools$name, function(tool_name) {
      tryCatch(
        {
          tool <- mcptool_read(tool_name)
          mcptool_instantiate(tool)
        },
        error = function(e) NULL
      )
    })
  )

  skills <- structure(
    names = rs$skills$name,
    lapply(rs$skills$path, function(skill_path) {
      tryCatch(
        Skill$new(path = skill_path),
        error = function(e) NULL
      )
    })
  )

  workflows <- structure(
    names = sprintf("%s (%s)", rs$workflows$skill_name, rs$workflows$name),
    lapply(seq_len(nrow(rs$workflows)), function(ii) {

      tryCatch(
        workflow_load(
          file = rs$workflows$file[[ii]],
          name = rs$workflows$name[[ii]],
          scheduler_class = AsyncScheduler
        ),
        error = function(e) NULL
      )
    })
  )

  server <- function(input, output, session) {

    shiny::moduleServer("app", module = function(input, output, session) {
      server_toolbox <- tricobbler_chat_server(
        input = input,
        output = output,
        session = session,
        chat = chat
      )

      resources <- server_toolbox$resources
      resources$tools$mset(.list = tools)
      resources$skills$mset(.list = skills)
      resources$workflows$mset(.list = workflows)
      server_toolbox$update_resources(
        selected_tools = names(tools),
        selected_skills = names(skills)
      )
    })
  }

  shiny::shinyApp(ui = ui, server = server, ...)
}
