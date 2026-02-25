#' @name tricobbler-shiny
#' @title \verb{Shiny} Chat Module for \pkg{tricobbler}
#'
#' @description
#' A \pkg{shiny} module providing an interactive chat interface with
#' integrated support for \pkg{tricobbler} skills, \verb{MCP} tools,
#' and workflow orchestration. The module offers two modes:
#'
#' \enumerate{
#'   \item \strong{Conversational chat}: Users chat with an
#'     \verb{LLM} that has \pkg{tricobbler} tools and skills
#'     attached.
#'   \item \strong{Workflow orchestration}: Users load and execute
#'     multi-agent workflow \verb{YAML} definitions with live status
#'     updates streamed to the chat.
#' }
#'
#' @details
#' The module uses \pkg{shinychat} for the chat \verb{UI} and
#' \pkg{ellmer} for \verb{LLM} communication. The caller is
#' responsible for resource discovery and injection: after calling
#' \code{tricobbler_chat_server()}, populate the returned
#' \code{resources} environment's \code{tools}, \code{skills},
#' and \code{workflows} \pkg{fastmap} objects, then call
#' \code{update_resources()} to refresh the sidebar.
#'
#' See \code{\link{tricobbler_demo_app}()} for a complete example
#' of this pattern.
#'
#' @section Provider configuration:
#' If \code{chat} is provided, the \verb{LLM} provider controls
#' are populated from the chat object's configuration. Users can
#' still override via the \verb{UI} controls.
#'
#' @section Workflow mode:
#' When a workflow is selected from the sidebar, the chat switches
#' to workflow orchestration mode. In this mode, user input is
#' stored as the workflow's \code{user_prompt} parameter and the
#' \code{\link{AsyncScheduler}} orchestrates multi-agent execution.
#' Progress events are streamed to the chat panel. Each chat agent
#' in the workflow can have its \verb{LLM} provider and model
#' individually configured via the sidebar. Only one workflow may
#' run at a time; select \code{"(none)"} to return to normal chat
#' mode.
#'
#' @seealso \code{\link{tricobbler_demo_app}()},
#'   \code{\link{discover_resources}()},
#'   \code{\link{provider_choices}()},
#'   \code{\link{create_chat}()}
NULL


# ---- Helpers ----

#' Parse chat agent configurations from a workflow \verb{YAML}
#'
#' Reads the \verb{YAML} file, resolves \code{$\{config\}}
#' template expressions with the default scheduler config, and
#' returns only the \code{type = "chat"} agents referenced by
#' the given workflow.
#'
#' @param file character, path to workflow \verb{YAML}
#' @param name character, workflow name to inspect
#' @param config list, template config for glue interpolation
#'   (default: \code{scheduler_config_default()})
#' @returns list of agent config lists (only \code{type == "chat"})
#' @noRd
parse_workflow_chat_agents <- function(file,
                                       name,
                                       config = scheduler_config_default()) {
  txt <- paste(readLines(file, warn = FALSE), collapse = "\n")
  parse_env <- new.env(parent = baseenv())
  parse_env$config <- config
  txt <- tryCatch(
    as.character(glue::glue(txt, .open = "${", .close = "}",
                            .envir = parse_env)),
    error = function(e) txt
  )
  li <- yaml::yaml.load(txt)

  wfs <- li$workflows %||% list()
  wf_names <- vapply(wfs, function(w) w$name %||% "", character(1L))
  idx <- match(name, wf_names)
  if (is.na(idx)) {
    return(list())
  }

  # Collect agent IDs referenced by this workflow
  wf <- wfs[[idx]]
  needed_ids <- character(0L)
  for (stage in wf$stages) {
    for (state in stage) {
      if (!is.null(state$agent_id)) {
        needed_ids <- c(needed_ids, state$agent_id)
      }
    }
  }
  needed_ids <- unique(needed_ids)

  # Return only chat agents
  agents <- li$agents %||% list()
  Filter(function(a) {
    a$id %in% needed_ids && identical(a$type, "chat")
  }, agents)
}


# ---- UI helpers ----

#' Build the sidebar control panel shared by page and modal layouts
#' @param ns namespace function from \code{shiny::NS()} or
#'   \code{session$ns}
#' @returns A \code{shiny::navlistPanel} with provider, workflow,
#'   and resource tabs
#' @noRd
tricobbler_chat_sidebar_ <- function(ns) {
  shiny::navlistPanel(
    id = ns("sidebar_nav"),
    well = TRUE,
    widths = c(12, 12),
    shiny::tabPanel(
      title = "LLM Provider",
      icon = shiny::icon("server"),
      shiny::selectInput(
        ns("provider"), "Provider",
        choices = c("(pre-configured)" = "", provider_choices()),
        selected = ""
      ),
      shiny::textInput(ns("model"), "Model", value = ""),
      shiny::textInput(ns("base_url"), "Base URL (optional)",
                       value = ""),
      shiny::textAreaInput(
        ns("system_prompt"), "System prompt",
        value = "", rows = 3L,
        placeholder = "Optional system prompt..."
      ),
      shiny::actionButton(
        ns("apply_provider"), "Apply",
        class = "btn-sm btn-outline-primary w-100 mt-2"
      )
    ),
    shiny::tabPanel(
      title = "Workflows",
      icon = shiny::icon("diagram-project"),
      shiny::selectInput(
        inputId = ns("selected_workflow"),
        label = "Start a workflow",
        choices = "(none)"
      ),
      shiny::uiOutput(ns("workflow_agents_ui")),
      shiny::conditionalPanel(
        condition = sprintf(
          "input['%s'] !== '(none)'",
          ns("selected_workflow")
        ),
        shiny::p(
          class = "text-muted small mt-2",
          "Type a message in the chat to run this workflow."
        )
      )
    ),
    shiny::tabPanel(
      title = "Resources",
      icon = shiny::icon("toolbox"),

      # Skills
      shiny::checkboxGroupInput(
        inputId = ns("selected_skills"),
        label = "Skills",
        choices = character(),
        selected = character()
      ),

      # MCP Tools
      shiny::checkboxGroupInput(
        inputId = ns("selected_tools"),
        label = "Tools",
        choices = character(),
        selected = character()
      ),

      shiny::actionButton(
        inputId = ns("load_resources"),
        label = "Load Selected",
        class = "btn-sm btn-primary w-100 mt-2",
        icon = shiny::icon("plug")
      )
    )
  )
}


# ---- UI ----

#' @rdname tricobbler-shiny
#'
#' @param id character, the \code{Shiny} module \verb{ID}
#' @param title character, title displayed in the sidebar header
#'   (default: \code{"TriCobbler"})
#' @param ... additional arguments passed to
#'   \code{shinychat::chat_ui()}
#'
#' @returns
#' \code{tricobbler_chat_ui()}: A \code{Shiny} \verb{UI} element
#'   (a \code{fluidPage} with a two-column layout).
#'
#' @export
tricobbler_chat_ui <- function(id,
                               title = "TriCobbler",
                               ...) {
  require_shinychat()

  ns <- shiny::NS(id)

  shiny::fluidPage(
    title = title,
    shiny::fluidRow(
      shiny::column(
        width = 4,
        shiny::h4(title),
        tricobbler_chat_sidebar_(ns)
      ),
      shiny::column(
        width = 8,
        shinychat::chat_ui(ns("chat"), ...),
        shiny::uiOutput(ns("status_panel"))
      )
    )
  )
}


#' @rdname tricobbler-shiny
#'
#' @param label character, text displayed on the button
#'   (default: \code{"Open Chat"})
#' @param icon a \code{Shiny} icon tag for the button
#'   (default: \code{shiny::icon("comments")})
#'
#' @returns
#' \code{tricobbler_chat_button()}: A \code{Shiny}
#'   \code{actionButton}. Clicking it opens a full-screen modal
#'   dialog containing the same chat interface as
#'   \code{tricobbler_chat_ui()}. Requires the corresponding
#'   \code{tricobbler_chat_server()} to be registered for the
#'   same module \code{id}.
#'
#' @export
tricobbler_chat_button <- function(id,
                                   label = "Open Chat",
                                   icon = shiny::icon("comments"),
                                   ...) {
  ns <- shiny::NS(id)
  shiny::actionButton(
    ns("open_chat_modal"),
    label = label,
    icon = icon,
    ...
  )
}


# ---- Server ----

#' @rdname tricobbler-shiny
#'
#' @param input,output,session standard \code{Shiny} module
#'   arguments, passed from the enclosing
#'   \code{shiny::moduleServer()} call.
#' @param chat an \code{ellmer::Chat} object. The chat client
#'   used for conversational mode.
#'
#' @returns
#' \code{tricobbler_chat_server()}: A named \code{list} with the
#' following elements:
#' \describe{
#'   \item{\code{reactives}}{\code{reactiveValues} object tracking
#'     module state. Named fields:
#'     \code{workflow_running} (logical),
#'     \code{workflow_context} (text summary carried into the next
#'     normal-mode chat turn), \code{update_chat} and
#'     \code{update_resources} (timestamp sentinels).}
#'   \item{\code{resources}}{Plain \pkg{base} environment exposing
#'     four mutable slots that the caller populates before calling
#'     \code{update_resources()}:
#'     \code{chat} (\code{ellmer::Chat} or \code{NULL}),
#'     \code{tools} (\pkg{fastmap} of \code{ellmer::ToolDef}),
#'     \code{skills} (\pkg{fastmap} of \code{TricobblerSkill}),
#'     \code{workflows} (\pkg{fastmap} of
#'     \code{TricobblerScheduler}).}
#'   \item{\code{extract_tools}}{\code{function()} --- collects and
#'     returns all active \code{ellmer::ToolDef} objects from both
#'     the \code{tools} and \code{skills} fastmaps as a flat
#'     \code{list}.}
#'   \item{\code{update_chat}}{\code{function(provider, model = NULL,
#'     system_prompt = NULL, base_url = NULL, ...)} --- replaces the
#'     active \code{ellmer::Chat}. \code{provider} may be a
#'     provider-suffix string (e.g. \code{"openai"}), an existing
#'     \code{Chat} object (used as-is), or \code{NULL} to clear.
#'     Sidebar inputs are refreshed to reflect the new
#'     configuration.}
#'   \item{\code{update_resources}}{\code{function(selected_tools,
#'     selected_skills, selected_workflow)} --- validates and
#'     re-renders the sidebar resource checkboxes and workflow
#'     dropdown. All three arguments are optional; when omitted,
#'     current sidebar selections are preserved.}
#'   \item{\code{active_workflow}}{\code{function()} --- returns the
#'     \code{TricobblerScheduler} currently selected in the
#'     \emph{Workflows} dropdown, or \code{NULL} when
#'     \code{"(none)"} is selected or the entry has been removed
#'     from \code{resources$workflows}.}
#'   \item{\code{run_workflow}}{\code{async function(user_input)} ---
#'     \pkg{coro} async function that stops any previous run,
#'     applies current sidebar agent configurations, registers
#'     progress event listeners, sets
#'     \code{manifest@master@parameters$user_prompt} to
#'     \code{user_input}, and awaits \code{scheduler$start()}.
#'     Workflow results are streamed to the chat panel via
#'     \pkg{shinychat}.}
#' }
#'
#' @export
tricobbler_chat_server <- function(input, output, session, chat = NULL) {

  require_shinychat()

  # initialize helpers
  ns <- session$ns


  local_reactives <- shiny::reactiveValues(
    update_chat = NULL,
    update_resources = NULL,
    workflow_running = FALSE,
    workflow_context = NULL
  )

  # It's the module writer's responsibility to maintain the instances
  # instances not comply will be dropped
  resources <- list2env(
    list(
      chat = chat,

      # map of MCP tools (`ellmer:::ToolDef`)
      tools = fastmap::fastmap(),

      # map of `Skill`
      skills = fastmap::fastmap(),

      # map of Scheduler/AsyncScheduler
      workflows = fastmap::fastmap()
    ),
    envir = new.env(parent = emptyenv())
  )

  extract_tools <- function() {
    tools <- unname(resources$tools$as_list())

    skills <- unname(unlist(
      recursive = FALSE,
      lapply(resources$skills$as_list(), function(skill) {
        skill$make_tools()
      })
    ))

    c(tools, skills)
  }

  update_chat <- function(provider, model = NULL, system_prompt = NULL, base_url = NULL, ...) {
    if (is.null(provider)) {
      resources$chat <- NULL
    } else if (inherits(provider, "Chat")) {
      resources$chat <- provider
    } else {
      resources$chat <- create_chat(
        provider,
        model = model,
        system_prompt = system_prompt,
        base_url = base_url
      )
    }
    if (!is.null(resources$chat)) {
      chat <- resources$chat

      chat$set_tools(extract_tools())

      config <- extract_chat_config(resources$chat)
      display_name <- provider_name_to_suffix(config$provider)
      shiny::updateSelectInput(
        session, "provider",
        selected = config$provider
      )

      if (length(config$model) == 1) {
        shiny::updateTextInput(
          session, "model",
          value = config$model
        )
      }

      if (length(config$base_url) == 1) {
        shiny::updateTextInput(
          session, "base_url",
          value = config$base_url
        )
      }
    }

    local_reactives$update_chat <- Sys.time()
  }

  update_resources <- function(selected_tools, selected_skills, selected_workflow) {

    lapply(resources$tools$keys(), function(tool_name) {
      tool <- resources$tools$get(tool_name)
      if (!inherits(tool, "ellmer::ToolDef")) {
        resources$tools$remove(tool_name)
      }
      return()
    })

    lapply(resources$skills$keys(), function(skill_name) {
      skill <- resources$skills$get(skill_name)
      if (!R6::is.R6(skill) || !inherits(skill, "TricobblerSkill")) {
        resources$skills$remove(skill_name)
      }
      return()
    })

    lapply(resources$workflows$keys(), function(workflow_name) {
      scheduler <- resources$workflows$get(workflow_name)
      if (!R6::is.R6(scheduler) || !inherits(scheduler, "TricobblerScheduler")) {
        resources$workflows$remove(workflow_name)
      }
      return()
    })

    if (missing(selected_tools)) {
      # characters or TRUE
      selected_tools <- shiny::isolate(input$selected_tools)
    }
    if (missing(selected_skills)) {
      selected_skills <- shiny::isolate(input$selected_skills)
    }
    if (missing(selected_workflow)) {
      selected_workflow <- character()
    }

    # resources UI needs to be updated
    local_reactives$update_resources <- structure(
      list(
        selected_tools = selected_tools,
        selected_skills = selected_skills,
        selected_workflow = selected_workflow
      ),
      timestamp = Sys.time()
    )
  }

  ## Observers

  # Apply LLM provider change
  shiny::bindEvent(
    shiny::observe({
      provider <- input$provider
      if (!nzchar(provider)) { return() }
      model <- trimws(input$model %||% "")
      base_url <- trimws(input$base_url %||% "")
      system_prompt <- trimws(input$system_prompt %||% "")
      update_chat(
        provider = provider,
        model = if (nzchar(model)) model,
        system_prompt = if (nzchar(system_prompt)) system_prompt,
        base_url = if (nzchar(base_url)) base_url
      )
    }),
    input$apply_provider,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  # Set tools & skills
  shiny::bindEvent(
    shiny::observe({
      chat <- resources$chat
      if (is.null(chat)) { return() }
      chat$set_tools(extract_tools())
    }),
    input$load_resources,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  # Update resource list UI
  shiny::bindEvent(
    shiny::observe({

      selected_resourced <- local_reactives$update_resources

      # Skills
      skill_choices <- resources$skills$keys()
      skill_selected <- as.character(selected_resourced$selected_skills)
      skill_selected <- skill_selected[skill_selected %in% skill_choices]
      skill_inputs <- shiny::updateCheckboxGroupInput(
        session = session,
        inputId = "selected_skills",
        choices = skill_choices,
        selected = as.character(skill_selected)
      )

      # MCP tools
      tool_choices <- resources$tools$keys()
      tool_selected <- as.character(selected_resourced$selected_tools)
      tool_selected <- tool_selected[tool_selected %in% tool_choices]
      tool_inputs <- shiny::updateCheckboxGroupInput(
        session = session,
        inputId = "selected_tools",
        choices = tool_choices,
        selected = as.character(tool_selected)
      )

      # Workflow - only one workflow is allowed at a time
      workflow_choices <- resources$workflows$keys()
      workflow_selected <- as.character(selected_resourced$selected_workflow)
      workflow_selected <- workflow_selected[workflow_selected %in% workflow_choices]
      if (length(workflow_selected) >= 1) {
        workflow_selected <- workflow_selected[[1]]
      } else {
        workflow_selected <- "(none)"
      }
      shiny::updateSelectInput(
        session = session,
        inputId = "selected_workflow",
        choices = c("(none)", workflow_choices),
        selected = workflow_selected
      )
    }),
    local_reactives$update_resources,
    ignoreNULL = FALSE, ignoreInit = FALSE
  )

  # Render workflow AI-agent configs in workflow
  active_workflow <- function() {
    sel <- shiny::isolate(input$selected_workflow)
    if (length(sel) != 1 || sel %in% c("", "(none)")) { return() }
    workflow <- resources$workflows$get(sel, missing = NULL)
    if (is.null(workflow)) { return() }
    if (!R6::is.R6(workflow) || !inherits(workflow, "TricobblerScheduler")) { return() }
    workflow
  }
  output$workflow_agents_ui <- shiny::renderUI({
    input$selected_workflow
    workflow <- active_workflow()
    if (is.null(workflow)) { return() }

    provider_opts <- c("(default)" = "", provider_choices())

    agent_keys <- workflow$agents$keys()
    lapply(agent_keys, function(agent_id) {
      agent <- workflow$agents$get(agent_id)
      safe_id <- gsub("[^a-zA-Z0-9_]", "_", agent@id)

      # agent <- workflow$agents$as_list()[[2]]
      if (!identical(agent@config$type, "chat")) { return(NULL) }

      shiny::tagList(
        shiny::tags$hr(class = "my-2"),
        shiny::tags$strong(
          sprintf("Agent: %s", agent@id),
          class = "d-block mb-1"
        ),
        shiny::selectInput(
          inputId = ns(paste0("wf_agent_provider_", safe_id)),
          label = "Provider",
          choices = provider_opts,
          selected = agent@config$provider %||% ""
        ),
        shiny::textInput(
          inputId = ns(paste0("wf_agent_model_", safe_id)),
          label = "Model",
          value = agent@config$model %||% ""
        ),
        shiny::textInput(
          inputId = ns(paste0("wf_agent_base_url_", safe_id)),
          label = "Base URL",
          value = agent@config$base_url %||% ""
        )
      )
    })
  })

  # set workflow AI-agent configs in workflow
  set_workflow_agents <- function() {
    workflow <- active_workflow()
    if (is.null(workflow)) { return() }
    providers <- provider_choices()

    # set agents
    agent_keys <- workflow$agents$keys()
    lapply(agent_keys, function(agent_id) {
      agent <- workflow$agents$get(agent_id)
      if (!identical(agent@config$type, "chat")) { return() }

      safe_id <- gsub("[^a-zA-Z0-9_]", "_", agent@id)
      provider <- input[[paste0("wf_agent_provider_", safe_id)]]
      model <- trimws(paste(input[[paste0("wf_agent_model_", safe_id)]], collapse = ""))
      base_url <- trimws(paste(input[[paste0("wf_agent_base_url_", safe_id)]], collapse = ""))

      if (!isTRUE(provider %in% providers)) { return() }

      config <- agent@config
      if (!identical(config$provider, provider)) {
        config <- list(
          type = "chat",
          description = config$description,
          id = config$id,
          echo = match.arg(config$echo, c("output", "all", "none"))
        )
      }

      config$provider <- provider
      if (nzchar(model)) {
        config$model <- model
      }
      if (nzchar(base_url)) {
        config$base_url <- base_url
      }
      agent <- reconstruct_chat_agent(config, id = agent@id, description = agent@description)
      workflow$agents$set(agent_id, agent)
      return()
    })
    return()
  }

  register_workflow_events <- function() {
    scheduler <- active_workflow()
    if (is.null(scheduler)) { return() }

    scheduler$on("init_stage.begin", function(event) {
      if (length(event$stage) == 1) {
        shinychat::chat_append_message(
          "chat", session = session,
          list(
            role = "assistant",
            content = sprintf("Initializing stage **%s** ...", event$stage)
          )
        )
      }
    })

    scheduler$on("runtime.dispatch", function(event) {
      shinychat::chat_append_message(
        "chat", operation = "replace", session = session,
        list(
          role = "assistant",
          content = sprintf(
            "Running state **%s** (attempt %d)...",
            event$state_name %||% "unknown",
            event$attempt %||% 1L
          )
        )
      )
    })

    scheduler$on("runtime.resolved", function(event) {
      # event$result is an AgentRuntimeAttachmentResult (S7)
      attachment_s7 <- event$result
      if (is.null(attachment_s7)) { return() }

      state_name <- attachment_s7@state_name
      agent_id <- attachment_s7@agent_id

      shinychat::chat_append_message(
        "chat", operation = "replace", session = session,
        list(
          content = sprintf(
            "\u2705 Agent **%s** finished (state: **%s**)",
            agent_id, state_name
          ),
          role = "assistant"
        )
      )

      # Render the S7 attachment via contents_shinychat()
      shinychat::chat_append(
        "chat",
        attachment_s7,
        role = "assistant",
        session = session
      )
    })

    scheduler$on("runtime.errored", function(event) {
      shinychat::chat_append_message(
        "chat", operation = "replace", session = session,
        list(
          content = sprintf(
            "\u26A0\uFE0F %s",
            event$message %||% "A runtime encountered an error... re-trying..."
          ),
          role = "assistant"
        )
      )
    })

    scheduler$on("runtime.exhausted", function(event) {
      attachment_s7 <- event$result
      shinychat::chat_append_message(
        "chat", operation = "replace", session = session,
        list(
          content = sprintf(
            "\u274C %s",
            event$message %||% "A runtime exhausted all retries."
          ),
          role = "assistant"
        )
      )

      if (is.null(attachment_s7)) { return() }
      shinychat::chat_append(
        "chat",
        attachment_s7,
        role = "assistant",
        session = session
      )
    })

    scheduler$on("runtime.redirect", function(event) {
      shinychat::chat_append_message(
        "chat", operation = "replace", session = session,
        list(
          content = event$message %||% "Redirecting to upstream state.",
          role = "assistant"
        )
      )
    })

    # TODO: on suspended, ask for further inputs
    scheduler$on("suspend", function(event) {
      shinychat::chat_append(
        "chat",
        sprintf(
          paste(
            "\u23F8\uFE0F **Workflow suspended** at state `%s`: %s",
            "\n\n*Automatically resuming...*"
          ),
          event$state_name %||% "unknown",
          event$message %||% ""
        ),
        role = "assistant",
        session = session
      )
      return("resume")
    })


    scheduler$on("scheduler.completed", function(event) {
      shinychat::chat_append_message(
        "chat", operation = "replace", session = session,
        list(
          content = "\U0001F389 **Workflow completed successfully.**",
          role = "assistant"
        )
      )

      attachment_s7 <- event$result
      if (!is.null(attachment_s7)) {
        shinychat::chat_append(
          "chat",
          attachment_s7,
          role = "assistant",
          session = session
        )

        # -- Store workflow context for the primary chat -----------
        # Gather a text summary from the last attachment or chat turns
        local_reactives$workflow_context <- attachment_s7@value %||% attachment_s7@error
      }
    })
  }

  run_workflow <- coro::async(function(user_input) {
    workflow <- active_workflow()
    if (is.null(workflow)) { return() }
    workflow$stop()

    set_workflow_agents()
    register_workflow_events()

    workflow$manifest@master@parameters$user_prompt <- user_input

    local_reactives$workflow_running <- TRUE

    tryCatch(
      {
        coro::await(workflow$start())
        local_reactives$workflow_running <- FALSE
        shiny::updateSelectInput(
          session, "selected_workflow",
          selected = "(none)"
        )
      },
      error = function(e) {
        local_reactives$workflow_running <- FALSE
        shiny::updateSelectInput(
          session, "selected_workflow",
          selected = "(none)"
        )
        # warning(e)
        shinychat::chat_append(
          "chat",
          sprintf("**Workflow failed:** %s", conditionMessage(e)),
          role = "assistant",
          session = session
        )
      }
    )
  })


  # ---- Chat interaction (normal mode: ExtendedTask) ----
  append_stream_task <- shiny::ExtendedTask$new(
    function(chat, ...) {
      stream <- chat$stream_async(...)
      p <- promises::promise_resolve(stream)
      promises::then(p, function(stream) {
        shinychat::chat_append("chat", stream)
      })
    }
  )

  shiny::bindEvent(
    shiny::observe({
      user_input <- input$chat_user_input
      workflow <- active_workflow()

      if (!is.null(workflow)) {
        # ---- Workflow mode ----
        if (local_reactives$workflow_running) {
          shinychat::chat_append(
            id = "chat",
            response = "A workflow is already running. Please wait for it to complete.",
            role = "assistant",
            session = session
          )
          return()
        }
        return(run_workflow(user_input))
      }

      # ---- Normal chat mode ----
      chat <- resources$chat
      if (is.null(chat)) {
        shinychat::chat_append(
          "chat",
          "Please configure an LLM provider first.",
          role = "assistant",
          session = session
        )
        return()
      }

      # Inject workflow context if available
      wf_ctx <- trimws(paste(local_reactives$workflow_context, collapse = "\n"))
      local_reactives$workflow_context <- NULL

      if ( nzchar(wf_ctx) ) {
        append_stream_task$invoke(
          chat = chat,
          mcp_attach(
            wf_ctx,
            .header = "## Context from a recent workflow execution", .wrap_code = FALSE
          ),
          "## User question:",
          user_input
        )
      } else {
        append_stream_task$invoke(chat = chat, user_input)
      }

    }),
    input$chat_user_input,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  # ---- Modal launcher (for tricobbler_chat_button) ----
  shiny::bindEvent(
    shiny::observe({
      shiny::showModal(shiny::modalDialog(
        shiny::tags$style(
          ".modal-dialog.modal-xl { width: 95vw; max-width: 1200px; }"
        ),
        shiny::fluidRow(
          shiny::column(
            width = 4,
            tricobbler_chat_sidebar_(ns)
          ),
          shiny::column(
            width = 8,
            shinychat::chat_ui(ns("chat"), height = "calc(85vh - 150px)"),
            shiny::uiOutput(ns("status_panel"))
          )
        ),
        title = "TriCobbler",
        size = "xl",
        easyClose = FALSE,
        footer = shiny::modalButton("Close")
      ))

      selected_resourced <- as.list(shiny::isolate(local_reactives$update_resources))
      update_resources(
        selected_tools = selected_resourced$selected_tools,
        selected_skills = selected_resourced$selected_skills,
        selected_workflow = selected_resourced$selected_workflow
      )
    }),
    input$open_chat_modal,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  # ---- Status panel ----
  output$status_panel <- shiny::renderUI({
    if (!isTRUE(local_reactives$workflow_running)) {
      return(NULL)
    }
    shiny::wellPanel(
      shiny::tags$strong(
        shiny::icon("spinner", class = "fa-spin me-2"),
        "Workflow running..."
      ),
      shiny::p(
        class = "text-muted",
        "Progress updates appear in the chat."
      )
    )
  })

  list(
    reactives = local_reactives,
    resources = resources,
    extract_tools = extract_tools,
    update_chat = update_chat,
    update_resources = update_resources,
    active_workflow = active_workflow,
    run_workflow = run_workflow
  )
}


