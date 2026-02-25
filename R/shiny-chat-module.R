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
#' \pkg{ellmer} for \verb{LLM} communication. Resources (tools,
#' skills, workflows) are discovered via
#' \code{\link{discover_resources}()} and presented in a sidebar
#' for selection.
#'
#' For multi-user \code{Shiny} apps, each session gets its own
#' cloned chat client.
#'
#' @section Provider configuration:
#' If \code{client} is provided, the \verb{LLM} provider
#' controls are populated from the chat object's configuration.
#' Users can still override via the \verb{UI} controls. If
#' \code{client = NULL}, users must select a provider and model
#' from the sidebar.
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
#' @seealso \code{\link{tricobbler_app}()},
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
#'   (typically a \pkg{bslib} page layout).
#'
#' @export
tricobbler_chat_ui <- function(id,
                               title = "TriCobbler",
                               ...) {
  require_shinychat()

  ns <- shiny::NS(id)

  sidebar_content <- shiny::tagList(
    # -- LLM Configuration --
    bslib::accordion(
      bslib::accordion_panel(
        "LLM Provider",
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
        ),
        value = "provider_panel",
        icon = shiny::icon("server")
      ),
      bslib::accordion_panel(
        "Resources",
        shiny::uiOutput(ns("resource_ui")),
        shiny::actionButton(
          ns("load_resources"), "Load Selected",
          class = "btn-sm btn-primary w-100 mt-2",
          icon = shiny::icon("plug")
        ),
        value = "resource_panel",
        icon = shiny::icon("toolbox")
      ),
      bslib::accordion_panel(
        "Workflows",
        shiny::uiOutput(ns("workflow_ui")),
        value = "workflow_panel",
        icon = shiny::icon("diagram-project")
      ),
      id = ns("sidebar_accordion"),
      open = "resource_panel"
    )
  )

  sidebar <- bslib::sidebar(
    sidebar_content,
    title = title,
    width = 320
  )

  bslib::page_sidebar(
    sidebar = sidebar,
    bslib::card(
      bslib::card_body(
        shinychat::chat_ui(ns("chat"), ...),
        class = "p-0",
        fillable = TRUE,
        fill = TRUE
      ),
      full_screen = TRUE,
      fill = TRUE
    ),
    shiny::uiOutput(ns("status_panel")),
    fillable = TRUE,
    title = title
  )
}


# ---- Server ----

#' @rdname tricobbler-shiny
#'
#' @param client an \code{ellmer::Chat} object or \code{NULL}.
#'   If provided, the chat is cloned per session. If \code{NULL},
#'   a new chat is constructed from the \verb{UI} provider
#'   selection.
#' @param tools logical, character vector, or \code{NULL}. Passed
#'   to \code{\link{discover_resources}()}; controls which
#'   \verb{MCP} tools are available for selection.
#' @param skills logical, character vector, or \code{NULL}. Passed
#'   to \code{\link{discover_resources}()}; controls which skills
#'   are available for selection.
#' @param workflows logical, character vector, or \code{NULL}.
#'   Passed to \code{\link{discover_resources}()}; controls which
#'   workflow \verb{YAML} files are available for selection.
#' @param system_prompt character or \code{NULL}, default system
#'   prompt used when constructing a chat from the \verb{UI}.
#'   Ignored when \code{client} is provided.
#'
#' @returns
#' \code{tricobbler_chat_server()}: invisible \code{NULL}
#'
#' @export
tricobbler_chat_server <- function(id,
                                   client = NULL,
                                   tools = TRUE,
                                   skills = TRUE,
                                   workflows = TRUE,
                                   system_prompt = NULL) {
  require_shinychat()

  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ---- Reactive state ----
    rv <- shiny::reactiveValues(
      chat = NULL,
      resources = NULL,
      loaded_tools = list(),
      # Workflow mode state
      workflow_running = FALSE,
      active_scheduler = NULL,
      wf_chat_agents = list(),
      # Context from the last workflow for the primary chat
      workflow_context = NULL
    )

    # ---- Initialize chat client ----
    shiny::observe({
      if (!is.null(client)) {
        # Clone per session for multi-user safety
        rv$chat <- client$clone(deep = TRUE)

        # Pre-fill UI from existing provider config
        config <- extract_chat_config(client)
        suffix_to_name <- .provider_suffix_to_name
        display_name <- suffix_to_name[config$provider]
        if (length(display_name) == 1L && !is.na(display_name)) {
          shiny::updateSelectInput(
            session, "provider",
            selected = config$provider
          )
        }
        if (length(config$model) == 1L) {
          shiny::updateTextInput(
            session, "model",
            value = config$model
          )
        }
        if (length(config$base_url) == 1L) {
          shiny::updateTextInput(
            session, "base_url",
            value = config$base_url
          )
        }
      }
    }) |> shiny::bindEvent(TRUE, once = TRUE)

    # ---- Discover resources ----
    shiny::observe({
      rv$resources <- discover_resources(
        tools = tools,
        skills = skills,
        workflows = workflows
      )
    }) |> shiny::bindEvent(TRUE, once = TRUE)

    # ---- Render resource checkboxes ----
    output$resource_ui <- shiny::renderUI({
      res <- rv$resources
      if (is.null(res)) {
        return(shiny::p("Discovering resources..."))
      }

      ui_parts <- list()

      # MCP Tools
      if (nrow(res$tools) > 0L) {
        tool_choices <- stats::setNames(
          res$tools$name,
          paste0(res$tools$name, " - ", substr(res$tools$description, 1, 40))
        )
        ui_parts <- c(ui_parts, list(
          shiny::tags$strong("MCP Tools"),
          shiny::checkboxGroupInput(
            ns("selected_tools"), NULL,
            choices = tool_choices,
            selected = tool_choices
          )
        ))
      }

      # Skills
      if (nrow(res$skills) > 0L) {
        skill_choices <- stats::setNames(
          res$skills$path,
          paste0(res$skills$name, " - ",
                 substr(res$skills$description, 1, 40))
        )
        ui_parts <- c(ui_parts, list(
          shiny::tags$strong("Skills"),
          shiny::checkboxGroupInput(
            ns("selected_skills"), NULL,
            choices = skill_choices,
            selected = skill_choices
          )
        ))
      }

      if (length(ui_parts) == 0L) {
        return(shiny::p(
          class = "text-muted",
          "No tools or skills discovered."
        ))
      }

      do.call(shiny::tagList, ui_parts)
    })

    # ---- Render workflow selector + per-agent config ----
    output$workflow_ui <- shiny::renderUI({
      res <- rv$resources
      if (is.null(res)) {
        return(shiny::p("Discovering workflows..."))
      }
      if (nrow(res$workflows) == 0L) {
        return(shiny::p(class = "text-muted", "No workflows found."))
      }

      # Labels: "skill_name / workflow_name"
      wf_labels <- sprintf(
        "%s / %s",
        res$workflows$skill_name,
        res$workflows$name
      )
      wf_choices <- stats::setNames(
        paste0(res$workflows$file, "::", res$workflows$name),
        wf_labels
      )

      shiny::tagList(
        shiny::selectInput(
          ns("selected_workflow"), "Select workflow",
          choices = c("(none)" = "", wf_choices)
        ),
        shiny::uiOutput(ns("workflow_agents_ui")),
        shiny::conditionalPanel(
          condition = sprintf(
            "input['%s'] !== ''",
            ns("selected_workflow")
          ),
          shiny::p(
            class = "text-muted small mt-2",
            "Type a message in the chat to run this workflow."
          )
        )
      )
    })

    # ---- Parse agents when workflow selection changes ----
    shiny::observeEvent(input$selected_workflow, {
      sel <- input$selected_workflow
      if (is.null(sel) || !nzchar(sel)) {
        rv$wf_chat_agents <- list()
        return()
      }
      parts <- strsplit(sel, "::", fixed = TRUE)[[1]]
      if (length(parts) != 2L) {
        rv$wf_chat_agents <- list()
        return()
      }
      rv$wf_chat_agents <- parse_workflow_chat_agents(
        parts[[1]], parts[[2]]
      )
    })

    # ---- Render per-agent provider controls ----
    output$workflow_agents_ui <- shiny::renderUI({
      agents <- rv$wf_chat_agents
      if (length(agents) == 0L) {
        return(NULL)
      }

      provider_opts <- c("(default)" = "", provider_choices())

      ui_parts <- lapply(agents, function(acfg) {
        aid <- acfg$id
        safe_id <- gsub("[^a-zA-Z0-9_]", "_", aid)
        cur_provider <- acfg$provider %||% ""
        cur_model <- acfg$model %||% ""
        cur_base_url <- acfg$base_url %||% ""

        shiny::tagList(
          shiny::tags$hr(class = "my-2"),
          shiny::tags$strong(
            sprintf("Agent: %s", aid),
            class = "d-block mb-1"
          ),
          shiny::selectInput(
            ns(paste0("wf_agent_provider_", safe_id)),
            "Provider",
            choices = provider_opts,
            selected = cur_provider
          ),
          shiny::textInput(
            ns(paste0("wf_agent_model_", safe_id)),
            "Model",
            value = cur_model
          ),
          shiny::textInput(
            ns(paste0("wf_agent_base_url_", safe_id)),
            "Base URL",
            value = cur_base_url
          )
        )
      })

      do.call(shiny::tagList, ui_parts)
    })

    # ---- Apply provider override ----
    shiny::observeEvent(input$apply_provider, {
      provider_val <- input$provider
      model_val <- input$model

      if (is.null(provider_val) || !nzchar(provider_val)) {
        # Keep existing client
        if (is.null(rv$chat)) {
          shiny::showNotification(
            "Please select a provider.",
            type = "warning"
          )
        }
        return()
      }

      base_url <- input$base_url
      if (!nzchar(base_url)) base_url <- NULL
      sys_prompt <- input$system_prompt
      if (!nzchar(sys_prompt)) sys_prompt <- system_prompt

      # model is optional: ellmer chat_*() constructors have defaults
      args <- list(provider = provider_val, system_prompt = sys_prompt,
                   base_url = base_url)
      if (nzchar(model_val)) {
        args$model <- model_val
      }
      new_chat <- do.call(create_chat, args)
      rv$chat <- new_chat
      # Re-register loaded tools on new chat
      for (tool_def in rv$loaded_tools) {
        rv$chat$register_tool(tool_def)
      }
      shiny::showNotification(
        paste("Chat configured:", provider_val, model_val),
        type = "message"
      )
    })

    # ---- Load selected resources ----
    shiny::observeEvent(input$load_resources, {
      chat <- rv$chat
      if (is.null(chat)) {
        shiny::showNotification(
          "Configure an LLM provider first.",
          type = "warning"
        )
        return()
      }

      loaded <- list()
      n_loaded <- 0L

      # Load MCP tools
      selected_tools <- input$selected_tools
      if (length(selected_tools) > 0L) {
        for (tool_name in selected_tools) {
          tool_def <- mcptool_read(tool_name)
          tool_obj <- mcptool_instantiate(tool_def)
          chat$register_tool(tool_obj)
          loaded[[tool_name]] <- tool_obj
          n_loaded <- n_loaded + 1L
        }
      }

      # Load skills
      selected_skills <- input$selected_skills
      if (length(selected_skills) > 0L) {
        for (skill_path in selected_skills) {
          skill <- Skill$new(skill_path)
          skill_tools <- skill$make_tools()
          for (nm in names(skill_tools)) {
            chat$register_tool(skill_tools[[nm]])
            loaded[[nm]] <- skill_tools[[nm]]
            n_loaded <- n_loaded + 1L
          }
        }
      }

      rv$loaded_tools <- c(rv$loaded_tools, loaded)

      shiny::showNotification(
        sprintf("Loaded %d tool(s)/skill(s).", n_loaded),
        type = "message"
      )
    })

    # ---- Chat interaction (normal mode: ExtendedTask) ----
    append_stream_task <- shiny::ExtendedTask$new(
      function(chat, user_input) {
        stream <- chat$stream_async(user_input)
        p <- promises::promise_resolve(stream)
        promises::then(p, function(stream) {
          shinychat::chat_append("chat", stream)
        })
      }
    )

    shiny::observeEvent(input$chat_user_input, {
      user_input <- input$chat_user_input
      sel <- input$selected_workflow

      # ---- Workflow mode ----
      if (!is.null(sel) && nzchar(sel)) {
        if (isTRUE(rv$workflow_running)) {
          shinychat::chat_append(
            "chat",
            paste(
              "A workflow is already running.",
              "Please wait for it to complete."
            ),
            role = "assistant",
            session = session
          )
          return()
        }
        .run_workflow(sel, user_input, rv, input, session)
        return()
      }

      # ---- Normal chat mode ----
      chat <- rv$chat
      if (is.null(chat)) {
        shinychat::chat_append(
          "chat",
          "Please configure an LLM provider first (sidebar).",
          role = "assistant",
          session = session
        )
        return()
      }
      # Inject workflow context if available
      wf_ctx <- rv$workflow_context
      if (!is.null(wf_ctx) && nzchar(wf_ctx)) {
        user_input <- sprintf(
          paste0(
            "Context from a recent workflow execution:\n\n%s\n\n",
            "---\n\nUser question: %s"
          ),
          wf_ctx, user_input
        )
        rv$workflow_context <- NULL
      }
      append_stream_task$invoke(chat, user_input)
    })

    # ---- Status panel ----
    output$status_panel <- shiny::renderUI({
      if (!isTRUE(rv$workflow_running)) {
        return(NULL)
      }
      bslib::card(
        bslib::card_header(
          shiny::tags$span(
            shiny::icon("spinner", class = "fa-spin me-2"),
            "Workflow running..."
          )
        ),
        bslib::card_body(
          shiny::p(class = "text-muted",
                   "Progress updates appear in the chat.")
        ),
        class = "border-info mt-2"
      )
    })

    invisible(NULL)
  })
}


# ---- Workflow runner (internal) ----

#' Launch a workflow from the chat input
#'
#' Collects per-agent provider overrides from the \verb{UI},
#' loads the scheduler with \code{work_path} pointing to the
#' original skill directory, sets the user prompt, registers
#' event listeners that stream progress to the chat, and starts
#' the \code{\link{AsyncScheduler}} asynchronously.
#'
#' @param sel character, the \code{"file::name"} selection value
#' @param user_input character, the user's prompt text
#' @param rv \code{shiny::reactiveValues}; must contain
#'   \code{wf_chat_agents}, \code{workflow_running},
#'   \code{active_scheduler}
#' @param input \code{shiny} input object (for reading agent
#'   \verb{UI} values)
#' @param session \code{shiny} session object (for namespaced
#'   \code{shinychat::chat_append} calls)
#' @noRd
.run_workflow <- function(sel, user_input, rv, input, session) {
  parts <- strsplit(sel, "::", fixed = TRUE)[[1]]
  if (length(parts) != 2L) {
    shinychat::chat_append(
      "chat", "Invalid workflow selection.",
      role = "assistant", session = session
    )
    return()
  }
  wf_file <- parts[[1]]
  wf_name <- parts[[2]]

  rv$workflow_running <- TRUE

  # -- Collect per-agent overrides from the sidebar inputs ------
  agent_overrides <- list()
  for (acfg in rv$wf_chat_agents) {
    aid <- acfg$id
    safe_id <- gsub("[^a-zA-Z0-9_]", "_", aid)
    prov <- input[[paste0("wf_agent_provider_", safe_id)]]
    mod  <- input[[paste0("wf_agent_model_", safe_id)]]
    burl <- input[[paste0("wf_agent_base_url_", safe_id)]]

    override <- list()
    if (!is.null(prov) && nzchar(prov)) {
      override$provider <- prov
    }
    if (!is.null(mod) && nzchar(mod)) {
      override$model <- mod
    }
    if (!is.null(burl) && nzchar(burl)) {
      override$base_url <- burl
    }
    if (length(override) > 0L) {
      agent_overrides[[aid]] <- override
    }
  }

  # -- Build config and load the scheduler ----------------------
  config <- scheduler_config_default()
  if (length(agent_overrides) > 0L) {
    config$agents <- agent_overrides
  }

  original_dir <- dirname(normalizePath(wf_file, mustWork = FALSE))
  scheduler <- workflow_load(
    wf_file,
    name = wf_name,
    scheduler_class = AsyncScheduler,
    config = config,
    work_path = original_dir
  )

  # Set user prompt on the manifest
  scheduler$manifest@master@parameters$user_prompt <- user_input
  rv$active_scheduler <- scheduler

  # -- Notify chat ---------------------------------------------
  shinychat::chat_append(
    "chat",
    sprintf(
      "**Workflow started:** `%s`\n\n**Prompt:** %s",
      wf_name, user_input
    ),
    role = "assistant",
    session = session
  )

  # -- Register event listeners ---------------------------------

  # Helper: format an attachment description for markdown display.
  # The description may contain literal newlines (\n) which

  # should render as markdown line breaks.
  format_description <- function(desc, max_chars = 2000L) {
    if (!is.character(desc) || !nzchar(paste(desc, collapse = ""))) {
      return("")
    }
    desc_str <- paste(desc, collapse = "\n")
    if (nchar(desc_str) > max_chars) {
      desc_str <- paste0(substr(desc_str, 1L, max_chars), "\n\n*(truncated)*")
    }
    desc_str
  }

  scheduler$on("init_stage.begin", function(event) {
    shinychat::chat_append(
      "chat",
      sprintf(
        "\U0001F504 Initializing stage **%s** ...",
        event$stage %||% "unknown"
      ),
      role = "assistant",
      session = session
    )
  })

  scheduler$on("runtime.dispatch", function(event) {
    shinychat::chat_append(
      "chat",
      sprintf(
        "\U0001F914 Running state **%s** (attempt %d)...",
        event$state_name %||% "unknown",
        event$attempt %||% 1L
      ),
      role = "assistant",
      session = session
    )
  })

  scheduler$on("runtime.resolved", function(event) {
    # event$result is an AgentRuntimeAttachmentResult (S7)
    attachment_s7 <- event$result

    if (!is.null(attachment_s7)) {
      state_name <- attachment_s7@state_name
      agent_id <- attachment_s7@agent_id

      shinychat::chat_append(
        "chat",
        sprintf(
          "\u2705 Agent **%s** finished (state: **%s**)",
          agent_id, state_name
        ),
        role = "assistant",
        session = session
      )

      # Render the S7 attachment via contents_shinychat()
      shinychat::chat_append(
        "chat",
        attachment_s7,
        role = "assistant",
        session = session
      )
    } else {
      shinychat::chat_append(
        "chat",
        "\u2705 A state completed (success)",
        role = "assistant",
        session = session
      )
    }
  })

  scheduler$on("runtime.errored", function(event) {
    shinychat::chat_append(
      "chat",
      sprintf(
        "\u26A0\uFE0F %s",
        event$message %||% "A runtime encountered an error."
      ),
      role = "assistant",
      session = session
    )
  })

  scheduler$on("runtime.exhausted", function(event) {
    shinychat::chat_append(
      "chat",
      sprintf(
        "\u274C %s",
        event$message %||% "A runtime exhausted all retries."
      ),
      role = "assistant",
      session = session
    )
  })

  scheduler$on("runtime.redirect", function(event) {
    shinychat::chat_append(
      "chat",
      sprintf(
        "\U0001F500 %s",
        event$message %||% "Redirecting to upstream state."
      ),
      role = "assistant",
      session = session
    )
  })

  scheduler$on("stage.completed", function(event) {
    shinychat::chat_append(
      "chat",
      sprintf(
        "\u2705 **Stage completed:** **%s**",
        event$stage %||% "unknown"
      ),
      role = "assistant",
      session = session
    )
  })

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

  # -- Start async execution ------------------------------------
  result_promise <- scheduler$start()

  # -- Listen for scheduler.completed to append the last attachment
  scheduler$on("scheduler.completed", function(event) {
    attachment_s7 <- event$result

    final_msg <- sprintf(
      "\U0001F389 **Workflow `%s` completed successfully.**",
      wf_name
    )
    shinychat::chat_append(
      "chat", final_msg,
      role = "assistant", session = session
    )

    if (!is.null(attachment_s7)) {
      shinychat::chat_append(
        "chat",
        attachment_s7,
        role = "assistant",
        session = session
      )
    }
  })

  promises::then(
    result_promise,
    onFulfilled = function(value) {
      rv$workflow_running <- FALSE

      # -- Store workflow context for the primary chat -----------
      # Gather a text summary from the last attachment or chat turns
      summary_text <- ""

      last_att <- scheduler$context$last_results(
        items = 1L, simplify = TRUE
      )
      if (is.list(last_att)) {
        summary_text <- format_description(last_att$description,
                                           max_chars = 4000L)
      }

      if (!nzchar(summary_text)) {
        chat_turns <- scheduler$context$chat_content
        if (length(chat_turns) > 0L) {
          turn_texts <- vapply(chat_turns, function(turn) {
            paste(vapply(turn@contents, function(c) {
              if (S7::S7_inherits(c, ellmer::ContentText)) {
                c@text
              } else {
                ""
              }
            }, character(1L)), collapse = "\n")
          }, character(1L))
          turn_texts <- turn_texts[nzchar(turn_texts)]
          if (length(turn_texts) > 0L) {
            summary_text <- paste(
              utils::tail(turn_texts, 3L),
              collapse = "\n\n---\n\n"
            )
          }
        }
      }

      rv$workflow_context <- summary_text

      # -- Reset workflow selector to (none) ---------------------
      shiny::updateSelectInput(
        session, "selected_workflow",
        selected = ""
      )
    },
    onRejected = function(err) {
      rv$workflow_running <- FALSE
      shinychat::chat_append(
        "chat",
        sprintf(
          "\u274C **Workflow `%s` failed:** %s",
          wf_name, conditionMessage(err)
        ),
        role = "assistant",
        session = session
      )

      # -- Reset workflow selector to (none) ---------------------
      shiny::updateSelectInput(
        session, "selected_workflow",
        selected = ""
      )
    }
  )
}
