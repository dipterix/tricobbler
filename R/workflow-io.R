#' @include class-manifest.R
#' @include generic-as_agent.R
NULL

# ---------------------------------------------------------------------------
# Provider Name Lookup Table
# ---------------------------------------------------------------------------
# Maps ellmer provider@name (display string) to chat_*() function suffix
# used by the ellmer::chat() dispatcher.
# Fallback: tolower(gsub("[^a-zA-Z0-9]", "_", provider@name))

.provider_name_to_suffix <- c(
  "Anthropic"         = "anthropic",
  "AWS/Bedrock"       = "aws_bedrock",
  "Azure/OpenAI"      = "azure_openai",
  "Cloudflare"        = "cloudflare",
  "Databricks"        = "databricks",
  "DeepSeek"          = "deepseek",
  "gitHub"            = "github",
  "Google/Gemini"     = "google_gemini",
  "Google/Vertex"     = "google_vertex",
  "Groq"              = "groq",
  "HuggingFace"       = "huggingface",
  "Mistral"           = "mistral",
  "Ollama"            = "ollama",
  "OpenAI"            = "openai",
  "OpenRouter"        = "openrouter",
  "Perplexity"        = "perplexity",
  "PortkeyAI"         = "portkey",
  "Snowflake/Cortex"  = "snowflake",
  "VLLM"              = "vllm"
)

# Reverse lookup: suffix -> display name
.provider_suffix_to_name <- structure(
  names = .provider_name_to_suffix,
  names(.provider_name_to_suffix)
)

#' Convert \pkg{ellmer} provider display name to \code{chat_*()} suffix
#' @param provider_name character, the \code{provider@@name} display string
#' @returns character, the suffix for \code{ellmer::chat_\{suffix\}()}
#' @noRd
provider_name_to_suffix <- function(provider_name) {
  idx <- match(provider_name, names(.provider_name_to_suffix))
  if (!is.na(idx)) {
    suffix <- .provider_name_to_suffix[[idx]]
  } else {
    # Fallback: normalize display name to dispatcher string
    suffix <- tolower(gsub("[^a-zA-Z0-9]", "_", provider_name))
  }
  suffix
}


# ---------------------------------------------------------------------------
# Extract Chat Configuration from a Chat Object
# ---------------------------------------------------------------------------

#' Extract serialization configuration from an \pkg{ellmer} Chat object
#'
#' @description Extracts provider name, model, base URL, and parameters
#'   from a \code{Chat} object for \verb{YAML} serialization. Credentials
#'   are never included; they come from environment variables at runtime.
#'
#' @param chat an \pkg{ellmer} \code{Chat} object
#' @returns A named list with fields: \code{type}, \code{provider},
#'   \code{model}, \code{base_url}, \code{params}, \code{extra_args},
#'   \code{extra_headers}. Empty fields are omitted.
#' @noRd
extract_chat_config <- function(chat) {
  if (!inherits(chat, "Chat")) {
    stop("'chat' must be an ellmer Chat object")
  }
  provider <- chat$get_provider()
  suffix <- provider_name_to_suffix(provider@name)

  config <- list(
    type = "chat",
    provider = suffix,
    model = provider@model,
    base_url = provider@base_url
  )

  # Serialize params (from ellmer::params()) - drop NULL entries
  p <- provider@params
  if (is.list(p) && length(p) > 0) {
    p <- Filter(Negate(is.null), p)
    if (length(p) > 0) {
      config$params <- p
    }
  }

  # Extra args
  ea <- provider@extra_args
  if (is.list(ea) && length(ea) > 0) {
    config$extra_args <- ea
  }

  # Extra headers
  eh <- provider@extra_headers
  if (is.character(eh) && length(eh) > 0) {
    config$extra_headers <- eh
  }

  config
}


# ---------------------------------------------------------------------------
# Serialize Manifest to Stage-Grouped YAML Structure
# ---------------------------------------------------------------------------

#' Convert a \code{\link{Manifest}} to the workflow \verb{YAML} structure
#'
#' @description Produces a flat list with the master fields
#'   (\code{name}, \code{version}, \code{parameters}, etc.) at the
#'   top level, plus \code{stages} (named by stage, each containing
#'   an array of state definitions).
#'   Individual states omit the \code{stage} field since it is implied
#'   by the parent key.
#'
#' @param manifest a \code{\link{Manifest}} object
#' @returns A named list suitable for \code{yaml::as.yaml()}
#' @noRd
manifest_to_workflow_list <- function(manifest) {
  stopifnot(S7::S7_inherits(manifest, Manifest))

  master_list <- as.list(manifest@master, recursive = TRUE)

  # Build stage-grouped structure
  stage_names <- manifest@master@stages
  stages_list <- structure(
    names = stage_names,
    lapply(stage_names, function(stg) {
      states_in_stage <- Filter(
        function(s) s@stage == stg,
        manifest@states
      )
      lapply(states_in_stage, function(s) {
        sl <- as.list(s, recursive = TRUE)
        # Remove the 'stage' field - it's implied by the parent key
        sl$stage <- NULL
        sl
      })
    })
  )

  master_list$stages <- stages_list
  master_list
}


#' Reconstruct a \code{\link{Manifest}} from the workflow \verb{YAML}
#' structure
#'
#' @param wf_list A named list with \code{name}, \code{version},
#'   \code{stages}, etc. at the top level, as read from the
#'   workflow \verb{YAML}
#' @returns A validated \code{\link{Manifest}} object
#' @noRd
workflow_list_to_manifest <- function(wf_list) {
  master_policy <- MasterPolicy(
    name = wf_list$name,
    description = concatern(
      wf_list$description,
      trim_lines = TRUE,
      trim_collapsed = TRUE
    ),
    version = wf_list$version,
    stages = names(wf_list$stages),
    parameters = as.list(wf_list$parameters)
  )

  # Flatten stage-grouped states back into a flat list,

  # injecting the stage field from the parent key
  state_policies <- list()
  for (stage_name in names(wf_list$stages)) {
    states_in_stage <- wf_list$stages[[stage_name]]
    for (state in states_in_stage) {
      state$stage <- stage_name
      state$description <- concatern(
        state$description,
        trim_lines = TRUE,
        trim_collapsed = TRUE
      )
      state$parameters <- as.list(state$parameters)
      # Fix resources: YAML may read empty arrays as list()
      if (
        is.null(state$resources) ||
        (is.list(state$resources) && length(state$resources) == 0)
      ) {
        state$resources <- character(0L)
      } else {
        state$resources <- as.character(state$resources)
      }
      # Fix depends_on: YAML may read empty/missing as NULL
      if (
        is.null(state$depends_on) || length(state$depends_on) == 0
      ) {
        state$depends_on <- StateDeps()
      } else {
        state$depends_on <- StateDeps(.list = state$depends_on)
      }
      state_policies <- c(state_policies, list(do.call(StatePolicy, state)))
    }
  }

  Manifest(master = master_policy, states = state_policies)
}


# ---------------------------------------------------------------------------
# Agent Config Reconstruction
# ---------------------------------------------------------------------------

#' Reconstruct an \code{\link{Agent}} from a config list
#'
#' @param agent_config A named list with at least \code{id} and \code{type}
#' @param base_dir character, base directory for resolving relative paths
#'   (typically \code{dirname(workflow_file)})
#' @returns An \code{\link{Agent}} object
#' @noRd
reconstruct_agent <- function(agent_config, base_dir = ".") {
  id <- agent_config$id
  agent_type <- agent_config$type
  description <- agent_config$description %||% ""

  agent <- switch(
    agent_type,
    "chat" = {
      reconstruct_chat_agent(agent_config, id = id, description = description)
    },
    "script" = {
      reconstruct_script_agent(
        agent_config, base_dir = base_dir,
        id = id, description = description
      )
    },
    "tool_definition" = {
      reconstruct_tool_agent(agent_config, id = id, description = description)
    },
    "package_function" = {
      reconstruct_package_agent(
        agent_config, id = id, description = description
      )
    },
    stop(sprintf("Unknown agent type: '%s'", agent_type))
  )

  agent@config <- agent_config
  agent
}


#' @noRd
reconstruct_chat_agent <- function(config, id, description) {
  provider_suffix <- config$provider
  model <- config$model %||% NULL

  # Build arguments for ellmer::chat() or ellmer::chat_{suffix}()
  chat_args <- list()
  if (!is.null(model)) {
    chat_args$model <- model
  }
  if (!is.null(config$base_url)) {
    chat_args$base_url <- config$base_url
  }
  if (!is.null(config$params) && length(config$params) > 0) {
    chat_args$params <- do.call(ellmer::params, config$params)
  }
  if (!is.null(config$extra_args) && length(config$extra_args) > 0) {
    chat_args$extra_args <- config$extra_args
  }
  if (!is.null(config$extra_headers) && length(config$extra_headers) > 0) {
    chat_args$extra_headers <- config$extra_headers
  }

  # Try to call ellmer::chat_{suffix}()
  chat_fn_name <- paste0("chat_", provider_suffix)
  chat_fn <- tryCatch(
    get(chat_fn_name, envir = asNamespace("ellmer")),
    error = function(e) NULL
  )

  if (is.null(chat_fn)) {
    stop(sprintf(
      "Cannot find ellmer provider function '%s'. Check provider: '%s'",
      chat_fn_name, provider_suffix
    ))
  }

  chat <- do.call(chat_fn, chat_args)
  as_agent(chat, id = id, description = description)
}


#' @noRd
reconstruct_script_agent <- function(config, base_dir, id, description) {
  source_path <- config$source
  fun_name <- config[["function"]]

  if (is.null(source_path) || is.null(fun_name)) {
    stop(sprintf(
      "Script agent '%s' requires 'source' and 'function' fields", id
    ))
  }

  # Resolve relative to YAML file directory
  full_path <- normalizePath(
    file.path(base_dir, source_path),
    mustWork = FALSE
  )
  if (!file.exists(full_path)) {
    stop(sprintf(
      "Script agent '%s': source file not found: %s", id, full_path
    ))
  }

  env <- new.env(parent = globalenv())
  source(full_path, local = env)

  fn <- env[[fun_name]]
  if (!is.function(fn)) {
    stop(sprintf(
      "Script agent '%s': '%s' is not a function in %s",
      id, fun_name, source_path
    ))
  }

  as_agent(fn, id = id, description = description)
}


#' @noRd
reconstruct_tool_agent <- function(config, id, description) {
  tool_path <- config$tool
  if (is.null(tool_path)) {
    stop(sprintf(
      "Tool definition agent '%s' requires a 'tool' field", id
    ))
  }
  as_agent(tool_path, id = id, description = description)
}


#' @noRd
reconstruct_package_agent <- function(config, id, description) {
  pkg_fun <- config$package_function
  if (is.null(pkg_fun)) {
    stop(sprintf(
      "Package function agent '%s' requires a 'package_function' field", id
    ))
  }
  as_agent(pkg_fun, id = id, description = description)
}


# ===================================================================
# Public API: workflow_save / workflow_load / workflow_list
# ===================================================================

#' @name workflow-io
#' @title Save and Load Workflow Definitions
#'
#' @description
#' Save and load complete workflow definitions (manifest + agent
#' configurations) to and from \verb{YAML} files. This enables
#' version-controlled, shareable workflow definitions that can
#' reconstruct full \code{\link{Scheduler}} pipelines.
#'
#' The \verb{YAML} file format supports multiple workflows sharing
#' a common agent pool:
#'
#' \itemize{
#'   \item \code{workflows}: A list of workflow definitions, each with
#'     a \code{master} policy and \code{stages} (named list of
#'     \code{\link{StatePolicy}} arrays)
#'   \item \code{agents}: A list of agent configurations, each with
#'     an \code{id}, \code{type}, and type-specific fields
#' }
#'
#' @section Agent Types:
#' Each agent entry in the \verb{YAML} has a \code{type} field:
#'
#' \describe{
#'   \item{\code{"chat"}}{An \pkg{ellmer} \code{Chat} object.
#'     Fields: \code{provider} (e.g., \code{"ollama"}, \code{"anthropic"}),
#'     \code{model}, and optionally \code{base_url}, \code{params},
#'     \code{extra_args}, \code{extra_headers}. Credentials are never
#'     serialized; they come from environment variables.}
#'   \item{\code{"script"}}{A function sourced from an \R file.
#'     Fields: \code{source} (path relative to the \verb{YAML} file),
#'     \code{function} (the function name to extract).}
#'   \item{\code{"tool_definition"}}{A \pkg{tricobbler} \verb{MCP} tool
#'     \verb{YAML} definition.
#'     Fields: \code{tool} (path or name of the tool).}
#'   \item{\code{"package_function"}}{A function from an installed package.
#'     Fields: \code{package_function} (e.g., \code{"utils::read.csv"}).}
#' }
#'
#' @section YAML Structure:
#' \preformatted{
#' workflows:
#'   - name: "my-workflow"
#'     version: "1.0.0"
#'     stages:
#'       triage:
#'         - name: extract_paths
#'           agent_id: triage_chat
#'           priority: 200
#'       executing:
#'         - name: create_skill
#'           agent_id: executor_chat
#'     parameters: \{ timeout: 300 \}
#'
#' agents:
#'   - id: triage_chat
#'     type: chat
#'     provider: ollama
#'     model: "qwen3-coder:30b"
#'   - id: executor
#'     type: script
#'     source: "scripts/tricobbler-agents/executor.R"
#'     function: "run_executor"
#' }
#'
#' @seealso \code{\link{Scheduler}}, \code{\link{Manifest}},
#'   \code{\link{manifest_read}}, \code{\link{manifest_write}}
NULL


#' @rdname workflow-io
#' @param file character, path to the workflow \verb{YAML} file
#' @param manifest a \code{\link{Manifest}} object (or list of
#'   \code{Manifest} objects for multiple workflows)
#' @param agents a list of agent configuration lists. Each must have
#'   \code{id} and \code{type} fields. \pkg{ellmer} \code{Chat} objects
#'   are auto-converted via \code{extract_chat_config()}. Alternatively,
#'   a list of \code{\link{Agent}} objects whose \code{config} property
#'   will be used.
#' @param append logical, if \code{TRUE} (default) merges into an existing
#'   file: workflows are matched by \code{name} (replace if same
#'   name), agents are matched by \code{id} (replace if same \code{id}).
#'   If \code{FALSE}, overwrites the file entirely.
#' @param agent_folder character, subdirectory relative to \code{file}
#'   where \code{script} agent source files are copied and stored.
#'   Defaults to \code{"scripts/tricobbler-agents"}. Must not contain
#'   \code{..} path components (fails with an error if it does). Pass
#'   \code{NULL} to skip copying and keep \code{source} paths as-is.
#' @returns \code{workflow_save()}: invisibly returns the file path
#' @examples
#' # Create a simple manifest
#' mp <- MasterPolicy(
#'   name = "demo", version = "1.0.0",
#'   stages = c("idle", "working"),
#'   parameters = list(timeout = 300)
#' )
#' sp1 <- StatePolicy(
#'   name = "init", stage = "idle",
#'   description = "Initialization", agent_id = "my_agent"
#' )
#' sp2 <- StatePolicy(
#'   name = "process", stage = "working",
#'   description = "Processing", agent_id = "my_worker"
#' )
#' manifest <- Manifest(master = mp, states = list(sp1, sp2))
#'
#' # Define agents as config lists
#' agents <- list(
#'   list(id = "my_agent", type = "package_function",
#'        package_function = "base::identity"),
#'   list(id = "my_worker", type = "package_function",
#'        package_function = "base::identity")
#' )
#'
#' # Save to temporary file
#' tmp <- tempfile(fileext = ".yaml")
#' workflow_save(tmp, manifest = manifest, agents = agents)
#'
#' # List available workflows
#' workflow_list(tmp)
#'
#' # Load back
#' wf <- workflow_load(tmp, name = "demo")
#' wf$manifest@name
#'
#' # Clean up
#' unlink(tmp)
#'
#' @export
workflow_save <- function(file, manifest = NULL, agents = NULL,
                          append = TRUE,
                          agent_folder = "scripts/tricobbler-agents") {

  base_dir <- dirname(normalizePath(file, mustWork = FALSE))

  # Normalize agents: convert Chat objects and Agent objects to config lists
  agents <- lapply(agents, function(a) {
    if (inherits(a, "Chat")) {
      return(extract_chat_config(a))
    }
    if (S7::S7_inherits(a, Agent)) {
      cfg <- a@config
      if (is.null(cfg)) {
        stop(sprintf(
          "Agent '%s' has no config. Set agent@config before saving.",
          a@id
        ))
      }
      return(cfg)
    }
    # Must be a raw config list
    if (!is.list(a) || is.null(a$id) || is.null(a$type)) {
      stop("Each agent must be a list with 'id' and 'type' fields")
    }
    a
  })

  # For script agents, copy source files into agent_folder and
  # rewrite source paths to be relative to the YAML file
  if (!is.null(agent_folder) && nzchar(agent_folder)) {
    # agent_folder is always relative to the YAML directory; ".." is forbidden
    parts <- strsplit(agent_folder, "[/\\\\]")[[1]]
    if (any(parts == "..")) {
      stop("'agent_folder' must not contain '..' components: ", agent_folder)
    }
    agents <- lapply(agents, function(a) {
      if (!identical(a$type, "script") || is.null(a$source)) {
        return(a)
      }
      # source is always relative to base_dir
      src_abs <- file.path(base_dir, a$source)
      dest_dir <- file.path(base_dir, agent_folder)
      dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
      dest_file <- file.path(dest_dir, basename(src_abs))
      # Only copy when the source is not already the destination
      if (normalizePath(src_abs, mustWork = FALSE) !=
          normalizePath(dest_file, mustWork = FALSE)) {
        if (!file.exists(src_abs)) {
          stop(sprintf(
            "Script agent '%s': source file not found: %s", a$id, src_abs
          ))
        }
        file.copy(src_abs, dest_file, overwrite = TRUE)
      }
      a$source <- file.path(agent_folder, basename(src_abs))
      a
    })
  }

  # Normalize manifest to list of manifests
  if (S7::S7_inherits(manifest, Manifest)) {
    manifests <- list(manifest)
  } else if (is.list(manifest) && length(manifest) > 0) {
    manifests <- manifest
  } else if (is.null(manifest)) {
    manifests <- list()
  } else {
    stop("'manifest' must be a Manifest object or list of Manifest objects")
  }

  # Convert manifests to workflow list structures
  new_workflows <- lapply(manifests, manifest_to_workflow_list)

  # If appending, read existing and merge
  if (isTRUE(append) && file.exists(file)) {
    existing <- yaml::read_yaml(file, readLines.warn = FALSE)
    existing_workflows <- existing$workflows %||% list()
    existing_agents <- existing$agents %||% list()

    # Merge workflows by name
    existing_names <- vapply(
      existing_workflows,
      function(w) w$name %||% "",
      character(1)
    )
    for (wf in new_workflows) {
      wf_name <- wf$name
      idx <- match(wf_name, existing_names)
      if (!is.na(idx)) {
        existing_workflows[[idx]] <- wf
      } else {
        existing_workflows <- c(existing_workflows, list(wf))
        existing_names <- c(existing_names, wf_name)
      }
    }

    # Merge agents by id
    existing_ids <- vapply(
      existing_agents,
      function(a) a$id %||% "",
      character(1)
    )
    for (a in agents) {
      idx <- match(a$id, existing_ids)
      if (!is.na(idx)) {
        existing_agents[[idx]] <- a
      } else {
        existing_agents <- c(existing_agents, list(a))
        existing_ids <- c(existing_ids, a$id)
      }
    }

    new_workflows <- existing_workflows
    agents <- existing_agents
  }

  output <- list(
    workflows = new_workflows,
    agents = agents
  )

  txt <- yaml::as.yaml(output)
  writeLines(txt, con = file)
  invisible(file)
}


#' @rdname workflow-io
#' @param name character or \code{NULL}. If \code{NULL}, returns a
#'   character vector of available workflow names. If a workflow name,
#'   loads that specific workflow.
#' @param scheduler_class an \R6 generator class with a \code{new(manifest, agents)}
#'   constructor used to instantiate the workflow. Defaults to
#'   \code{\link{Scheduler}}. Pass \code{NULL} to
#'   return a plain \code{list(manifest, agents)} instead.
#' @returns \code{workflow_load()}:
#'   \itemize{
#'     \item If \code{name = NULL}: character vector of available
#'       workflow names
#'     \item If \code{name} is given and \code{scheduler_class} is non-\code{NULL}:
#'       a \code{\link{Scheduler}} instance (or instance of the provided
#'       generator class) — typically of class \code{"TricobblerScheduler"}
#'     \item If \code{name} is given and \code{scheduler_class = NULL}:
#'       a list with \code{manifest} (\code{\link{Manifest}}) and
#'       \code{agents} (list of \code{\link{Agent}} objects)
#'   }
#' @export
workflow_load <- function(file, name = NULL, scheduler_class = Scheduler) {
  if (!file.exists(file)) {
    stop(sprintf("Workflow file not found: %s", file))
  }
  li <- yaml::read_yaml(file, readLines.warn = FALSE)
  workflows <- li$workflows %||% list()

  # Extract workflow names
  wf_names <- vapply(
    workflows,
    function(w) w$name %||% NA_character_,
    character(1)
  )

  # List mode
  if (is.null(name) || (length(name) == 1 && is.na(name))) {
    message("No workflow chosen. Please choose from the following workflows:\n", paste("  -", wf_names, collapse = "\n"))
    return(invisible(wf_names))
  }

  # Find the requested workflow
  idx <- match(name, wf_names)
  if (is.na(idx)) {
    stop(sprintf(
      "Workflow '%s' not found. Available: %s",
      name, paste(sQuote(wf_names), collapse = ", ")
    ))
  }

  wf_list <- workflows[[idx]]
  manifest <- workflow_list_to_manifest(wf_list)

  # Reconstruct agents
  agent_configs <- li$agents %||% list()
  base_dir <- dirname(normalizePath(file, mustWork = FALSE))

  # Only reconstruct agents referenced by this workflow
  needed_ids <- unique(vapply(
    manifest@states,
    function(s) s@agent_id,
    character(1)
  ))

  agents_out <- list()
  for (acfg in agent_configs) {
    if (acfg$id %in% needed_ids) {
      agent <- reconstruct_agent(acfg, base_dir = base_dir)
      agents_out <- c(agents_out, list(agent))
    }
  }

  if (!is.null(scheduler_class)) {
    return(scheduler_class$new(manifest = manifest, agents = agents_out))
  }
  list(manifest = manifest, agents = agents_out)
}


#' @rdname workflow-io
#' @returns \code{workflow_list()}: character vector of workflow names
#'   in the file
#' @export
workflow_list <- function(file) {
  workflow_load(file, name = NULL)
}
