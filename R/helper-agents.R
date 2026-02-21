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


reconstruct_package_agent <- function(config, id, description) {
  pkg_fun <- config$package_function
  if (is.null(pkg_fun)) {
    stop(sprintf(
      "Package function agent '%s' requires a 'package_function' field", id
    ))
  }
  as_agent(x = paste(pkg_fun, collapse = " "),
           id = id,
           description = description)
}

reconstruct_tool_agent <- function(config, id, description) {
  tool_path <- config$tool
  if (is.null(tool_path)) {
    stop(sprintf(
      "Tool definition agent '%s' requires a 'tool' field", id
    ))
  }
  as_agent(x = paste(tool_path, collapse = " "),
           id = id,
           description = description)
}

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
  if (length(config$params) > 0) {
    chat_args$params <- do.call(ellmer::params, config$params)
  }
  if (length(config$extra_args) > 0) {
    chat_args$extra_args <- config$extra_args
  }
  if (length(config$extra_headers) > 0) {
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

reconstruct_script_agent <- function(config, base_dir, id, description) {

  base_dir <- normalizePath(base_dir, mustWork = FALSE)

  if (is.function(config$impl)) {
    fn <- config$impl
  } else if (is.raw(config$source)) {

    # function is serialized into raw
    fn <- unserialize(config$source)

    if (!is.function(fn)) {
      stop(sprintf(
        "Binary agent '%s' is not a function",
        id
      ))
    }

  } else {
    source_path <- config$source
    if (length(source_path) != 1) {
      stop(sprintf("Script agent '%s' requires 'source' fields", id))
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

    if (endsWith(tolower(source_path), ".rds")) {
      fn <- readRDS(full_path)

      if (!is.function(fn)) {
        stop(sprintf(
          "Script agent '%s': `%s` cannot be loaded as a function",
          id, source_path
        ))
      }
    } else {
      fun_name <- config$function_name
      if (length(fun_name) != 1) {
        stop(sprintf("Script agent '%s' requires 'function_name' fields", id))
      }

      env <- new.env(parent = baseenv())
      source(full_path, local = env)

      fn <- env[[fun_name]]

      if (!is.function(fn)) {
        stop(sprintf(
          "Script agent '%s': '%s' is not a function in %s",
          id, fun_name, source_path
        ))
      }
    }
  }

  agent <- as_agent(fn, id = id, description = description)

  agent@config$source <- config$source
  agent@config$function_name <- config$function_name
  agent
}

#' Reconstruct an \code{\link{Agent}} from a config list
#'
#' @param agent_config A named list with at least \code{id} and \code{type}
#' @param base_dir character, base directory for resolving relative paths
#'   (typically \code{dirname(workflow_file)})
#' @returns An \code{\link{Agent}} object
#' @noRd
reconstruct_agent <- function(agent_config, base_dir = ".", ...) {

  id <- agent_config$id
  agent_type <- paste(agent_config$type, collapse = " ")
  description <- agent_config$description %||% ""

  if (length(id) != 1 || is.na(id) || !nzchar(id)) {
    stop("Agent ID is invalid.")
  }

  agent <- switch(
    agent_type,
    "chat" = {
      reconstruct_chat_agent(config = agent_config,
                             id = id,
                             description = description)
    },
    "script" = {
      reconstruct_script_agent(
        config = agent_config,
        base_dir = base_dir,
        id = id,
        description = description
      )
    },
    "tool_definition" = {
      reconstruct_tool_agent(config = agent_config,
                             id = id,
                             description = description)
    },
    "package_function" = {
      reconstruct_package_agent(config = agent_config,
                                id = id,
                                description = description)
    },
    stop(sprintf("Unknown agent type: '%s'", agent_type))
  )

  agent_config$description <- description
  agent@config[names(agent_config)] <- agent_config
  agent
}
