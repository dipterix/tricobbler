# ---------------------------------------------------------------------------
# Skill R6 Class
# ---------------------------------------------------------------------------

#' Parse a \code{SKILL.md} file into frontmatter and body
#'
#' @description Reads a \code{SKILL.md} file and splits it into YAML
#'   frontmatter and markdown body. The frontmatter is delimited by
#'   \code{---} lines at the top of the file.
#'
#' @param path character, path to the \code{SKILL.md} file
#' @returns A list with components:
#' \describe{
#'   \item{\code{name}}{character, the skill name from frontmatter
#'     (or \code{NULL} if absent)}
#'   \item{\code{description}}{character, the skill description from
#'     frontmatter (or \code{NULL} if absent)}
#'   \item{\code{frontmatter}}{list, all parsed YAML frontmatter fields}
#'   \item{\code{body}}{character scalar, the markdown body after the
#'     frontmatter}
#' }
#' @noRd
parse_skill_md <- function(path) {
  if (!file.exists(path)) {
    stop("SKILL.md not found at: ", path)
  }
  lines <- readLines(path, warn = FALSE)
  n <- length(lines)

  # Find frontmatter delimiters (--- lines)
  fm_start <- NA_integer_
  fm_end <- NA_integer_

  for (i in seq_len(n)) {
    line <- trimws(lines[[i]])
    if (nzchar(line)) {
      if (line == "---") {
        fm_start <- i
        break
      } else {
        # First non-empty line is not ---, no frontmatter
        break
      }
    }
  }

  if (!is.na(fm_start)) {
    for (j in seq(fm_start + 1L, n)) {
      if (trimws(lines[[j]]) == "---") {
        fm_end <- j
        break
      }
    }
  }

  frontmatter <- list()
  body_start <- 1L

  if (!is.na(fm_start) && !is.na(fm_end) && fm_end > fm_start + 1L) {
    fm_text <- paste(lines[seq(fm_start + 1L, fm_end - 1L)], collapse = "\n")
    frontmatter <- tryCatch(
      yaml::yaml.load(fm_text),
      error = function(e) list()
    )
    body_start <- fm_end + 1L
  }

  body <- ""
  if (body_start <= n) {
    body <- paste(lines[seq(body_start, n)], collapse = "\n")
    body <- trimws(body)
  }

  list(
    name = frontmatter$name,
    description = frontmatter$description,
    frontmatter = frontmatter,
    body = body
  )
}

sanitize_string_arg <- function(x) {
  if (length(x) == 1 && is.character(x) && startsWith(x, '["')) {
    x <- jsonlite::fromJSON(x)
  }
  x
}

convert_from_type <- function(x, type) {
  if (length(x) == 1 && is.character(x) && !is.na(x)) {
    x <- trimws(x)
    if (grepl("^<[a-zA-Z0-9].*>", x)) {
      stop("XML input detected. Please do not do that. Use pure JSON!")
    }
  }
  ellmer:::convert_from_type(x = x, type = type)
}

#' Discover scripts under a skill \code{scripts/} directory
#'
#' @description Recursively scans the \code{scripts/} subdirectory of a skill
#'   and returns metadata for each script file. All scripts are treated as
#'   command-line executables dispatched via the appropriate interpreter.
#'
#' @param scripts_dir character, path to the \code{scripts/} directory
#' @returns A named list keyed by script name (relative path with extension).
#'   Each element is a list with:
#'   \describe{
#'     \item{\code{name}}{character, the script name (relative path with
#'       extension)}
#'     \item{\code{path}}{character, absolute file path}
#'     \item{\code{ext}}{character, lowercase file extension}
#'     \item{\code{type}}{character, script type (\code{"rscript"},
#'       \code{"python"}, \code{"shell"}, \code{"batch"}, or
#'       \code{"executable"})}
#'   }
#' @noRd
discover_scripts <- function(scripts_dir) {
  if (!dir.exists(scripts_dir)) {
    return(list())
  }

  files <- list.files(
    scripts_dir,
    recursive = TRUE,
    full.names = FALSE,
    include.dirs = FALSE
  )

  if (length(files) == 0L) {
    return(list())
  }

  scripts <- list()

  for (rel_path in files) {
    ext <- tolower(tools::file_ext(rel_path))
    full_path <- normalizePath(
      file.path(scripts_dir, rel_path),
      mustWork = TRUE
    )

    # Script name: full relative path with extension (CLI-only)
    script_name <- rel_path

    script_type <- switch(
      ext,
      "r" = "rscript",
      "py" = "python",
      "sh" = "shell", "bash" = "shell",
      "bat" = "batch", "cmd" = "batch",
      "executable"
    )

    scripts[[script_name]] <- list(
      name = script_name,
      path = full_path,
      ext = ext,
      type = script_type
    )
  }

  scripts
}


#' Extract roxygen2 documentation from an R script file
#'
#' @description Reads the roxygen2 comment block (lines starting with
#'   \code{#'}) immediately preceding a function definition in a script
#'   file. Returns the raw comment text with the \code{#'} prefix stripped.
#'
#' @param path character, absolute path to the R script file
#' @param fn_name character, the function name to find documentation for
#' @returns character scalar, the extracted documentation text
#'   (or a fallback message if no roxygen2 comments are found)
#' @noRd
extract_roxygen_help <- function(path, fn_name) {
  if (!file.exists(path)) {
    return(sprintf("Script file not found: %s", path))
  }
  lines <- readLines(path, warn = FALSE)
  n <- length(lines)

  # Find the line defining the function: fn_name <- function(...)
  fn_pattern <- sprintf(
    "^\\s*%s\\s*(<-|=)\\s*function\\s*\\(",
    gsub("([.\\\\|()\\[{}^$*+?])", "\\\\\\1", fn_name)
  )
  fn_line <- grep(fn_pattern, lines)
  if (length(fn_line) == 0L) {
    return(sprintf(
      "No roxygen2 documentation found for '%s' in %s.",
      fn_name, basename(path)
    ))
  }
  fn_line <- fn_line[[1L]]

  # Walk backwards from the line before the function def to collect #' lines
  doc_lines <- character(0L)
  i <- fn_line - 1L
  while (i >= 1L && grepl("^\\s*#'", lines[[i]])) {
    doc_lines <- c(lines[[i]], doc_lines)
    i <- i - 1L
  }

  if (length(doc_lines) == 0L) {
    return(sprintf(
      "No roxygen2 documentation found for '%s' in %s.",
      fn_name, basename(path)
    ))
  }

  # Strip the #' prefix
  doc_text <- sub("^\\s*#'\\s?", "", doc_lines)
  paste(doc_text, collapse = "\n")
}


#' Build the interpreter command for a script based on file extension
#'
#' @description Determines the appropriate interpreter and arguments for
#'   running a script as a command-line process. R scripts use
#'   \code{Rscript}, Python scripts use \code{python3} (or \code{python}
#'   on Windows), and shell scripts use \code{bash} (or \code{cmd /c}
#'   on Windows). Other files are run directly as executables.
#'
#' @param script_path character, absolute path to the script file
#' @param ext character, lowercase file extension (e.g., \code{"r"},
#'   \code{"py"}, \code{"sh"})
#' @returns A list with components:
#'   \describe{
#'     \item{\code{command}}{character, the interpreter or executable path}
#'     \item{\code{prefix_args}}{character vector, arguments to place
#'       before the user-supplied flags (typically the script path)}
#'   }
#' @noRd
build_script_command <- function(script_path, ext) {
  os <- get_os()
  switch(
    ext,
    "r" = list(
      command = file.path(R.home("bin"), "Rscript"),
      prefix_args = script_path
    ),
    "py" = list(
      command = if (os == "windows") "python" else "python3",
      prefix_args = script_path
    ),
    "sh" =, "bash" = {
      if (os == "windows") {
        list(command = "cmd", prefix_args = c("/c", script_path))
      } else {
        list(command = "bash", prefix_args = script_path)
      }
    },
    "bat" =, "cmd" = {
      if (os == "windows") {
        list(command = "cmd", prefix_args = c("/c", script_path))
      } else {
        warning(
          "Windows batch files are not supported on ", os,
          call. = FALSE
        )
        list(command = script_path, prefix_args = character(0L))
      }
    },
    # Default: run directly (assumes executable with shebang or binary)
    list(command = script_path, prefix_args = character(0L))
  )
}


#' Discover reference files in a skill directory
#'
#' @description Scans a skill directory for reference files using the
#'   conventions found in the Claude skill ecosystem. This includes:
#'   \enumerate{
#'     \item Top-level files (excluding \code{SKILL.md} and hidden files)
#'     \item Files inside directories whose name starts with
#'       \code{"reference"} (case-insensitive), e.g. \code{reference/},
#'       \code{references/}, \code{Reference/}
#'   }
#'
#'   Directories named \code{scripts} or \code{assets} are always
#'   excluded, as they serve distinct roles in the skill spec.
#'
#' @param skill_path character, absolute path to the skill directory
#' @returns A sorted character vector of relative paths suitable for
#'   use as the \code{file_name} parameter in the references action
#'   (e.g. \code{"FORMS.md"}, \code{"references/finance.md"}).
#'   Returns \code{character(0)} when no reference files are found.
#' @noRd
discover_references <- function(skill_path) {
  refs <- character(0L)

  # --- Top-level files (excluding SKILL.md, hidden files, directories) ---
  all_files <- list.files(
    skill_path, recursive = FALSE, include.dirs = FALSE,
    all.files = FALSE  # excludes hidden files like .DS_Store
  )
  all_dirs <- list.dirs(
    skill_path, full.names = FALSE, recursive = FALSE
  )
  top_files <- all_files[
    !all_files %in% c(all_dirs, "SKILL.md") &
      !startsWith(all_files, ".")
  ]
  refs <- c(refs, top_files)

  # --- Files inside reference* directories (case-insensitive) ---
  # Exclude reserved directories scripts/ and assets/
  ref_dirs <- all_dirs[
    grepl("^reference", tolower(all_dirs)) &
      !tolower(all_dirs) %in% c("scripts", "assets")
  ]
  for (dir_name in ref_dirs) {
    dir_path <- file.path(skill_path, dir_name)
    nested <- list.files(
      dir_path, recursive = TRUE, include.dirs = FALSE,
      all.files = FALSE
    )
    # Filter hidden files in nested results
    nested <- nested[!grepl("(^|/)\\.", nested)]
    if (length(nested) > 0L) {
      refs <- c(refs, file.path(dir_name, nested))
    }
  }

  sort(refs)
}


#' Sanitize a string into a valid claude skill name
#'
#' @description Converts a string to a valid skill name: lowercase letters
#'   and dashes only, must start with a letter.
#' @param x character, input string
#' @returns character, sanitized name
#' @noRd
sanitize_skill_name <- function(x) {
  x <- tolower(x)
  # Replace non-alpha-dash with dash
  x <- gsub("[^a-z-]", "-", x)
  # Collapse consecutive dashes
  x <- gsub("-{2,}", "-", x)
  # Strip leading/trailing dashes
  x <- gsub("^-+|-+$", "", x)
  # Must start with a letter
  if (!grepl("^[a-z]", x) || !nzchar(x)) {
    stop(
      "Cannot derive a valid skill name from '", x, "'. ",
      "Skill names must start with a lowercase letter and contain ",
      "only lowercase letters and dashes."
    )
  }
  x
}


#' Skill: Load and Serve Claude Skill Directories
#'
#' @description R6 class that loads a claude skill directory (containing
#'   \code{SKILL.md}, optional \code{scripts/}, and reference files) and
#'   exposes it as a single \code{\link[ellmer]{tool}} definition via the
#'   \code{$make_tools()} method.
#'
#' @details
#' A skill directory must contain a \code{SKILL.md} file with optional YAML
#' frontmatter (\code{name}, \code{description}, \code{metadata}) and a
#' markdown body. The directory may also contain:
#' \itemize{
#'   \item Reference files: top-level files (other than \code{SKILL.md}) and
#'     files inside directories whose name starts with \code{"reference"}
#'     (case-insensitive), e.g. \code{reference/}, \code{references/},
#'     \code{Reference/}
#'   \item A \code{scripts/} subdirectory with callable R, shell, or python
#'     scripts (executed via CLI only)
#' }
#'
#' The \code{$make_tools()} method returns a single
#' \code{\link[ellmer]{tool}} named \verb{skill_\{name\}} that dispatches
#' on an \code{action} parameter:
#' \describe{
#'   \item{\code{"readme"}}{Returns the \code{SKILL.md} body. Must be
#'     called first to unlock other actions.}
#'   \item{\code{"reference"}}{Reads reference files with optional line
#'     range and grep filtering (only available when reference files exist)}
#'   \item{\code{"script"}}{Executes scripts via CLI with
#'     \code{processx::run()} (only available when scripts exist)}
#' }
#'
#' @examples
#' skill_dir <- system.file("skills", "weather", package = "tricobbler")
#' if (nzchar(skill_dir) && dir.exists(skill_dir)) {
#'   skill <- Skill$new(skill_dir)
#'   skill$name
#'   skill$description
#'
#'   # Produce ellmer tool definitions
#'   tools <- skill$make_tools()
#'   names(tools)
#' }
#'
#' @export
Skill <- R6::R6Class(
  classname = "TricobblerSkill",
  portable = TRUE,
  cloneable = FALSE,
  private = list(
    .name = character(0L),
    .path = character(0L)
  ),
  active = list(
    #' @field name character, skill name (lowercase letters and dashes only,
    #'   read-only)
    name = function(value) {
      if (!missing(value)) {
        stop("Field `name` is read-only")
      }
      private$.name
    },

    #' @field path character, absolute path to the skill directory (read-only)
    path = function(value) {
      if (!missing(value)) {
        stop("Field `path` is read-only")
      }
      private$.path
    }
  ),
  public = list(

    #' @field description character, skill description from
    #'   \code{SKILL.md} frontmatter
    description = NULL,

    #' @field body character, the markdown body of \code{SKILL.md}
    body = NULL,

    #' @field scripts named list, script metadata discovered from the
    #'   \code{scripts/} subdirectory (see \code{Details})
    scripts = NULL,

    #' @field file_choices character, relative paths of reference files
    #'   available for the references action (top-level files excluding
    #'   \code{SKILL.md}, plus files in \code{reference*/} directories)
    file_choices = NULL,

    #' @description Initialize a new \code{Skill} from a skill directory
    #' @param path character, path to the skill directory (must contain
    #'   \code{SKILL.md})
    initialize = function(path) {
      path <- normalizePath(path, mustWork = FALSE)
      if (!dir.exists(path)) {
        stop("Skill directory does not exist: ", path)
      }
      skill_md <- file.path(path, "SKILL.md")
      if (!file.exists(skill_md)) {
        stop("SKILL.md not found in: ", path)
      }
      private$.path <- path

      self$parse()
    },

    #' @description Parse the \code{SKILL.md} file and update fields
    parse = function() {
      skill_md <- file.path(private$.path, "SKILL.md")
      # Parse SKILL.md
      parsed <- parse_skill_md(skill_md)

      # Set name: from frontmatter, or fallback to directory basename
      raw_name <- parsed$name
      if (is.null(raw_name) || !nzchar(raw_name)) {
        raw_name <- basename(private$.path)
      }
      private$.name <- sanitize_skill_name(raw_name)

      self$description <- parsed$description %||% ""
      self$body <- parsed$body

      # Discover reference files (top-level + reference*/ directories)
      path <- private$.path
      self$file_choices <- discover_references(path)

      # Discover scripts
      scripts_dir <- file.path(path, "scripts")
      self$scripts <- discover_scripts(scripts_dir)

      invisible(self)
    },

    #' @description Produce a single \code{\link[ellmer]{tool}} definition
    #'   for this skill
    #' @returns A named list with one \code{\link[ellmer]{tool}} object.
    #'   The name is \verb{skill_\{name\}}.
    make_tools = function() {

      skill_name <- private$.name
      skill_body <- self$body
      skill_path <- private$.path
      skill_file_choices <- self$file_choices
      skill_scripts <- self$scripts
      skill_description <- self$description

      has_references <- length(skill_file_choices) > 0L
      has_scripts <- length(skill_scripts) > 0L

      # ------ Build action enum values ------
      action_values <- "readme"
      if (has_references) {
        action_values <- c(action_values, "reference")
      }
      if (has_scripts) {
        action_values <- c(action_values, "script")
      }

      # ------ Build description ------
      script_names <- names(skill_scripts)

      action_docs <- c(
        "- \"readme\": Returns the SKILL.md body. Call this first."
      )
      if (has_references) {
        action_docs <- c(
          action_docs,
          "- \"reference\": Read reference files. Use `reference_kwargs` to specify the reference `file_name`, lines to read, or filter with regular expressions. Available files: ",
          sprintf("  - %s", skill_file_choices)
        )
      }
      if (has_scripts) {
        script_docs <- vapply(script_names, function(sn) {
          s <- skill_scripts[[sn]]
          # Try to get usage help for the script
          help_info <- ""
          if (s$ext == "r") {
            # For R scripts, try to extract first line of docopt doc
            tryCatch({
              script_lines <- readLines(s$path, warn = FALSE)
              # Look for docopt pattern start
              doc_start <- grep('^\\s*"', script_lines)[1]
              if (!is.na(doc_start)) {
                # Extract first non-empty line after the quote
                doc_lines <- script_lines[doc_start:min(doc_start + 10, length(script_lines))]
                # Find the description (first non-empty line after opening quote)
                desc_line <- trimws(gsub('^\\s*"', '', doc_lines[1]))
                if (nzchar(desc_line)) {
                  help_info <- sprintf(": %s", desc_line)
                }
              }
            }, error = function(e) NULL)
          }
          sprintf("    - \"%s\" [%s]%s", sn, s$type, help_info)
        }, character(1L))
        action_docs <- c(
          action_docs,
          "- \"script\": Execute a script via CLI.",
          "  - Use `cli_kwargs` with `file_name` to specify which script, ",
          "  - Use args=\"--help\" to see usage for any script.",
          "  - args=[\"--key1\", \"value1\", ...] for script arguments. ",
          "  - Available scripts:",
          script_docs
        )
      }
      action_docs <- paste(action_docs, collapse = "\n")

      tool_desc <- sprintf(
        "Skill tool `skill_%s`: %s.",
        skill_name,
        skill_description
      )

      # ------ Build arguments ------
      tool_arguments <- list(
        action = ellmer::type_enum(
          values = action_values,
          description = action_docs
        )
      )

      if (has_references) {
        tool_arguments$reference_kwargs <- ellmer::type_object(
          .description = paste0(
            "Arguments for action=\"reference\"; ",
            "Ignored for other actions."
          ),
          .required = FALSE,
          file_name = ellmer::type_enum(
            values = skill_file_choices,
            required = TRUE,
            description = "Reference file to read."
          ),
          pattern = ellmer::type_string(
            required = FALSE,
            description = paste0(
              "Optional perl-compatible regular expression to filter ",
              "lines. Empty string means no filtering."
            )
          ),
          line_start = ellmer::type_integer(
            required = FALSE,
            description = "Start line number (1-based). Default: 1"
          ),
          n_rows = ellmer::type_integer(
            required = FALSE,
            description = paste0(
              "Number of lines to read from line_start. Default: 50"
            )
          )
        )
      }

      if (has_scripts) {
        tool_arguments$cli_kwargs <- ellmer::type_object(
          .description = paste0(
            "Arguments for action=\"script\"; ",
            "Ignored for other actions."
          ),
          .required = FALSE,
          file_name = ellmer::type_enum(
            values = script_names,
            required = TRUE,
            description = "Script file name to execute."
          ),
          args = ellmer::type_array(
            ellmer::type_string(),
            required = FALSE,
            description = "CLI arguments (array of strings) passed to the script."
          ),
          envs = ellmer::type_array(
            ellmer::type_string(),
            required = FALSE,
            description = paste0(
              "Environment variables as KEY=VALUE strings."
            )
          )
        )
      }

      # ------ Closure state ------
      readme_unlocked <- FALSE

      # ------ Function when action=readme -------
      tool_fn_readme <- function() {
        readme_unlocked <<- TRUE
        paste(skill_body, collapse = "\n")
      }

      # ------ Function when action=reference -------
      tool_fn_reference <- function(file_name = NULL, pattern = "",
                                    line_start = 1L, n_rows = 50L, ...) {
        if (!has_references) {
          stop("This skill has no reference files.")
        }
        if (!isTRUE(file_name %in% skill_file_choices)) {
          stop(sprintf(
            "Invalid reference file '%s'. Available reference files: \n%s",
            paste(file_name, collapse = ""),
            paste("  -", skill_file_choices, collapse = "\n")
          ))
        }
        if (length(pattern) != 1 || !is.character(pattern) ||
            is.na(pattern) || !nzchar(pattern)) {
          pattern <- ""
        }

        line_start <- as.integer(line_start)
        if (!isTRUE(line_start > 0)) {
          line_start <- 1L
        }

        n_rows <- as.integer(n_rows)
        if (!isTRUE(n_rows > 0)) {
          n_rows <- -1L
        }

        fpath <- file.path(skill_path, file_name)
        if (!file.exists(fpath)) {
          stop("File not found: ", file_name)
        }

        lines <- readLines(fpath, warn = FALSE)
        n <- length(lines)
        if (n == 0L) {
          return("(empty file)")
        }
        # Apply line range
        line_start <- max(1L, as.integer(line_start))
        if (line_start > n) {
          return("(reaching EOF)")
        }
        if (n_rows < 0L) {
          n_rows <- n
        }
        line_end <- min(line_start + n_rows - 1L, n)

        line_index <- seq(line_start, line_end)
        lines <- lines[line_index]

        # Apply grep pattern
        if (length(pattern) == 1L && !is.na(pattern) &&
            nzchar(pattern)) {
          sel <- grepl(pattern, x = lines, perl = TRUE)
          line_index <- line_index[sel]
          lines <- lines[sel]
        }

        paste0("L", line_index, ": ", lines, collapse = "\n")
      }

      # ------ Function when action=script ------
      tool_fn_script <- function(file_name = NULL, args = c(),
                                 envs = list(), ...) {
        if (!has_scripts) {
          stop("This skill has no scripts.")
        }

        if (!isTRUE(file_name %in% script_names)) {
          stop(sprintf(
            "Invalid script file '%s'. Available files: \n%s",
            paste(file_name, collapse = ""),
            paste("  -", script_names, collapse = "\n")
          ))
        }
        script_file <- file_name
        cli_args <- as.character(args)
        cli_envs <- as.list(envs)
        cli_envs <- cli_envs[!names(cli_envs) %in% ""]

        if (length(cli_envs)) {
          cli_envs <- structure(
            names = c("", names(cli_envs)),
            c("current", as.vector(cli_envs, mode = "character"))
          )
        } else {
          cli_envs <- NULL
        }

        if (length(cli_args) == 0) {
          cli_args <- "--help"
        }

        # Resolve script (exact match by relative path with extension)
        script <- skill_scripts[[script_file]]
        if (is.null(script)) {
          stop(
            "Script '", script_file, "' not found. ",
            "Available scripts: ",
            paste(script_names, collapse = ", ")
          )
        }

        # Run script with --help flag (works for all CLI scripts)
        cmd_info <- build_script_command(
          script$path, script$ext
        )

        if (isTRUE(cli_args %in% c("--help", "-h"))) {
          result <- processx::run(
            command = cmd_info$command,
            args = c(cmd_info$prefix_args, "--help"),
            env = cli_envs,
            wd = self$path, cleanup_tree = TRUE,
            echo_cmd = TRUE, spinner = TRUE,
            # stderr_to_stdout = TRUE,
            error_on_status = FALSE
          )
        } else {
          result <- processx::run(
            command = cmd_info$command,
            args = c(cmd_info$prefix_args, cli_args),
            env = cli_envs,
            wd = self$path, cleanup_tree = TRUE,
            echo_cmd = TRUE, spinner = TRUE,
            # stderr_to_stdout = TRUE,
            error_on_status = FALSE
          )
        }

        # Check status and return appropriate output
        if (result$status != 0L) {
          stop(
            "Script '", script_file,
            "' exited with status ", result$status,
            ".\nstderr: ", result$stderr
          )
        }

        # Return stdout (or stderr if stdout is empty, for --help)
        if (nzchar(result$stdout)) {
          return(result$stdout)
        } else if (nzchar(result$stderr)) {
          return(result$stderr)
        } else {
          return("(no output)")
        }
      }
      # ------ Unified tool function ------
      tool_fn <- function(
        action,
        reference_kwargs = NULL,
        cli_kwargs = NULL,
        ...
      ) {
        if (...length() > 0) {
          stop("You are calling the tool with unexpected object key(s): ",
               paste(sQuote(...names()), collapse = ", "), ". ",
               "Please ONLY include key `action`, `reference_kwargs`, and `cli_kwargs`.")
        }

        action <- sanitize_string_arg(action)
        reference_kwargs <- sanitize_string_arg(reference_kwargs)
        cli_kwargs <- sanitize_string_arg(cli_kwargs)
        message("Calling skill with action=", action)


        # ---- Guard: readme must be called first ----
        if (action != "readme" && !readme_unlocked) {
          stop(
            sprintf(
              paste0(
                "Call action=\"readme\" first to read the skill ",
                "guidelines before using action=\"%s\"."
              ),
              action
            )
          )
        }

        # ---- Dispatch ----
        switch(
          action,

          # ============================================================
          # Action: readme
          # ============================================================
          "readme" = {
            tool_fn_readme()
          },

          # ============================================================
          # Action: reference
          # ============================================================
          "reference" = {
            if (!has_references) {
              stop("This skill has no reference files.")
            }
            if (length(reference_kwargs$file_name) != 1) {
              stop("When action='reference', `reference_kwargs` must not be empty, and `file_name` must be specified.")
            }
            do.call(tool_fn_reference, reference_kwargs)
          },

          # ============================================================
          # Action: script
          # ============================================================
          "script" = {
            if (!has_scripts) {
              stop("This skill has no scripts.")
            }
            if (length(cli_kwargs$file_name) != 1) {
              stop("When action='script', `cli_kwargs` must not be empty, and `file_name` must be specified.")
            }
            do.call(tool_fn_script, cli_kwargs)
          },

          # ============================================================
          # Unknown action
          # ============================================================
          stop(
            "Unknown action: '", action, "'. ",
            "Available: ",
            paste(action_values, collapse = ", ")
          )
        )
      }

      # ------ Build tool ------
      tool_name <- sprintf("skill_%s", skill_name)

      tools <- list()

      desc <- lapply(names(tool_arguments), function(name) {
        arg_def <- tool_arguments[[name]]
        arg_desc <- unlist(strsplit(arg_def@description, "\n"))
        paste(c(name, sprintf("  %s", arg_desc)), collapse = "\n")
      })
      desc <- paste(unlist(desc), collapse = "\n\n")

      # tools[[tool_name]] <- ellmer::tool(
      #   fun = function(object = "{}", ..., action, reference_kwargs, cli_kwargs) {
      #     object <- sanitize_string_arg(object)
      #     cat("========\n", object, "\n========\n")
      #     object <- as.list(jsonlite::fromJSON(object))
      #     if (!missing(action)) {
      #       object$action <- action
      #     }
      #     if (!missing(reference_kwargs)) {
      #       object$reference_kwargs <- reference_kwargs
      #     }
      #     if (!missing(cli_kwargs)) {
      #       object$cli_kwargs <- cli_kwargs
      #     }
      #     do.call(tool_fn, object)
      #     # tool_fn()
      #   },
      #   description = tool_desc,
      #   # arguments = tool_arguments,
      #   arguments = list(
      #     "object" = ellmer::type_string(
      #       description = paste(c(
      #         "A JSON string containing the following arguments\n",
      #         desc
      #       ), collapse = "\n"),
      #       required = TRUE
      #     ),
      #     "..." = ellmer::type_object(
      #       .description = "Please do NOT use this argument: will throw errors if you specify",
      #       .required = FALSE
      #     ),
      #     "action" = local({
      #       action <- tool_arguments$action
      #       action@required <- FALSE
      #       action
      #     }),
      #     "reference_kwargs" = tool_arguments$reference_kwargs,
      #     "cli_kwargs" = tool_arguments$cli_kwargs
      #   ),
      #   name = tool_name,
      #   convert = TRUE
      # )

      tool_arguments[["..."]] <- ellmer::type_ignore()

      tools[[tool_name]] <- ellmer::tool(
        fun = function(action = NA_character_,
                       reference_kwargs = NULL,
                       cli_kwargs = NULL,
                       ...) {
          if (missing(action) || is.na(action)) {
            action <- "readme"
          } else {
            action <- convert_from_type(action, tool_arguments$action)
          }
          switch(
            action,
            'reference' = {
              reference_kwargs <- convert_from_type(
                x = reference_kwargs,
                type = tool_arguments$reference_kwargs
              )
            },
            'script' = {
              cli_kwargs <- convert_from_type(
                x = cli_kwargs,
                type = tool_arguments$cli_kwargs
              )
            }
          )
          tool_fn(action = action,
                  reference_kwargs = reference_kwargs,
                  cli_kwargs = cli_kwargs)
        },
        description = tool_desc,
        arguments = tool_arguments,
        name = tool_name,
        convert = FALSE
      )

      tools
    }
  )
)
