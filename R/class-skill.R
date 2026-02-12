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


#' Discover scripts under a skill \code{scripts/} directory
#'
#' @description Recursively scans the \code{scripts/} subdirectory of a skill
#'   and returns metadata for each script file. R scripts (\code{.R}/\code{.r})
#'   are sourced to capture function objects. Other files are treated as
#'   executables.
#'
#' @param scripts_dir character, path to the \code{scripts/} directory
#' @returns A named list keyed by script name (relative path sans extension).
#'   Each element is a list with:
#'   \describe{
#'     \item{\code{name}}{character, the script name}
#'     \item{\code{path}}{character, absolute file path}
#'     \item{\code{type}}{character vector, \code{"r"} and/or
#'       \code{"executable"}}
#'     \item{\code{fn}}{function or \code{NULL}, sourced R function (for R
#'       scripts only)}
#'     \item{\code{formals}}{character or \code{NULL}, human-readable function
#'       signature (for R scripts only)}
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

    # Script name: relative path without extension
    script_name <- sub("\\.[^.]+$", "", rel_path)

    # R scripts get sourced; others are executables
    if (ext %in% c("r")) {
      fn <- NULL
      fn_formals <- NULL

      tryCatch(
        {
          env <- new.env(parent = baseenv())
          source(full_path, local = env, echo = FALSE, verbose = FALSE)
          # Find the function matching the basename (sans extension)
          base_name <- tools::file_path_sans_ext(basename(rel_path))
          fns <- Filter(function(nm) is.function(env[[nm]]), ls(env))
          if (base_name %in% fns) {
            fn <- env[[base_name]]
          } else if (length(fns) == 1L) {
            # Only one function defined; use it
            fn <- env[[fns[[1L]]]]
          }
          if (is.function(fn)) {
            fmls <- names(formals(fn))
            fmls <- fmls[!startsWith(fmls, ".")]
            fn_formals <- paste(fmls, collapse = ", ")
          }
        },
        error = function(e) {
          warning(
            "Failed to source R script '", rel_path, "': ",
            conditionMessage(e),
            call. = FALSE
          )
        }
      )

      scripts[[script_name]] <- list(
        name = script_name,
        path = full_path,
        ext = ext,
        type = if (is.function(fn)) c("r", "rscript") else "rscript",
        fn = fn,
        formals = fn_formals
      )
    } else {
      script_type <- switch(
        ext,
        "py" = "python",
        "sh" = "shell", "bash" = "shell",
        "bat" = "batch", "cmd" = "batch",
        "executable"
      )
      scripts[[script_name]] <- list(
        name = script_name,
        path = full_path,
        ext = ext,
        type = script_type,
        fn = NULL,
        formals = NULL
      )
    }
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
    # Fallback: return formals if we can parse the file
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
#'   exposes it as a suite of \code{\link[ellmer]{tool}} definitions via the
#'   \code{$make_tools()} method.
#'
#' @details
#' A skill directory must contain a \code{SKILL.md} file with optional YAML
#' frontmatter (\code{name}, \code{description}, \code{metadata}) and a
#' markdown body. The directory may also contain:
#' \itemize{
#'   \item Reference files (any top-level file other than \code{SKILL.md})
#'   \item A \code{scripts/} subdirectory with callable R, shell, or python
#'     scripts
#' }
#'
#' The \code{$make_tools()} method returns up to three
#' \code{\link[ellmer]{tool}} objects:
#' \enumerate{
#'   \item \verb{skill-\{name\}-readme}: always created; returns the
#'     \code{SKILL.md} body
#'   \item \verb{skill-\{name\}-references}: created when reference files
#'     exist; reads file contents with optional line range and grep filtering
#'   \item \verb{skill-\{name\}-script-call}: created when scripts exist;
#'     calls R functions or executables
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

    #' @field file_choices character, filenames available for the references
    #'   tool (top-level files excluding \code{SKILL.md})
    file_choices = NULL,

    #' @description Initialize a new \code{Skill} from a skill directory
    #' @param path character, path to the skill directory (must contain
    #'   \code{SKILL.md})
    initialize = function(path) {
      # path = system.file("skills", "weather", package = "tricobbler")
      # self = Skill$new(path)
      # private <- self$.__enclos_env__$private
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

      # Discover top-level files for references (excluding SKILL.md)
      path <- private$.path
      all_files <- list.files(path, recursive = FALSE, include.dirs = FALSE)
      # Filter out directories (list.files may include them on some filesystems)
      all_dirs <- list.dirs(path, full.names = FALSE, recursive = FALSE)
      all_files <- all_files[!all_files %in% c(all_dirs, "SKILL.md")]
      self$file_choices <- all_files

      # Discover scripts
      scripts_dir <- file.path(path, "scripts")
      self$scripts <- discover_scripts(scripts_dir)

      invisible(self)
    },

    #' @description Produce \code{\link[ellmer]{tool}} definitions for this
    #'   skill
    #' @returns A named list of \code{\link[ellmer]{tool}} objects. Names are
    #'   \verb{skill-\{name\}-readme}, and optionally
    #'   \verb{skill-\{name\}-references} and
    #'   \verb{skill-\{name\}-script-call}.
    make_tools = function() {

      skill_name <- private$.name
      skill_body <- self$body
      skill_path <- private$.path
      skill_file_choices <- self$file_choices
      skill_scripts <- self$scripts
      skill_description <- self$description

      has_references <- length(skill_file_choices) > 0L
      has_scripts <- length(skill_scripts) > 0L

      # ------ Build sibling tool cross-reference text ------
      sibling_parts <- character(0L)
      if (has_references) {
        sibling_parts <- c(
          sibling_parts,
          sprintf(
            "Use `skill-%s-references` to read reference files.",
            skill_name
          )
        )
      }
      if (has_scripts) {
        sibling_parts <- c(
          sibling_parts,
          sprintf(
            "Use `skill-%s-script-call` to execute scripts.",
            skill_name
          )
        )
      }
      sibling_text <- paste(sibling_parts, collapse = " ")

      tools <- list()

      # ============================================================
      # Tool 1: skill-{name}-readme (always)
      # ============================================================
      readme_name <- sprintf("skill-%s-readme", skill_name)
      readme_desc <- sprintf(
        "Skill `%s`: %s. This tool returns the skill description (SKILL.md body). %s",
        skill_name,
        skill_description,
        if (nzchar(sibling_text)) {
          paste0("\n\n", sibling_text)
        } else {
          ""
        }
      )

      readme_fn <- function() {
        ellmer::ContentText(skill_body)
      }

      tools[[readme_name]] <- ellmer::tool(
        fun = readme_fn,
        description = readme_desc,
        arguments = list(),
        name = readme_name
      )

      # ============================================================
      # Tool 2: skill-{name}-references (conditional)
      # ============================================================
      if (has_references) {
        ref_name <- sprintf("skill-%s-references", skill_name)

        # Determine default file
        sel <- tolower(skill_file_choices) %in% "reference.md"
        if (any(sel)) {
          default_file <- skill_file_choices[sel][[1]]
        } else {
          sel <- endsWith(tolower(skill_file_choices), ".md")
          default_file <- c(skill_file_choices[sel], skill_file_choices)[[1]]
        }

        ref_desc <- sprintf(
          paste0(
            "Read reference files from skill '%s'. ",
            "Provides further context to `skill-%s-readme`.\n\n",
            "Available files: %s\n",
            "Default (if `file_name` is not specified): \"%s\"\n\n",
            "Pass an empty string for `file_name` to list available files ",
            "without reading content."
          ),
          skill_name,
          skill_name,
          paste(sprintf("\"%s\"", skill_file_choices), collapse = ", "),
          default_file
        )

        # Capture choices and base path in closure
        ref_fn <- function(
          file_name = default_file,
          pattern = "",
          line_start = 1L,
          line_end = -1L
        ) {
          # If empty or NA, return available choices
          if (is.na(file_name) || !nzchar(file_name)) {
            return(paste(
              collapse = "\n",
              c(
                "Available reference files:",
                paste("  -", skill_file_choices, collapse = "\n")
              )
            ))
          }

          # Validate file_name
          file_name <- match.arg(file_name, choices = skill_file_choices)

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
          if (is.na(line_end) || line_end < 0L) {
            line_end <- n
          } else {
            line_end <- min(as.integer(line_end), n)
          }

          if (line_start > n) {
            return("")
          }

          line_index <- seq(line_start, line_end)
          lines <- lines[line_index]

          # Apply grep pattern
          if (length(pattern) == 1 && !is.na(pattern) && nzchar(pattern)) {
            sel <- grepl(pattern, x = lines, perl = TRUE)
            line_index <- line_index[sel]
            lines <- lines[sel]
          }

          # TODO: think about also returning the line number
          paste(lines, collapse = "\n")
        }

        ref_arguments <- list(
          file_name = ellmer::type_enum(
            values = skill_file_choices,
            description = sprintf(
              "Name of the reference file to read. Default: \"%s\". Pass empty string to list available choices.", # nolint: line_length_linter.
              default_file
            )
          ),
          pattern = ellmer::type_string(
            description = "Optional perl-compatible regular expression to filter lines. Empty string means no filtering.", # nolint: line_length_linter.
            required = FALSE
          ),
          line_start = ellmer::type_integer(
            description = "Start line number (1-based). Default: 1",
            required = FALSE
          ),
          line_end = ellmer::type_integer(
            description = "End line number (1-based, inclusive). Default: -1 (read to end)", # nolint: line_length_linter.
            required = FALSE
          )
        )

        tools[[ref_name]] <- ellmer::tool(
          fun = ref_fn,
          description = ref_desc,
          arguments = ref_arguments,
          name = ref_name
        )
      }

      # ============================================================
      # Tool 3: skill-{name}-script-call (conditional)
      # ============================================================
      if (has_scripts) {
        script_name <- sprintf("skill-%s-script-call", skill_name)

        script_names <- names(skill_scripts)

        # Build per-script documentation for the description
        script_docs <- vapply(script_names, function(sn) {
          s <- skill_scripts[[sn]]
          type_str <- paste(s$type, collapse = "/")
          sig <- if (!is.null(s$formals)) {
            sprintf(" (%s)", s$formals)
          } else {
            ""
          }
          sprintf("  - \"%s\" [%s]%s", sn, type_str, sig)
        }, character(1L))

        script_desc <- sprintf(
          paste0(
            "Execute a script from skill '%s'.\n\n",
            "Available scripts:\n%s\n\n",
            "For `.type`:\n",
            "- \"auto\": use R call if available, otherwise executable\n",
            "- \"r\": call the R function directly (R scripts only)\n",
            "- \"executable\": run as a command-line process ",
            "(interpreter auto-detected: Rscript, python3, bash)\n\n",
            "The `args` parameter is a JSON string of named arguments. ",
            "For R calls, these are passed to the function via `do.call()`. ",
            "For executables, they are serialized as command-line flags.\n\n",
            "Pass `args = \"--help\"` or `args = \"-h\"` to get usage ",
            "information instead of executing the script."
          ),
          skill_name,
          paste(script_docs, collapse = "\n")
        )

        script_fn <- function(.name, .type = "auto", args = "{}") {
          print(list(
            .name = .name,
            .type = .type,
            args = args
          ))
          # Resolve script by name: try full path first, then simple basename
          script <- skill_scripts[[.name]]
          if (is.null(script)) {
            # Try matching by basename (sans extension)
            base_matches <- Filter(function(s) {
              tools::file_path_sans_ext(basename(s$path)) == .name
            }, skill_scripts)
            if (length(base_matches) > 0L) {
              script <- base_matches[[1L]]
            }
          }
          if (is.null(script)) {
            stop(
              "Script '", .name, "' not found. Available scripts: ",
              paste(names(skill_scripts), collapse = ", ")
            )
          }

          # Help mode: return usage information instead of executing
          if (args %in% c("--help", "-h")) {
            header <- sprintf("Script: %s [%s]",
                             script$name,
                             paste(script$type, collapse = "/"))
            if ("r" %in% script$type && is.function(script$fn)) {
              fn_name <- tools::file_path_sans_ext(basename(script$path))
              roxygen <- extract_roxygen_help(script$path, fn_name)
              fmls <- if (!is.null(script$formals)) {
                sprintf("\nArguments: %s", script$formals)
              } else {
                ""
              }
              return(paste(c(header, fmls, "", roxygen), collapse = "\n"))
            } else {
              # Executable: run with --help via interpreter
              cmd_info <- build_script_command(
                script$path, script$ext
              )
              result <- tryCatch(
                processx::run(
                  command = cmd_info$command,
                  args = c(cmd_info$prefix_args, "--help"),
                  error_on_status = FALSE,
                  timeout = 10
                ),
                error = function(e) {
                  list(stdout = "", stderr = conditionMessage(e), status = 1L)
                }
              )
              output <- if (nzchar(result$stdout)) {
                result$stdout
              } else if (nzchar(result$stderr)) {
                result$stderr
              } else {
                "(no help output available)"
              }
              return(paste(c(header, "", output), collapse = "\n"))
            }
          }

          # Parse args (convert = FALSE, so args is always a raw string)
          # Try JSON first; if it fails, treat as CLI string
          if (is.null(args) || !nzchar(args) || identical(args, "{}")) {
            parsed_args <- list()
          } else {
            parsed_args <- tryCatch(
              jsonlite::fromJSON(args, simplifyVector = TRUE),
              error = function(e) NULL
            )
            if (is.null(parsed_args)) {
              # Not valid JSON — report the raw value back
              stop(
                "Failed to parse `args` as JSON.\n",
                "Received: ", args, "\n",
                "Expected a JSON object, e.g.: ",
                "{\"location\": \"Houston\"}"
              )
            }
            if (!is.list(parsed_args)) {
              parsed_args <- as.list(parsed_args)
            }
          }

          # Determine call type
          .type <- match.arg(.type, choices = c("auto", "r", "executable"))
          use_r <- switch(
            .type,
            "auto" = "r" %in% script$type && is.function(script$fn),
            "r" = TRUE,
            "executable" = FALSE
          )

          if (use_r) {
            # Direct R function call
            if (!is.function(script$fn)) {
              stop(
                "Script '", .name, "' cannot be called as an R function. ",
                "Available types: ",
                paste(script$type, collapse = ", "),
                ". Try `.type = \"executable\"`."
              )
            }

            # Remove args not in formals (except if function has ...)
            fn_formals <- names(formals(script$fn))
            if (!"..." %in% fn_formals) {
              parsed_args <- parsed_args[
                names(parsed_args) %in% fn_formals
              ]
            }
            # Remove dot-prefixed internal args (e.g., .runtime)
            parsed_args <- parsed_args[
              !grepl("^\\.", names(parsed_args))
            ]

            result <- do.call(script$fn, parsed_args)
            return(result)

          } else {
            # Process call: dispatch interpreter by script type
            cmd_args <- character(0L)

            for (nm in names(parsed_args)) {
              val <- parsed_args[[nm]]
              cmd_args <- c(cmd_args, sprintf("--%s", nm), as.character(val))
            }

            cmd_info <- build_script_command(script$path, script$ext)
            result <- processx::run(
              command = cmd_info$command,
              args = c(cmd_info$prefix_args, cmd_args),
              error_on_status = FALSE
            )

            if (result$status != 0L) {
              stop(
                "Script '", .name, "' exited with status ", result$status,
                ".\nstderr: ", result$stderr
              )
            }

            return(result$stdout)
          }
        }

        script_arguments <- list(
          .name = ellmer::type_enum(
            values = script_names,
            description = "Name of the script to call. Use the full relative path or the simple name if unambiguous." # nolint: line_length_linter.
          ),
          .type = ellmer::type_enum(
            values = c("auto", "r", "executable"),
            description = "Execution mode: 'auto' (default), 'r' (R function call), or 'executable' (command-line)." # nolint: line_length_linter.
          ),
          args = ellmer::type_string(
            description = "A JSON object string of named arguments to pass to the script. Pass \"--help\" or \"-h\" to get usage information.", # nolint: line_length_linter.
            required = FALSE
          )
        )

        tools[[script_name]] <- ellmer::tool(
          fun = script_fn,
          description = script_desc,
          arguments = script_arguments,
          name = script_name,
          convert = FALSE
        )
      }

      tools
    }
  )
)
