# ---------------------------------------------------------------------------
# Sandboxed File I/O Tool Generators
# ---------------------------------------------------------------------------
# Factory functions that produce ellmer::tool() objects scoped to a
# root directory.
# ---------------------------------------------------------------------------


#' Create file I/O tools in sand-box
#'
#' @description
#' Generate \pkg{ellmer} \code{ToolDef} objects that restrict all
#' file operations to a given root directory.  Every path argument
#' accepted by the returned tools is \emph{relative} to
#' \code{root_path}; attempts to escape the sandbox (e.g. via
#' \code{..}) are rejected with an informative error that the \verb{LLM}
#' can act on.
#'
#' \code{sandboxed_tools()} returns a named list of all four tools
#' at once.  The individual helpers
#' (\code{sandboxed_read_file}, \code{sandboxed_write_file}, etc.)
#' return a single \code{ToolDef} each and are useful when you only
#' need a subset.
#'
#' @param root_path character, the directory to which every file
#'   operation is confined.  The directory is created (recursively)
#'   if it does not already exist.
#' @param read_n_lines integer, default number of lines returned by
#'   the read tool when the caller does not specify
#'   \code{n_lines}.
#'
#' @return For \code{sandboxed_tools()}: a named list of four
#'   \code{ToolDef} objects (\code{read_file}, \code{write_file},
#'   \code{walk_directory}, \code{copy_file}).
#'   For the individual helpers: a single \code{ToolDef}.
#'
#' @examples
#' root <- file.path(tempdir(), "sandbox-demo")
#' tools <- sandboxed_tools(root)
#' names(tools)
#'
#' # Individual tool
#' read_tool <- sandboxed_read_file(root)
#'
#' @name sandboxed-tools
NULL


# -- internal path validation -----------------------------------------------

#' @noRd
resolve_sandboxed_path <- function(root_path, relative_path, ensure_root = FALSE) {
  # Reject absolute paths
  if (grepl("^[/\\\\~]", relative_path)) {
    stop(
      "Absolute paths are not allowed. ",
      "Use a relative path inside the sandbox."
    )
  }
  if (startsWith(relative_path, "[")) {
    stop("Path cannot start with square brackets `[`.")
  }
  if (startsWith(relative_path, "\"")) {
    stop("Path cannot start with quotes `\"`.")
  }

  # Hard-reject any ".." component — no traversal allowed
  parts <- unlist(strsplit(relative_path, "[/\\\\]+"))
  if (any(parts == "..")) {
    stop(
      "Path must not contain '..'. ",
      "Use a simple relative path that stays inside the sandbox."
    )
  }

  # Reject illegal characters (strict cross-platform set)
  # Forbidden on Windows: < > : " | ? *
  # Also reject control characters (0x00-0x1F) and DEL (0x7F)
  # Also reject backslash (normalise to forward-slash only)
  illegal_re <- "[<>:\"|?*\\\\]|[[:cntrl:]]"
  if (grepl(illegal_re, relative_path, perl = TRUE)) {
    stop(
      "Path contains illegal characters. ",
      "Stop using < > : \" | ? * \\ and control characters."
    )
  }

  root_path <- normalizePath(root_path, mustWork = FALSE)
  if (!dir.exists(root_path) && ensure_root) {
    dir.create(root_path, recursive = TRUE)
  }

  # Filter out empty segments and "." (current dir)
  resolved_parts <- parts[nzchar(parts) & parts != "."]

  if (length(resolved_parts) == 0L) {
    return(root_path)
  }

  file.path(root_path, paste(resolved_parts, collapse = .Platform$file.sep))
}


# -- individual generators --------------------------------------------------


#' @rdname sandboxed-tools
#' @export
sandboxed_read_file <- function(root_path, read_n_lines = 50L) {
  root_path <- normalizePath(root_path, mustWork = FALSE)
  read_n_lines <- as.integer(read_n_lines)

  tool2(
    fun = function(relative_path, line_start = 1L, n_lines = read_n_lines) {

      # relative_path <- sanitize_string_arg(relative_path)
      # line_start <- sanitize_string_arg(line_start)
      # n_lines <- sanitize_string_arg(n_lines)

      full <- resolve_sandboxed_path(root_path, relative_path)

      if (!file.exists(full)) {
        stop(paste0("File not found: ", relative_path))
      }

      lines <- readLines(full, warn = FALSE)
      total <- length(lines)

      line_start <- max(as.integer(line_start), 1L)
      n_lines <- as.integer(n_lines)
      line_end <- min(line_start + n_lines - 1L, total)

      if (line_start > total) {
        return(sprintf(
          "--- File '%s' has %d lines; line_start=%d is past end of file ---",
          relative_path, total, line_start
        ))
      }

      selected <- lines[seq.int(line_start, line_end)]
      header <- sprintf(
        "--- %s  (lines %d-%d of %d) ---",
        relative_path, line_start, line_end, total
      )
      paste(c(header, selected), collapse = "\n")
    },
    description = paste0(
      "Read lines from a file inside the sandbox. ",
      "Returns a chunk of lines starting at 'line_start' ",
      "(1-based). Use 'n_lines' to control how many lines to read."
    ),
    arguments = list(
      relative_path = ellmer::type_string(
        "Relative path inside the sandbox root.",
        required = TRUE
      ),
      line_start = ellmer::type_integer(
        "First line to read (1-based, default 1).",
        required = FALSE
      ),
      n_lines = ellmer::type_integer(
        paste0(
          "Number of lines to read (default ",
          read_n_lines, ")."
        ),
        required = FALSE
      )
    ),
    name = "read_file"
  )
}

#' @rdname sandboxed-tools
#' @param text character, the text content to expose as a virtual file.
#' @param description character, a short human-readable label for the
#'   virtual file shown to the \verb{LLM} in tool descriptions.
#' @export
sandboxed_load_text <- function(text, description, read_n_lines = 50L) {
  read_n_lines <- as.integer(read_n_lines)

  lines <- strsplit(paste(text, collapse = "\n"), "\n")[[1]]
  total <- length(lines)

  tool2(
    fun = function(line_start = 1L, n_lines = read_n_lines) {

      # line_start <- sanitize_string_arg(line_start)
      # n_lines <- sanitize_string_arg(n_lines)

      line_start <- max(as.integer(line_start), 1L, na.rm = TRUE)
      n_lines <- max(as.integer(n_lines), 1L, na.rm = TRUE)
      line_end <- min(line_start + n_lines - 1L, total)

      if (line_start > total) {
        return(sprintf(
          "--- line_start=%d is past end of file (total %d lines) ---",
          line_start, total
        ))
      }

      selected <- lines[seq.int(line_start, line_end)]
      header <- sprintf(
        "--- (lines %d-%d of %d) ---",
        line_start, line_end, total
      )
      paste(c(header, selected), collapse = "\n")
    },
    description = paste(
      collapse = "\n",
      c(
        description,
        "\nReturns a chunk of lines starting at 'line_start' ",
        "(1-based). Use 'n_lines' to control how many lines to read."
      )
    ),
    arguments = list(
      line_start = ellmer::type_integer(
        "First line to read (1-based, default 1).",
        required = FALSE
      ),
      n_lines = ellmer::type_integer(
        paste0(
          "Number of lines to read (default ",
          read_n_lines, ")."
        ),
        required = FALSE
      )
    ),
    name = sprintf("load_text_%s", digest::digest(list(text, description), algo = "crc32"))
  )
}

#' @rdname sandboxed-tools
#' @export
sandboxed_write_file <- function(root_path) {
  root_path <- normalizePath(root_path, mustWork = FALSE)

  tool2(
    fun = function(relative_path, content, append = FALSE) {

      # relative_path_alt <- sanitize_string_arg(relative_path)
      # if (!identical(relative_path, relative_path_alt)) {
      #   relative_path <- relative_path_alt
      #   content <- sanitize_string_arg(content)
      #   append <- sanitize_string_arg(append)
      # }

      full <- resolve_sandboxed_path(root_path, relative_path, ensure_root = TRUE)

      parent <- dirname(full)
      if (!dir.exists(parent)) {
        dir.create(parent, recursive = TRUE)
      }

      append <- isTRUE(append)
      if (append) {
        cat(content, "\n", sep = "", file = full, append = TRUE)
      } else {
        writeLines(content, full, useBytes = TRUE)
      }

      n_lines <- count_lines(full)
      action <- if (append) "Appended to" else "Wrote"
      sprintf(
        "%s '%s' (now %d lines).",
        action, relative_path, n_lines
      )
    },
    description = paste0(
      "Write (or append to) a file inside the sandbox. ",
      "Set 'append' to true to add content to an existing file ",
      "rather than overwriting it."
    ),
    arguments = list(
      relative_path = ellmer::type_string(
        "Relative path inside the sandbox root."
      ),
      content = ellmer::type_string(
        "Text content to write."
      ),
      append = ellmer::type_boolean(
        "If true, append to file instead of overwriting (default false).",
        required = FALSE
      )
    ),
    name = "write_file"
  )
}


#' @rdname sandboxed-tools
#' @export
sandboxed_walk_directory <- function(root_path) {
  root_path <- normalizePath(root_path, mustWork = FALSE)

  tool2(
    fun = function(relative_path = ".") {
      # relative_path <- sanitize_string_arg(relative_path)

      full <- resolve_sandboxed_path(root_path, relative_path, ensure_root = FALSE)

      if (!dir.exists(full)) {
        return(list(
          sandboxed_root = root_path,
          relative_search_path = relative_path,
          directories = character(0L),
          files = character(0L),
          notes = "Search path has not been created yet."
        ))
      }

      entries <- list.files(full, all.files = FALSE, full.names = FALSE, recursive = TRUE, include.dirs = TRUE)
      info <- file.info(file.path(full, entries))

      list(
        sandboxed_root = root_path,
        relative_search_path = relative_path,
        directories = entries[info$isdir],
        files = entries[!info$isdir]
      )
    },
    description = paste0(
      "List files and directories inside the sandbox. ",
      "Returns a dictionary of relative directories and files"
    ),
    arguments = list(
      relative_path = ellmer::type_string(
        paste0(
          "Relative directory path inside the sandbox ",
          "(default '.' for root)."
        ),
        required = FALSE
      )
    ),
    name = "walk_directory"
  )
}


#' @rdname sandboxed-tools
#' @export
sandboxed_copy_file <- function(root_path) {
  root_path <- normalizePath(root_path, mustWork = FALSE)

  tool2(
    fun = function(from_path, to_path) {
      # from_path <- sanitize_string_arg(from_path)
      # to_path <- sanitize_string_arg(to_path)

      if (length(from_path) != 1) {
        stop("source file `from_path` length must be 1")
      }
      from <- resolve_sandboxed_path(root_path, from_path, root_path = FALSE)
      to <- resolve_sandboxed_path(root_path, to_path, root_path = FALSE)

      if (!file.exists(from)) {
        return(paste0("Source not found: ", from_path))
      }

      parent <- dirname(to)
      if (!dir.exists(parent)) {
        dir.create(parent, recursive = TRUE)
      }

      is_dir <- file.info(from)[['isdir']]

      if (is_dir) {

        if (!dir.exists(to)) {
          dir.create(to, recursive = TRUE)
        }

        fs <- list.files(path = from, all.files = TRUE, full.names = FALSE, include.dirs = TRUE, recursive = FALSE, no.. = TRUE)
        for (f in fs) {
          src_path <- file.path(from, f)
          if (file.info(src_path)[["isdir"]]) {
            file.copy(from = src_path, to = to, overwrite = TRUE, recursive = TRUE, copy.mode = TRUE, copy.date = TRUE)
          } else {
            file.copy(from = src_path, to = file.path(to, f), overwrite = TRUE, recursive = FALSE, copy.mode = TRUE, copy.date = TRUE)
          }
        }

      } else {
        file.copy(from, to, overwrite = TRUE)
      }

      sprintf("Copied '%s' -> '%s'.", from_path, to_path)
    },
    description = paste0(
      "Copy a file/directory within the sandbox. Both source and ",
      "destination must be relative paths inside the ",
      "sandbox root."
    ),
    arguments = list(
      from_path = ellmer::type_string(
        "Relative source path."
      ),
      to_path = ellmer::type_string(
        "Relative destination path."
      )
    ),
    name = "copy_file"
  )
}


#' @rdname sandboxed-tools
#' @export
sandboxed_tools <- function(root_path, read_n_lines = 50L) {
  list(
    read_file = sandboxed_read_file(root_path, read_n_lines),
    write_file = sandboxed_write_file(root_path),
    walk_directory = sandboxed_walk_directory(root_path),
    copy_file = sandboxed_copy_file(root_path)
  )
}
