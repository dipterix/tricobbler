#' Execution Context for Workflow State Management
#'
#' @description R6 class that manages the execution environment for workflow
#'   states. Provides logging, result storage, and attachment management during
#'   workflow execution. This is part of the Runtime Layer (Tier 2) alongside
#'   \code{Scheduler}.
#' @export
AgentContext <- R6::R6Class(
  classname = "TricobblerAgentContext",
  portable = TRUE,
  cloneable = FALSE,
  private = list(
    # where to store shared memory, individual conversations, attachments
    .rootpath = character(),

    # scheduler
    .scheduler = NULL,

    # ID of the context, created automatically
    .id = NULL,
    .results = NULL
  ),
  active = list(
    #' @field id character, unique identifier for this context (read-only)
    id = function() {
      private$.id
    },

    #' @field rootpath character, root directory for all context storage
    #' (read-only)
    rootpath = function() {
      private$.rootpath
    },

    #' @field current_stage character, currently executing stage from scheduler
    #' (read-only)
    current_stage = function() {
      private$.scheduler$current_stage
    },

    #' @field current_state character, currently executing state from scheduler
    #' (read-only)
    current_state = function() {
      private$.scheduler$current_state
    },

    #' @field memory_path character, path to shared memory directory (read-only)
    memory_path = function() {
      file.path(self$rootpath, "memory")
    },

    #' @field store_path character, path to this context's conversation
    #' directory (read-only)
    store_path = function() {
      file.path(self$rootpath, "conversations", self$id)
    },

    # eventually I would like the loggers to be accessible by all agents
    # and is de-id'd. The attachment can be accessible by safe local agents
    # to prevent data leaks
    #' @field logger_path character, path to the log file (read-only)
    logger_path = function() {
      file.path(self$store_path, "logger.log")
    },

    #' @field attachment_path character, path to attachments directory
    #' (read-only)
    attachment_path = function() {
      file.path(self$store_path, "attachments")
    }
  ),
  public = list(
    #' @description Associate a scheduler with this context
    #' @param scheduler Scheduler object to associate
    set_scheduler = function(scheduler) {
      stopifnot(
        R6::is.R6(scheduler) && inherits(scheduler, "TricobblerScheduler")
      ) # nolint: line_length_linter.
      private$.scheduler <- scheduler
    },

    #' @description Initialize a new context
    #' @param id character, unique identifier (NULL to auto-generate)
    #' @param path character, root directory path (NULL for default cache)
    initialize = function(id = NULL, path = NULL) {
      if (length(path) == 1) {
        if (!dir.exists(path)) {
          stop("AgentContext$new(): `path` must be an existing directory. Please create the folder first") # nolint: line_length_linter.
        }
      } else {
        path <- file.path(
          tools::R_user_dir("tricobbler", which = "cache"),
          "context"
        ) # nolint: line_length_linter.
      }
      private$.rootpath <- path

      if (length(id) > 0) {
        # fetch existing context
        if (!file.exists(self$logger_path)) {
          stop("Unable to retrieve context with ID ", id)
        }
      } else {
        # generate ID based on scheduler and time
        dg <- substr(
          digest::digest(list(Sys.getpid(), Sys.info(), Sys.time())),
          1L,
          6L
        ) # nolint: line_length_linter.
        id <- sprintf("%s-%s", format(Sys.time(), "%y%m%dT%H%M%S"), dg)
      }

      private$.id <- id
      private$.results <- fastmap::fastqueue()
    },

    #' @description Create directory structure and initialize logging
    init_resources = function() {
      dir.create(self$memory_path, showWarnings = FALSE, recursive = TRUE)
      dir.create(self$store_path, showWarnings = FALSE, recursive = TRUE)
      dir.create(self$attachment_path, showWarnings = FALSE, recursive = TRUE)
      file.create(self$logger_path, showWarnings = FALSE)

      # reload result index
      private$.results <- NULL

      index_path <- file.path(self$attachment_path, "index")
      if (file.exists(index_path)) {
        tbl <- read.table(
          index_path,
          header = FALSE,
          col.names = c(
            "stage",
            "state",
            "agent_id",
            "current_attempt",
            "filename"
          )
        )
        private$.results <- tbl[rev(seq_len(nrow(tbl))), ]
      }
      invisible(self)
    },

    #' @description Write timestamped messages to log file
    #' @param ... character, message components to paste together
    #' @param caller object, the caller (scheduler, context, or agent)
    #' @param level character, log level (INFO, WARN, ERROR, FATAL)
    #' @param verbose character or logical, whether to verbose the log results
    #'    to the R console. When logical \code{TRUE}, it is equivalent to
    #'    \code{'cli'} (the default), which uses the \pkg{cli} package for
    #'    colored/styled console outputs. If \pkg{cli} is not installed,
    #'    falls back to \code{'base'}. When logical \code{FALSE} or
    #'    character \code{'none'}, the logs will not be printed out.
    #'    Use \code{'base'} to force plain \code{cat()}/\code{message()}
    #'    output without styling.
    logger = function(
      ...,
      caller = get_globals("active_agent"),
      level = c("INFO", "WARN", "ERROR", "FATAL"),
      verbose = c("cli", "base", "none")
    ) {
      # identify the role based on caller
      if (is.null(caller)) {
        caller <- dynGet("self")
      }
      if (identical(caller, private$.scheduler)) {
        role <- "scheduler"
        level <- "TRACE"
      } else if (identical(caller, self)) {
        role <- "context"
        if (length(level) > 1) {
          level <- match.arg(level)
        }
      } else {
        if (!S7::S7_inherits(caller, class = Agent)) {
          stop("The caller must be an `tricobbler::Agent`")
        }
        role <- sprintf("Agent %s", paste(caller@id, collapse = ""))
        level <- match.arg(level)
      }
      # Normalize verbose parameter
      if (is.logical(verbose)) {
        verbose <- if (isTRUE(verbose)) "cli" else "none"
      } else {
        verbose <- match.arg(verbose)
      }

      prefix <- sprintf(
        "%s %s [%s]: ",
        level,
        format(Sys.time(), "%H:%M:%S"),
        role
      )
      str <- paste(c(...), collapse = "")
      str <- gsub("[\b\r]", "", str)
      str <- strsplit(str, "\n")[[1]]
      str <- paste0(prefix, str, collapse = "\n")
      # write to file with UTF-8 encoding
      logger_path <- self$logger_path
      if (file.exists(logger_path)) {
        con <- file(logger_path, open = "a", encoding = "UTF-8")
        on.exit(close(con), add = TRUE)
        cat(str, sep = "\n", file = con)
      }

      # Console output based on verbose setting
      if (verbose == "none") {
        return(invisible(NULL))
      }

      str_wrapped <- strwrap(
        strsplit(str, "\n")[[1]],
        width = getOption("width") - 1L,
        exdent = 4L
      )
      str_wrapped <- paste(str_wrapped, collapse = "\n")

      # Use cli if requested, available, and console supports ANSI colors
      use_cli <- verbose == "cli" &&
        package_installed("cli") &&
        call_pkg_fun("cli", "num_ansi_colors",
          .if_missing = "none", .missing_default = 1L
        ) > 1L

      if (use_cli) {
        # Use cli for colored output
        cli_style <- switch(
          level,
          "TRACE" = "col_grey",
          "DEBUG" = "col_silver",
          "INFO"  = "col_blue",
          "WARN"  = "col_yellow",
          "ERROR" = "col_red",
          "FATAL" = "col_magenta",
          NULL
        )
        if (!is.null(cli_style)) {
          styled_str <- call_pkg_fun(
            "cli", cli_style, str_wrapped,
            .if_missing = "none", .missing_default = str_wrapped
          )
        } else {
          styled_str <- str_wrapped
        }
        if (level %in% c("WARN", "ERROR", "FATAL")) {
          message(styled_str)
        } else {
          cat(styled_str, sep = "\n")
        }
      } else {
        # Base: use cat/message without styling
        if (level %in% c("WARN", "ERROR", "FATAL")) {
          message(str_wrapped)
        } else {
          cat(str_wrapped, sep = "\n")
        }
      }
    },

    #' @description Execute expression with this context activated
    #' @param expr expression, the expression to evaluate with the
    #'   context active
    #' @param quoted logical, whether \code{expr} is already quoted
    #' @param env environment in which to evaluate the expression
    with_activated = function(expr, quoted = FALSE, env = parent.frame()) {
      if (!quoted) {
        expr <- substitute(expr)
      }
      with_globals(
        list(active_context = self),
        expr = expr,
        quoted = TRUE,
        env = env
      )
    },

    #' @description Save agent output with metadata
    #' @param result object, the result from agent execution
    #' @param stage character, stage name
    #' @param state character, state name
    #' @param agent_id character, agent identifier
    #' @param current_attempt integer, attempt number
    #' @param description character, human-readable result description
    #' @param ... additional metadata to store
    record_result = function(
      result,
      stage,
      state,
      agent_id,
      current_attempt,
      description,
      ...
    ) {
      now <- Sys.time()
      dt <- format(now, "%y%m%dT%H%M%S")
      fname <- sprintf(
        "[%s][%s][%s]_%s_%d",
        stage,
        state,
        agent_id,
        dt,
        current_attempt
      )

      if (!length(description) && inherits(description, "missing")) {
        description <- NULL
      } else {
        description <- paste(description, collapse = "")
      }

      attachment <- list(
        result = result,
        description = description,
        agent_id = agent_id,
        stage = stage,
        state = state,
        current_attempt = current_attempt,
        ...,
        ._timestamp = now
      )

      attachment_path <- file.path(self$attachment_path, fname)
      saveRDS(attachment, attachment_path)

      index_path <- file.path(self$attachment_path, "index")
      tf <- tempfile()
      on.exit({
        unlink(tf, force = TRUE)
      })

      if (file.exists(index_path)) {
        file.copy(index_path, to = tf, overwrite = TRUE, recursive = FALSE)
      } else {
        file.create(tf)
      }
      row <- data.frame(
        stage = stage,
        state = state,
        agent_id = agent_id,
        current_attempt = current_attempt,
        filename = fname
      )
      write.table(
        row,
        file = tf,
        append = TRUE,
        row.names = FALSE,
        col.names = FALSE
      )
      file.copy(from = tf, to = index_path, overwrite = TRUE, recursive = FALSE)
      private$.results <- rbind(row, private$.results)

      if (inherits(result, "tricobbler_state_error")) {
        level <- "ERROR"
        prefix <- "Following error"
      } else {
        level <- "INFO"
        prefix <- "Following result"
      }
      if (!length(description)) {
        description <- "(Agent fail to describe the result. Please retrieve the raw output via the identifier)" # nolint: line_length_linter.
      }
      self$logger(
        sprintf(
          "%s recorded: Agent=%s, stage=%s, state=%s, attempt=%d, identifier=%s\n%s", # nolint: line_length_linter.
          prefix,
          agent_id,
          stage,
          state,
          current_attempt,
          fname,
          description
        ),
        caller = self,
        level = level
      )
      invisible(self)
    },

    #' @description Retrieve the most recent result(s)
    #' @param items integer, number of results to retrieve
    #' @param simplify logical, return single result if items == 1
    last_results = function(items = 1, simplify = length(items) == 1) {
      if (!is.data.frame(private$.results) || !nrow(private$.results)) {
        return(NULL)
      }
      n <- nrow(private$.results)
      items <- min(items, n)
      fnames <- private$.results$filename[seq_len(items)]
      attachment_root <- self$attachment_path
      ret <- lapply(fnames, function(fname) {
        attachment_path <- file.path(attachment_root, fname)
        if (file.exists(attachment_path)) {
          return(readRDS(attachment_path))
        }
        return(NULL)
      })
      if (length(simplify) && length(ret) == 1) {
        ret <- ret[[1]]
      }
      ret
    },

    #' @description Retrieve a specific attachment by its identifier
    #' @param attachment_id character, the attachment identifier
    #'    (filename from record_result)
    get_attachment = function(attachment_id) {
      if (missing(attachment_id)) {
        stop("AgentContext$get_attachment(): `attachment_id` is required")
      }

      if (
        !is.character(attachment_id) ||
          length(attachment_id) != 1 ||
          is.na(attachment_id) ||
          attachment_id == ""
      ) {
        stop("AgentContext$get_attachment(): `attachment_id` must be a single non-empty character string") # nolint: line_length_linter.
      }

      pattern <- "^\\[[^]]+\\]\\[[^]]+\\]\\[[^]]+\\]_[0-9]{6}T[0-9]{6}_[0-9]+$"
      if (!grepl(pattern, attachment_id)) {
        stop(
          "AgentContext$get_attachment(): Invalid identifier format. ",
          "Expected [stage][state][agent_id]_YYMMDDTHHMMSS_{attempt #}"
        )
      }

      attachment_path <- file.path(self$attachment_path, attachment_id)
      if (file.exists(attachment_path)) {
        return(readRDS(attachment_path))
      }

      idx <- private$.results
      if (!is.data.frame(idx) || !nrow(idx) || !"filename" %in% names(idx)) {
        idx <- self$list_attachments(reindex = TRUE)
      }

      in_history <- is.data.frame(idx) &&
        nrow(idx) &&
        attachment_id %in% idx$filename
      if (in_history) {
        stop(
          "AgentContext$get_attachment(): Attachment '",
          attachment_id,
          "' is recorded but the file is no longer available on disk ",
          "(might be deleted)."
        )
      }

      stop(
        "AgentContext$get_attachment(): Attachment '",
        attachment_id,
        "' is not registered. ",
        "Use context$list_attachments() to inspect available attachments."
      )
    },

    #' @description List all available attachments
    #' @param reindex logical, whether to reload the index from disk
    #'    (default FALSE)
    list_attachments = function(reindex = FALSE) {
      if (reindex) {
        # Reload index from disk
        index_path <- file.path(self$attachment_path, "index")
        if (file.exists(index_path)) {
          tbl <- read.table(
            index_path,
            header = FALSE,
            col.names = c(
              "stage",
              "state",
              "agent_id",
              "current_attempt",
              "filename"
            )
          )
          private$.results <- tbl[rev(seq_len(nrow(tbl))), ]
        } else {
          private$.results <- NULL
        }
      }

      if (!is.data.frame(private$.results) || nrow(private$.results) == 0) {
        return(character(0L))
      }

      return(private$.results)
    },

    #' @description Check if an attachment exists
    #' @param attachment_id character, the attachment identifier
    #'   (filename from record_result)
    #' @return logical, TRUE if the attachment exists on disk, FALSE otherwise
    has_attachment = function(attachment_id) {
      if (
        !is.character(attachment_id) ||
          length(attachment_id) != 1 ||
          is.na(attachment_id) ||
          attachment_id == ""
      ) {
        return(FALSE)
      }
      # Validate format: [stage][state][agent_id]_YYMMDDTHHMMSS_{attempt}
      pattern <- "^\\[[^]]+\\]\\[[^]]+\\]\\[[^]]+\\]_[0-9]{6}T[0-9]{6}_[0-9]+$"
      if (!grepl(pattern, attachment_id)) {
        return(FALSE)
      }
      attachment_path <- file.path(self$attachment_path, attachment_id)
      file.exists(attachment_path)
    },

    #' @description Read and parse log file contents
    #' @param method character, read from "tail" (end) or "head" (beginning)
    #' @param skip_lines integer, skipping lines relative to the end or start; 
    #'   If `method` is `"head"`, then `skip` skips the first several lines,
    #'   otherwise skipping the last several lines; default is `0L` 
    #'   (no skipping).
    #' @param max_lines integer, maximum number of lines to read (default 300)
    #' @param pattern character, optional regex pattern to filter log content
    #' @param levels character, log levels to include
    #'   (default: all levels)
    #' @return data.frame with columns: line_no, level, time, caller, content.
    #'   Returns empty data.frame if file missing or no matches.
    read_logs = function(
      method = c("tail", "head"),
      skip_lines = 0L,
      max_lines = 300L,
      pattern = NULL,
      levels = c("TRACE", "DEBUG", "INFO", "WARN", "ERROR", "FATAL")
    ) {
      method <- match.arg(method)
      logger_path <- self$logger_path

      # Empty result template
      empty_result <- data.frame(
        line_no = integer(0L),
        level = character(0L),
        time = character(0L),
        caller = character(0L),
        content = character(0L),
        stringsAsFactors = FALSE
      )

      if (!file.exists(logger_path)) {
        return(empty_result)
      }

      # Validate parameters
      max_lines <- as.integer(max_lines)
      if (is.na(max_lines) || max_lines < 1L) {
        max_lines <- 300L
      }
      skip_lines <- as.integer(skip_lines)
      if (is.na(skip_lines) || skip_lines < 0L) {
        skip_lines <- 0L
      }

      # Calculate skip and n_read based on method
      if (method == "head") {
        skip <- skip_lines
        n_read <- max_lines
        line_no_begin <- skip_lines + 1L
      } else {
        # Tail: need to count to know where to start
        n_total <- count_lines(logger_path)
        if (n_total == 0L) {
          return(empty_result)
        }
        line_no_end <- n_total - skip_lines
        line_no_begin <- max(1L, line_no_end - max_lines + 1L)
        if (line_no_begin > line_no_end) {
          return(empty_result)
        }
        skip <- line_no_begin - 1L
        n_read <- line_no_end - line_no_begin + 1L
      }

      # Single scan call with native skip (efficient, no read-and-discard)
      lines <- scan(
        logger_path, what = character(), skip = skip, nlines = n_read,
        sep = "\n", quiet = TRUE, blank.lines.skip = FALSE,
        fileEncoding = "UTF-8"
      )

      if (length(lines) == 0L) {
        return(empty_result)
      }

      line_nos <- seq(line_no_begin, by = 1L, length.out = length(lines))

      # Parse log format: {LEVEL} {HH:MM:SS} [{caller}]: {message}
      # Note: [^]]+ matches any char except ], works in R regex
      log_pattern <- paste0(
        "^(TRACE|DEBUG|INFO|WARN|ERROR|FATAL)\\s+",
        "(\\d{2}:\\d{2}:\\d{2})\\s+",
        "\\[([^]]+)\\]:\\s*(.*)"
      )
      matches <- regmatches(lines, regexec(log_pattern, lines))

      # Build result from parsed lines
      parsed_idx <- which(vapply(matches, length, integer(1L)) == 5L)

      if (length(parsed_idx) == 0L) {
        return(empty_result)
      }

      result <- data.frame(
        line_no = line_nos[parsed_idx],
        content = vapply(matches[parsed_idx], `[[`, character(1L), 5L),
        level = vapply(matches[parsed_idx], `[[`, character(1L), 2L),
        caller = vapply(matches[parsed_idx], `[[`, character(1L), 4L),
        time = vapply(matches[parsed_idx], `[[`, character(1L), 3L),
        stringsAsFactors = FALSE
      )

      # Filter by levels
      if (length(levels) > 0L) {
        result <- result[result$level %in% levels, , drop = FALSE]
      }

      # Filter by pattern (grep on content)
      if (length(pattern) == 1L && nzchar(pattern)) {
        matched <- grepl(pattern, result$content)
        if (!any(matched)) {
          return(empty_result)
        }
        result <- result[matched, , drop = FALSE]
      }

      rownames(result) <- NULL
      result
    }
  )
)




# --------------------------------------------------------------------------
# MCP Tools for Context Log Access
# --------------------------------------------------------------------------

#' Read Log Entries from Head (Beginning)
#'
#' @description
#' Read and parse log file contents from the beginning of the log file.
#' Returns structured log entries with level, time, caller, and content.
#' This tool is useful for reviewing the start of a workflow execution
#' or debugging early-stage issues.
#'
#' @param max_lines Integer, maximum number of lines to read (default 300).
#' @param skip_lines Integer, number of lines to skip from the beginning
#'   (default 0).
#' @param pattern Character, optional regex pattern to filter log content.
#'   Only entries matching the pattern in their content will be returned.
#' @param levels Character vector, log levels to include. Defaults to all
#'   levels: \code{TRACE}, \code{DEBUG}, \code{INFO}, \code{WARN},
#'   \code{ERROR}, \code{FATAL}.
#'
#' @return A list containing:
#' \describe{
#'   \item{success}{Logical, whether the operation completed successfully}
#'   \item{context_id}{Character, the context identifier}
#'   \item{count}{Integer, number of log entries returned}
#'   \item{entries}{List of log entries, each with \code{line_no},
#'     \code{level}, \code{time}, \code{caller}, \code{content}}
#'   \item{error}{Character, error message if success is \code{FALSE}}
#' }
#'
#' @keywords mcp-tool mcp-category-info mcp-output-json
#' @noRd
mcp_tool_context_logs_head <- function(
  max_lines = 300L,
  skip_lines = 0L,

  pattern = NULL,
  levels = c("TRACE", "DEBUG", "INFO", "WARN", "ERROR", "FATAL")
) {

  context <- get_globals("active_context")

  if (is.null(context)) {
    return(jsonlite::toJSON(
      list(
        success = FALSE,
        error = "No active context. Start a workflow execution first."
      ),
      auto_unbox = TRUE, pretty = FALSE
    ))
  }

  if (!R6::is.R6(context) || !inherits(context, "TricobblerAgentContext")) {
    return(jsonlite::toJSON(
      list(success = FALSE, error = "Invalid context object."),
      auto_unbox = TRUE, pretty = FALSE
    ))
  }

  result <- tryCatch(
    {
      logs <- context$read_logs(
        method = "head",
        max_lines = max_lines,
        skip_lines = skip_lines,
        pattern = pattern,
        levels = levels
      )

      jsonlite::toJSON(
        list(
          success = TRUE,
          context_id = context$id,
          count = nrow(logs),
          entries = logs
        ),
        auto_unbox = TRUE, dataframe = "rows", pretty = FALSE
      )
    },
    error = function(e) {
      jsonlite::toJSON(
        list(
          success = FALSE,
          context_id = context$id,
          error = conditionMessage(e)
        ),
        auto_unbox = TRUE, pretty = FALSE
      )
    }
  )

  result
}


#' Read Log Entries from Tail (End)
#'
#' @description
#' Read and parse log file contents from the end of the log file.
#' Returns structured log entries with level, time, caller, and content.
#' This tool is useful for reviewing recent workflow activity or
#' debugging the latest issues.
#'
#' @param max_lines Integer, maximum number of lines to read (default 300).
#' @param skip_lines Integer, number of lines to skip from the end
#'   (default 0). Use this for pagination when reading older entries.
#' @param pattern Character, optional regex pattern to filter log content.
#'   Only entries matching the pattern in their content will be returned.
#' @param levels Character vector, log levels to include. Defaults to all
#'   levels: \code{TRACE}, \code{DEBUG}, \code{INFO}, \code{WARN},
#'   \code{ERROR}, \code{FATAL}.
#'
#' @return A list containing:
#' \describe{
#'   \item{success}{Logical, whether the operation completed successfully}
#'   \item{context_id}{Character, the context identifier}
#'   \item{count}{Integer, number of log entries returned}
#'   \item{entries}{List of log entries, each with \code{line_no},
#'     \code{level}, \code{time}, \code{caller}, \code{content}}
#'   \item{error}{Character, error message if success is \code{FALSE}}
#' }
#'
#' @keywords mcp-tool mcp-category-info mcp-output-json
#' @noRd
mcp_tool_context_logs_tail <- function(
  max_lines = 300L,
  skip_lines = 0L,
  pattern = NULL,
  levels = c("TRACE", "DEBUG", "INFO", "WARN", "ERROR", "FATAL")
) {
  context <- get_globals("active_context")

  if (is.null(context)) {
    return(jsonlite::toJSON(
      list(
        success = FALSE,
        error = "No active context. Start a workflow execution first."
      ),
      auto_unbox = TRUE, pretty = FALSE
    ))
  }

  if (!R6::is.R6(context) || !inherits(context, "TricobblerAgentContext")) {
    return(jsonlite::toJSON(
      list(success = FALSE, error = "Invalid context object."),
      auto_unbox = TRUE, pretty = FALSE
    ))
  }

  result <- tryCatch(
    {
      logs <- context$read_logs(
        method = "tail",
        max_lines = max_lines,
        skip_lines = skip_lines,
        pattern = pattern,
        levels = levels
      )

      jsonlite::toJSON(
        list(
          success = TRUE,
          context_id = context$id,
          count = nrow(logs),
          entries = logs
        ),
        auto_unbox = TRUE, dataframe = "rows", pretty = FALSE
      )
    },
    error = function(e) {
      jsonlite::toJSON(
        list(
          success = FALSE,
          context_id = context$id,
          error = conditionMessage(e)
        ),
        auto_unbox = TRUE, pretty = FALSE
      )
    }
  )

  result
}


#' Search Log Entries by Pattern
#'
#' @description
#' Search log file contents using a regex pattern. Reads the entire log
#' (up to \code{max_lines}) and returns entries matching the pattern.
#' This tool is useful for finding specific events, errors, or agent
#' activities across the entire log history.
#'
#' @param pattern Character, regex pattern to search for in log content.
#'   This parameter is required.
#' @param max_lines Integer, maximum number of lines to search through
#'   (default 1000). Reads from the end of the log.
#' @param levels Character vector, log levels to include. Defaults to all
#'   levels: \code{TRACE}, \code{DEBUG}, \code{INFO}, \code{WARN},
#'   \code{ERROR}, \code{FATAL}.
#'
#' @return A list containing:
#' \describe{
#'   \item{success}{Logical, whether the operation completed successfully}
#'   \item{context_id}{Character, the context identifier}
#'   \item{pattern}{Character, the search pattern used}
#'   \item{count}{Integer, number of matching log entries}
#'   \item{entries}{List of matching log entries, each with \code{line_no},
#'     \code{level}, \code{time}, \code{caller}, \code{content}}
#'   \item{error}{Character, error message if success is \code{FALSE}}
#' }
#'
#' @keywords mcp-tool mcp-category-info mcp-output-json
#' @noRd
mcp_tool_context_logs_search <- function(
  pattern,
  max_lines = 1000L,
  levels = c("TRACE", "DEBUG", "INFO", "WARN", "ERROR", "FATAL")
) {
  # Validate pattern

if (missing(pattern) || !is.character(pattern) ||
        length(pattern) != 1 || !nzchar(pattern)) {
    return(jsonlite::toJSON(
      list(
        success = FALSE,
        error = "pattern parameter is required and must be a non-empty string" # nolint: line_length_linter.
      ),
      auto_unbox = TRUE, pretty = FALSE
    ))
  }

  context <- get_globals("active_context")

  if (is.null(context)) {
    return(jsonlite::toJSON(
      list(
        success = FALSE,
        error = "No active context. Start a workflow execution first."
      ),
      auto_unbox = TRUE, pretty = FALSE
    ))
  }

  if (!R6::is.R6(context) || !inherits(context, "TricobblerAgentContext")) {
    return(jsonlite::toJSON(
      list(success = FALSE, error = "Invalid context object."),
      auto_unbox = TRUE, pretty = FALSE
    ))
  }

  result <- tryCatch(
    {
      logs <- context$read_logs(
        method = "tail",
        max_lines = max_lines,
        skip_lines = 0L,
        pattern = pattern,
        levels = levels
      )

      jsonlite::toJSON(
        list(
          success = TRUE,
          context_id = context$id,
          pattern = pattern,
          count = nrow(logs),
          entries = logs
        ),
        auto_unbox = TRUE, dataframe = "rows", pretty = FALSE
      )
    },
    error = function(e) {
      jsonlite::toJSON(
        list(
          success = FALSE, context_id = context$id, pattern = pattern,
          error = conditionMessage(e)
        ),
        auto_unbox = TRUE, pretty = FALSE
      )
    }
  )

  result
}


# --------------------------------------------------------------------------
# MCP Tools for Context Attachment Access
# --------------------------------------------------------------------------

#' List All Attachments in Context
#'
#' @description
#' List all attachments (recorded results) in the current workflow context.
#' Returns metadata for each attachment including stage, state, agent ID,
#' attempt number, and the attachment identifier needed for retrieval.
#'
#' @param reindex Logical, whether to reload the index from disk
#'   (default \code{FALSE}). Set to \code{TRUE} if attachments may have been
#'   modified externally.
#'
#' @return A list containing:
#' \describe{
#'   \item{success}{Logical, whether the operation completed successfully}
#'   \item{context_id}{Character, the context identifier}
#'   \item{count}{Integer, number of attachments found}
#'   \item{attachments}{List of attachment metadata, each with \code{stage},
#'     \code{state}, \code{agent_id}, \code{current_attempt}, \code{filename}}
#'   \item{error}{Character, error message if success is \code{FALSE}}
#' }
#'
#' @keywords mcp-tool mcp-category-info mcp-output-json
#' @noRd
mcp_tool_context_attachment_list <- function(reindex = FALSE) {
  context <- get_globals("active_context")

  if (is.null(context)) {
    return(jsonlite::toJSON(
      list(
        success = FALSE,
        error = "No active context. Start a workflow execution first."
      ),
      auto_unbox = TRUE, pretty = FALSE
    ))
  }

  if (!R6::is.R6(context) || !inherits(context, "TricobblerAgentContext")) {
    return(jsonlite::toJSON(
      list(success = FALSE, error = "Invalid context object."),
      auto_unbox = TRUE, pretty = FALSE
    ))
  }

  result <- tryCatch(
    {
      attachments <- context$list_attachments(reindex = isTRUE(reindex))

      if (is.data.frame(attachments) && nrow(attachments) > 0) {
        count <- nrow(attachments)
      } else {
        attachments <- data.frame(
          stage = character(0),
          state = character(0),
          agent_id = character(0),
          current_attempt = integer(0),
          filename = character(0),
          stringsAsFactors = FALSE
        )
        count <- 0L
      }

      jsonlite::toJSON(
        list(
          success = TRUE,
          context_id = context$id,
          count = count,
          attachments = attachments
        ),
        auto_unbox = TRUE, dataframe = "rows", pretty = FALSE
      )
    },
    error = function(e) {
      jsonlite::toJSON(
        list(
          success = FALSE,
          context_id = context$id,
          error = conditionMessage(e)
        ),
        auto_unbox = TRUE, pretty = FALSE
      )
    }
  )

  result
}


#' Check if Attachment Exists
#'
#' @description
#' Check whether a specific attachment exists in the current workflow context.
#' This is a lightweight check that validates the identifier format and
#' verifies the file exists on disk without loading the attachment data.
#'
#' @param attachment_id Character, the attachment identifier. This is the
#'   \code{filename} value from \code{mcp_tool_context_attachment_list} or
#'   the \code{identifier} logged when a result is recorded. Format:
#'   \code{[stage][state][agent_id]_YYMMDDTHHMMSS_attempt}.
#'
#' @return A list containing:
#' \describe{
#'   \item{success}{Logical, whether the operation completed successfully}
#'   \item{context_id}{Character, the context identifier}
#'   \item{attachment_id}{Character, the queried attachment identifier}
#'   \item{exists}{Logical, whether the attachment exists}
#'   \item{error}{Character, error message if success is \code{FALSE}}
#' }
#'
#' @keywords mcp-tool mcp-category-info mcp-output-json
#' @noRd
mcp_tool_context_attachment_exists <- function(attachment_id) {
  # Validate attachment_id
  if (missing(attachment_id) || !is.character(attachment_id) ||
      length(attachment_id) != 1 || is.na(attachment_id) ||
      !nzchar(attachment_id)) {
    return(jsonlite::toJSON(
      list(
        success = FALSE,
        error = "attachment_id is required and must be a non-empty string"
      ),
      auto_unbox = TRUE, pretty = FALSE
    ))
  }

  context <- get_globals("active_context")

  if (is.null(context)) {
    return(jsonlite::toJSON(
      list(
        success = FALSE,
        error = "No active context. Start a workflow execution first."
      ),
      auto_unbox = TRUE, pretty = FALSE
    ))
  }

  if (!R6::is.R6(context) || !inherits(context, "TricobblerAgentContext")) {
    return(jsonlite::toJSON(
      list(success = FALSE, error = "Invalid context object."),
      auto_unbox = TRUE, pretty = FALSE
    ))
  }

  result <- tryCatch(
    {
      exists <- context$has_attachment(attachment_id)

      jsonlite::toJSON(
        list(
          success = TRUE,
          context_id = context$id,
          attachment_id = attachment_id,
          exists = exists
        ),
        auto_unbox = TRUE, pretty = FALSE
      )
    },
    error = function(e) {
      jsonlite::toJSON(
        list(
          success = FALSE,
          context_id = context$id,
          attachment_id = attachment_id,
          error = conditionMessage(e)
        ),
        auto_unbox = TRUE, pretty = FALSE
      )
    }
  )

  result
}


#' Get Attachment by Identifier
#'
#' @description
#' Retrieve a specific attachment (recorded result) from the current workflow
#' context. Returns the full attachment data including the result object,
#' description, and metadata.
#'
#' @param attachment_id Character, the attachment identifier. This is the
#'   \code{filename} value from \code{mcp_tool_context_attachment_list} or
#'   the \code{identifier} logged when a result is recorded. Format:
#'   \verb{[stage][state][agent_id]_YYMMDDTHHMMSS_attempt}.
#'
#' @return A list containing:
#' \describe{
#'   \item{success}{Logical, whether the operation completed successfully}
#'   \item{context_id}{Character, the context identifier}
#'   \item{attachment_id}{Character, the queried attachment identifier}
#'   \item{attachment}{List, the attachment data including \code{result},
#'     \code{description}, \code{stage}, \code{state}, \code{agent_id},
#'     \code{current_attempt}, and \code{._timestamp}}
#'   \item{error}{Character, error message if success is \code{FALSE}}
#' }
#'
#' @keywords mcp-tool mcp-category-info mcp-output-json
#' @noRd
mcp_tool_context_attachment_get <- function(attachment_id) {
  # Validate attachment_id
  if (missing(attachment_id) || !is.character(attachment_id) ||
      length(attachment_id) != 1 || is.na(attachment_id) ||
      !nzchar(attachment_id)) {
    return(jsonlite::toJSON(
      list(
        success = FALSE,
        error = "attachment_id is required and must be a non-empty string"
      ),
      auto_unbox = TRUE, pretty = FALSE
    ))
  }

  context <- get_globals("active_context")

  if (is.null(context)) {
    return(jsonlite::toJSON(
      list(
        success = FALSE,
        error = "No active context. Start a workflow execution first."
      ),
      auto_unbox = TRUE, pretty = FALSE
    ))
  }

  if (!R6::is.R6(context) || !inherits(context, "TricobblerAgentContext")) {
    return(jsonlite::toJSON(
      list(success = FALSE, error = "Invalid context object."),
      auto_unbox = TRUE, pretty = FALSE
    ))
  }

  result <- tryCatch(
    {
      attachment <- context$get_attachment(attachment_id)

      # Convert timestamp to ISO format for JSON
      if (!is.null(attachment$`._timestamp`)) {
        attachment$`._timestamp` <- format(
          attachment$`._timestamp`,
          "%Y-%m-%dT%H:%M:%S"
        )
      }

      jsonlite::toJSON(
        list(
          success = TRUE,
          context_id = context$id,
          attachment_id = attachment_id,
          attachment = attachment
        ),
        auto_unbox = TRUE, pretty = FALSE
      )
    },
    error = function(e) {
      jsonlite::toJSON(
        list(
          success = FALSE,
          context_id = context$id,
          attachment_id = attachment_id,
          error = conditionMessage(e)
        ),
        auto_unbox = TRUE, pretty = FALSE
      )
    }
  )

  result
}
