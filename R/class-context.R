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
    .index = NULL,

    .is_default = FALSE,

    .cache = NULL,

    finalize = function() {
      if (!is.null(private$.cache)) {
        private$.cache$reset()
      }
    }
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

    #' @field memory_path character, path to shared memory directory,
    #' used to store agent memories across the sessions (read-only)
    memory_path = function() {
      file.path(
        tools::R_user_dir("tricobbler", which = "cache"),
        "context",
        "memory"
      )
    },

    #' @field is_default_context logical, whether this is the default context
    #' (read-only)
    is_default_context = function() {
      private$.is_default
    },

    #' @field cache map, stores and persists temporary data
    cache = function() {
      private$.cache
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
    },

    #' @field index AttachmentIndex, the attachment index (read-only).
    #'   Available after \code{init_resources()} is called.
    index = function() {
      private$.index
    }
  ),
  public = list(

    #' @field debug logical, whether to print out agent calls
    debug = FALSE,

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
    initialize = function(
      id = NULL,
      path = file.path(tempdir(), "tricobbler", "context")
    ) {
      private$.rootpath <- path
      if (length(id) > 0) {
        private$.id <- id
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
        )
        private$.id <- sprintf("%s-%s", format(Sys.time(), "%y%m%dT%H%M%S"), dg)
      }

      private$.cache <- fastmap::fastmap()
      private$.index <- NULL

      if (!dir.exists(path)) {
        dir.create(path, recursive = TRUE)
        path <- normalizePath(path, mustWork = TRUE)
      }
      # set again with updated path
      private$.rootpath <- path

      # check if this context is a default context
      default_path <- normalizePath(
        file.path(tempdir(), "tricobbler", "context"),
        mustWork = FALSE
      )
      if (length(id) == 1 && isTRUE(id == "default") && path == default_path) {
        private$.is_default <- TRUE
      }
    },

    #' @description Create directory structure and initialize logging
    init_resources = function() {
      dir.create(self$memory_path, showWarnings = FALSE, recursive = TRUE)
      dir.create(self$store_path, showWarnings = FALSE, recursive = TRUE)
      dir.create(self$attachment_path, showWarnings = FALSE, recursive = TRUE)
      file.create(self$logger_path, showWarnings = FALSE)

      # Initialize attachment index (SQLite-backed)
      db_path <- file.path(self$attachment_path, "index.sqlite")
      private$.index <- AttachmentIndex$new(db_path)

      invisible(self)
    },

    #' @description Write time-stamped messages to log file
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
      caller,
      level = c("INFO", "TRACE", "DEBUG", "WARN", "ERROR", "FATAL"),
      verbose = c("cli", "base", "none")
    ) {
      force(caller)
      if (identical(caller, private$.scheduler)) {
        role <- "Scheduler"
        level <- "TRACE"
      } else if (identical(caller, self)) {
        role <- "Context"
        if (length(level) > 1) {
          level <- match.arg(level)
        }
      } else if (inherits(caller, "TricobblerAgentRuntime")) {
        role <- sprintf("Runtime %s", caller$id)
        level <- match.arg(level)
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

      log_to_file(..., path = self$logger_path, role = role, level = level, verbose = verbose)
    },

    record_attachment = function(runtime, succeed) {
      private$.index$mark_finished(
        attachment_id = runtime$attachment_id,
        succeed = succeed
      )

      self$logger(
        sprintf(
          "Attachment recorded, identifier=%s, status=%s",
          runtime$attachment_id, if (succeed) "finished" else "errored"
        ),
        caller = self,
        level = "TRACE"
      )

      invisible(self)
    },


    #' @description Retrieve the most recent result(s)
    #' @param items integer, number of results to retrieve
    #' @param simplify logical, return single result if items == 1
    last_results = function(items = 1, simplify = length(items) == 1) {
      idx <- private$.index$list()
      if (!is.data.frame(idx) || !nrow(idx)) {
        return(NULL)
      }
      n <- nrow(idx)
      items <- min(items, n)
      fnames <- idx$attachment_id[seq_len(items)]
      attachment_root <- self$attachment_path
      ret <- lapply(fnames, function(fname) {
        fpath <- file.path(attachment_root, paste0(fname, ".rds"))
        if (file.exists(fpath)) {
          return(readRDS(fpath))
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
    #'    (filename from record_result). Or a \code{StatePolicy} object to
    #'    retrieve the latest attachment for that state.
    get_attachment = function(attachment_id) {
      if (missing(attachment_id)) {
        stop("AgentContext$get_attachment(): `attachment_id` is required")
      }

      if (S7::S7_inherits(attachment_id, StatePolicy)) {
        return(self$get_attachment_by_state(
          state = attachment_id@name,
          stage = attachment_id@stage
        ))
      }

      if (
        !is.character(attachment_id) ||
          length(attachment_id) != 1 ||
          is.na(attachment_id) ||
          attachment_id == ""
      ) {
        stop(
          "AgentContext$get_attachment(): `attachment_id` must be a single non-empty character string" # nolint: line_length_linter.
        )
      }

      pattern <- "^\\[[^]]+\\]\\[[^]]+\\]\\[[^]]+\\]_[0-9]{6}T[0-9]{6}_[0-9]+$"
      if (!grepl(pattern, attachment_id)) {
        stop(
          "AgentContext$get_attachment(): Invalid identifier format. ",
          "Expected [stage][state][agent_id]_YYMMDDTHHMMSS_{attempt #}"
        )
      }

      fpath <- file.path(self$attachment_path, paste0(attachment_id, ".rds"))
      if (file.exists(fpath)) {
        return(readRDS(fpath))
      }

      in_history <- private$.index$exists(attachment_id)
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

    #' @description Retrieve the latest attachment from a specific state
    #' @param state character, the name of the state
    #' @param stage character, optional stage name to narrow down the search
    get_attachment_by_state = function(state, stage) {
      if (missing(state) || length(state) != 1 || !nzchar(state)) {
        stop("AgentContext$get_attachment_by_state(): `state` is required.")
      }
      if (missing(stage) || length(stage) != 1 || !nzchar(stage)) {
        stop("AgentContext$get_attachment_by_state(): `stage` is required for data privacy/integrity.") # nolint: line_length_linter.
      }

      matches <- private$.index$query(state = state, stage = stage)

      if (!is.data.frame(matches) || nrow(matches) == 0L) {
        return(NULL)
      }

      # Query returns most-recent first
      fname <- matches$attachment_id[[1L]]

      fpath <- file.path(self$attachment_path, paste0(fname, ".rds"))
      if (file.exists(fpath)) {
        return(readRDS(fpath))
      }

      stop(
        sprintf(
          "Attachment for state '%s' (ID: %s) is recorded but file is missing.",
          state, fname
        )
      )
    },

    #' @description List all available attachments
    #' @param reindex logical, currently unused (index is always live);
    #'   kept for backward compatibility
    list_attachments = function(reindex = FALSE) {
      tbl <- private$.index$list()
      if (!is.data.frame(tbl) || nrow(tbl) == 0L) {
        return(character(0L))
      }
      tbl
    },

    #' @description Check if an attachment exists
    #' @param attachment_id character, the attachment identifier
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
      fpath <- file.path(self$attachment_path, paste0(attachment_id, ".rds"))
      file.exists(fpath)
    },

    #' @description Find incomplete executions (crashed or in-progress)
    #' @param timeout_secs numeric or \code{NULL}, seconds after which
    #'   init/running entries are considered incomplete. If \code{NULL},
    #'   returns all init/running entries.
    #' @return A data.frame of incomplete index entries
    list_incomplete = function(timeout_secs = NULL) {
      private$.index$list_incomplete(timeout_secs)
    },

    #' @description Read a specific runtime's per-execution log file
    #' @param attachment_id character, the attachment identifier
    #'   (which is also the runtime's log file prefix)
    #' @return character vector of log lines, or \code{NULL} if not found
    get_runtime_log = function(attachment_id) {
      log_path <- file.path(
        self$attachment_path,
        paste0(attachment_id, ".log")
      )
      if (file.exists(log_path)) {
        readLines(log_path)
      } else {
        NULL
      }
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
        logger_path,
        what = character(),
        skip = skip,
        nlines = n_read,
        sep = "\n",
        quiet = TRUE,
        blank.lines.skip = FALSE,
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

default_context <- local({
  impl <- NULL
  init <- function() {
    if (is.null(impl)) {
      root_path <- file.path(tempdir(), "tricobbler", "context")
      default_id <- "default"
      store_path <- file.path(root_path, "conversations", default_id)
      logger_path <- file.path(store_path, "logger.log")
      attachment_path <- file.path(store_path, "attachments")

      if (!dir.exists(attachment_path)) {
        dir.create(attachment_path,
                   showWarnings = FALSE,
                   recursive = TRUE)
      }
      if (!file.exists(logger_path)) {
        file.create(logger_path, showWarnings = FALSE)
      }
      impl <<- AgentContext$new(id = default_id, path = root_path)
      impl$init_resources()
    }
    impl
  }
})

