#' @include aaa.R
NULL

#' Attachment Index for Workflow Result Tracking
#'
#' @description R6 class that manages the attachment index for workflow
#'   execution. Backed by \pkg{RSQLite}, this class provides atomic,
#'   query-friendly tracking of attachment lifecycle status. All database
#'   access uses open/close-per-operation pattern for safety.
#'
#' @details
#' The index tracks each attachment through its lifecycle:
#' \describe{
#'   \item{init}{Runtime created, execution not yet started}
#'   \item{running}{Agent execution in progress}
#'   \item{finished}{Execution completed successfully}
#'   \item{errored}{Execution completed with error}
#'   \item{skipped}{Execution was skipped (reserved for future use)}
#' }
#'
#' ## Backend Abstraction
#'
#' All \code{SQLite} calls are isolated behind the private
#' \code{$.with_db()} method. To swap backends (e.g., \pkg{duckdb}),
#' only \code{$.with_db()} and \code{$.init_db()} need modification.
#'
#' @export
AttachmentIndex <- R6::R6Class(
  classname = "TricobblerAttachmentIndex",
  portable = TRUE,
  cloneable = FALSE,
  private = list(
    .db_path = NULL,

    # ---- Backend abstraction layer ----------------------------------------

    .with_db = function(callback) {
      con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = private$.db_path)
      on.exit(RSQLite::dbDisconnect(con), add = TRUE)
      callback(con)
    },

    .init_db = function() {
      private$.with_db(function(con) {
        RSQLite::dbExecute(con, "
          CREATE TABLE IF NOT EXISTS attachment_index (
            attachment_id TEXT PRIMARY KEY,
            stage         TEXT NOT NULL,
            state         TEXT NOT NULL,
            agent_id      TEXT NOT NULL,
            attempt       INTEGER NOT NULL DEFAULT 0,
            status        TEXT NOT NULL DEFAULT 'init',
            succeed       INTEGER,
            created_at    REAL NOT NULL,
            updated_at    REAL NOT NULL
          )
        ")
        # Index for common queries
        RSQLite::dbExecute(con, "
          CREATE INDEX IF NOT EXISTS idx_state_stage
          ON attachment_index (state, stage)
        ")
        RSQLite::dbExecute(con, "
          CREATE INDEX IF NOT EXISTS idx_status
          ON attachment_index (status)
        ")
      })
    }
  ),
  public = list(

    #' @description Initialize the attachment index
    #' @param db_path character, path to the SQLite database file
    initialize = function(db_path) {
      stopifnot(
        is.character(db_path),
        length(db_path) == 1L,
        nzchar(db_path)
      )
      db_dir <- dirname(db_path)
      if (!dir.exists(db_dir)) {
        dir.create(db_dir, recursive = TRUE, showWarnings = FALSE)
      }
      private$.db_path <- db_path
      private$.init_db()
      invisible(self)
    },

    #' @description Register a new attachment entry with status 'init'
    #' @param attachment_id character, unique attachment identifier
    #' @param stage character, stage name
    #' @param state character, state name
    #' @param agent_id character, agent identifier
    #' @param attempt integer, attempt number
    register = function(attachment_id, stage, state, agent_id, attempt = 0L) {
      now <- as.numeric(Sys.time())
      private$.with_db(function(con) {
        RSQLite::dbExecute(con, "
          INSERT OR REPLACE INTO attachment_index
            (attachment_id, stage, state, agent_id, attempt,
             status, succeed, created_at, updated_at)
          VALUES (?, ?, ?, ?, ?, 'init', NULL, ?, ?)
        ", params = list(
          attachment_id, stage, state, agent_id,
          as.integer(attempt), now, now
        ))
      })
      invisible(self)
    },

    #' @description Update the status of an existing attachment
    #' @param attachment_id character, attachment identifier
    #' @param status character, new status
    update_status = function(attachment_id, status) {
      valid_statuses <- c("init", "running", "finished", "errored", "skipped")
      status <- match.arg(status, valid_statuses)
      now <- as.numeric(Sys.time())
      private$.with_db(function(con) {
        RSQLite::dbExecute(con, "
          UPDATE attachment_index
          SET status = ?, updated_at = ?
          WHERE attachment_id = ?
        ", params = list(status, now, attachment_id))
      })
      invisible(self)
    },

    #' @description Mark an attachment as finished or errored
    #' @param attachment_id character, attachment identifier
    #' @param succeed logical, whether execution succeeded
    mark_finished = function(attachment_id, succeed) {
      status <- if (isTRUE(succeed)) "finished" else "errored"
      now <- as.numeric(Sys.time())
      private$.with_db(function(con) {
        RSQLite::dbExecute(con, "
          UPDATE attachment_index
          SET status = ?, succeed = ?, updated_at = ?
          WHERE attachment_id = ?
        ", params = list(status, as.integer(isTRUE(succeed)), now,
                         attachment_id))
      })
      invisible(self)
    },

    #' @description Retrieve a single index entry
    #' @param attachment_id character, attachment identifier
    #' @return A single-row data.frame or \code{NULL} if not found
    get = function(attachment_id) {
      private$.with_db(function(con) {
        row <- RSQLite::dbGetQuery(con, "
          SELECT * FROM attachment_index WHERE attachment_id = ?
        ", params = list(attachment_id))
        if (nrow(row) == 0L) {
          return(NULL)
        }
        row$succeed <- as.logical(row$succeed)
        row
      })
    },

    #' @description List all index entries
    #' @param status character or \code{NULL}, optional status filter
    #' @return A data.frame of all matching entries (most recent first)
    list = function(status = NULL) {
      private$.with_db(function(con) {
        if (!is.null(status)) {
          tbl <- RSQLite::dbGetQuery(con, "
            SELECT * FROM attachment_index
            WHERE status = ?
            ORDER BY created_at DESC
          ", params = list(status))
        } else {
          tbl <- RSQLite::dbGetQuery(con, "
            SELECT * FROM attachment_index
            ORDER BY created_at DESC
          ")
        }
        if (nrow(tbl) > 0L) {
          tbl$succeed <- as.logical(tbl$succeed)
        }
        tbl
      })
    },

    #' @description Query index entries by state and/or stage
    #' @param state character, state name to filter by
    #' @param stage character or \code{NULL}, optional stage filter
    #' @param status character or \code{NULL}, optional status filter
    #' @return A data.frame of matching entries (most recent first)
    query = function(state, stage = NULL, status = NULL) {
      private$.with_db(function(con) {
        sql <- "SELECT * FROM attachment_index WHERE state = ?"
        params <- list(state)
        if (!is.null(stage)) {
          sql <- paste(sql, "AND stage = ?")
          params <- c(params, list(stage))
        }
        if (!is.null(status)) {
          sql <- paste(sql, "AND status = ?")
          params <- c(params, list(status))
        }
        sql <- paste(sql, "ORDER BY created_at DESC")
        tbl <- RSQLite::dbGetQuery(con, sql, params = params)
        if (nrow(tbl) > 0L) {
          tbl$succeed <- as.logical(tbl$succeed)
        }
        tbl
      })
    },

    #' @description Find incomplete entries (init or running past timeout)
    #' @param timeout_secs numeric, seconds after which init/running entries
    #'   are considered incomplete. If \code{NULL} (default), returns all
    #'   entries with status 'init' or 'running' regardless of age.
    #' @return A data.frame of incomplete entries
    list_incomplete = function(timeout_secs = NULL) {
      private$.with_db(function(con) {
        if (is.null(timeout_secs)) {
          tbl <- RSQLite::dbGetQuery(con, "
            SELECT * FROM attachment_index
            WHERE status IN ('init', 'running')
            ORDER BY created_at DESC
          ")
        } else {
          cutoff <- as.numeric(Sys.time()) - as.numeric(timeout_secs)
          tbl <- RSQLite::dbGetQuery(con, "
            SELECT * FROM attachment_index
            WHERE status IN ('init', 'running')
              AND updated_at < ?
            ORDER BY created_at DESC
          ", params = list(cutoff))
        }
        if (nrow(tbl) > 0L) {
          tbl$succeed <- as.logical(tbl$succeed)
        }
        tbl
      })
    },

    #' @description Check if an attachment exists in the index
    #' @param attachment_id character, attachment identifier
    #' @return logical
    exists = function(attachment_id) {
      private$.with_db(function(con) {
        row <- RSQLite::dbGetQuery(con, "
          SELECT 1 FROM attachment_index WHERE attachment_id = ? LIMIT 1
        ", params = list(attachment_id))
        nrow(row) > 0L
      })
    },

    #' @description Get the database file path
    #' @return character, path to the SQLite database
    get_db_path = function() {
      private$.db_path
    }
  )
)
