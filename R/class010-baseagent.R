#' @title `TriCobbler` Base Agent Class
#'
#' @description
#' An abstract R6 class representing a single agent within the `TriCobbler`
#' framework. It acts as a wrapper around an `ellmer` chat object, providing
#' standardized methods for interaction, state management, and context
#' window monitoring.
#'
#' @details
#' The `BaseAgent` class is designed to be the foundation for the "TriCobbler Trinity":
#' \itemize{
#'   \item \code{Planner}: Manages task decomposition.
#'   \item \code{Worker}: Executes specific sub-tasks.
#'   \item \code{Librarian}: Manages long-term memory and context sweeping.
#' }
#'
#' @field name An optional identifier for the agent instance.
#'
#' @export
BaseAgent <- R6::R6Class(
  classname = "TriCobblerBaseAgent",

  private = list(
    .chat = NULL,           # The underlying ellmer::Chat object
    .role = character(),    # e.g., "Analyst", "Coder", "Librarian"
    .context_window = integer() # The maximum token capacity for the model
  ),

  public = list(

    #' @description
    #' Initialize a new `TriCobbler` Agent.
    #' @param role A string defining the agent's purpose.
    #' @param chat An initialized \code{ellmer::Chat} object.
    #' @param context_window Integer. The token limit for the model in use (default 4000).
    initialize = function(role, chat, context_window = 4000L) {
      private$.context_window <- as.integer(context_window)
      stopifnot("Context window must be > 0" = isTRUE(private$.context_window > 0))

      private$.role <- role
      self$set_chat_object(chat)
    },

    #' @description
    #' Send a prompt to the agent and receive a response.
    #' @param prompt The string message to send to the LLM.
    #' @param ... Additional arguments passed to \code{ellmer::chat$chat()}.
    #' @param echo Control the visibility of the stream ("all", "none", or "output").
    chat = function(prompt, ..., echo = c("all", "none", "output")) {
      echo <- match.arg(echo)
      private$.chat$chat(prompt, ..., echo = echo)
    },

    #' @description
    #' Calculate the current context window usage.
    #' @return A numeric value between 0 and 1 representing the percentage of
    #' the context window currently occupied by the chat history.
    get_load = function() {
      tokens <- private$.chat$get_tokens()
      if (is.null(tokens) || length(tokens) == 0 || nrow(tokens) == 0) {
        return(0)
      }
      used <- sum(tokens$input, na.rm = TRUE) + sum(tokens$output, na.rm = TRUE)
      return(used / private$.context_window)
    },

    #' @description
    #' Replace or update the internal chat object.
    #' @param chat A new \code{ellmer::Chat} object.
    #' @param copy_turns Logical. If TRUE, transfers conversation history from
    #' the old chat object to the new one.
    #' @return The agent instance (invisible).
    set_chat_object = function(chat, copy_turns = TRUE) {
      stopifnot(R6::is.R6(chat) && inherits(chat, "Chat"))

      if (copy_turns && !is.null(private$.chat)) {
        turns <- private$.chat$get_turns()
        chat$set_turns(turns)
      }

      private$.chat <- chat
      invisible(self)
    }
  ),

  active = list(
    #' @field role The assigned role of the agent (read-only).
    role = function() { return(private$.role) },

    #' @field model The name of the underlying LLM model (read-only).
    model = function() { return(private$.chat$get_model()) },

    #' @field chat_object Access or update the internal ellmer chat object.
    chat_object = function(value) {
      if (!missing(value)) self$set_chat_object(value)
      return(private$.chat)
    },

    #' @field turns Get or set the full list of conversation turns.
    #' Primarily used by the `Librarian` for history management.
    turns = function(value) {
      if (!missing(value)) private$.chat$set_turns(value)
      return(private$.chat$get_turns())
    }
  )
)
