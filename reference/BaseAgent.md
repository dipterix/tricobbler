# `TriCobbler` Base Agent Class

An abstract R6 class representing a single agent within the `TriCobbler`
framework. It acts as a wrapper around an `ellmer` chat object,
providing standardized methods for interaction, state management, and
context window monitoring.

## Details

The `BaseAgent` class is designed to be the foundation for the
"TriCobbler Trinity":

- `Planner`: Manages task decomposition.

- `Worker`: Executes specific sub-tasks.

- `Librarian`: Manages long-term memory and context sweeping.

## Active bindings

- `role`:

  The assigned role of the agent (read-only).

- `model`:

  The name of the underlying LLM model (read-only).

- `chat_object`:

  Access or update the internal ellmer chat object.

- `turns`:

  Get or set the full list of conversation turns. Primarily used by the
  `Librarian` for history management.

## Methods

### Public methods

- [`BaseAgent$new()`](#method-TriCobblerBaseAgent-new)

- [`BaseAgent$chat()`](#method-TriCobblerBaseAgent-chat)

- [`BaseAgent$get_load()`](#method-TriCobblerBaseAgent-get_load)

- [`BaseAgent$set_chat_object()`](#method-TriCobblerBaseAgent-set_chat_object)

- [`BaseAgent$clone()`](#method-TriCobblerBaseAgent-clone)

------------------------------------------------------------------------

### Method `new()`

Initialize a new `TriCobbler` Agent.

#### Usage

    BaseAgent$new(role, chat, context_window = 4000L)

#### Arguments

- `role`:

  A string defining the agent's description.

- `chat`:

  An initialized
  [`ellmer::Chat`](https://ellmer.tidyverse.org/reference/Chat.html)
  object.

- `context_window`:

  Integer. The token limit for the model in use (default 4000).

------------------------------------------------------------------------

### Method `chat()`

Send a prompt to the agent and receive a response.

#### Usage

    BaseAgent$chat(prompt, ..., echo = c("all", "none", "output"))

#### Arguments

- `prompt`:

  The string message to send to the LLM.

- `...`:

  Additional arguments passed to `ellmer::chat$chat()`.

- `echo`:

  Control the visibility of the stream ("all", "none", or "output").

------------------------------------------------------------------------

### Method `get_load()`

Calculate the current context window usage.

#### Usage

    BaseAgent$get_load()

#### Returns

A numeric value between 0 and 1 representing the percentage of the
context window currently occupied by the chat history.

------------------------------------------------------------------------

### Method `set_chat_object()`

Replace or update the internal chat object.

#### Usage

    BaseAgent$set_chat_object(chat, copy_turns = TRUE)

#### Arguments

- `chat`:

  A new
  [`ellmer::Chat`](https://ellmer.tidyverse.org/reference/Chat.html)
  object.

- `copy_turns`:

  Logical. If TRUE, transfers conversation history from the old chat
  object to the new one.

#### Returns

The agent instance (invisible).

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    BaseAgent$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
