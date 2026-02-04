# Execution Context for Workflow State Management

R6 class that manages the execution environment for workflow states.
Provides logging, result storage, and attachment management during
workflow execution. This is part of the Runtime Layer (Tier 2) alongside
`Scheduler`.

## Active bindings

- `id`:

  character, unique identifier for this context (read-only)

- `rootpath`:

  character, root directory for all context storage (read-only)

- `current_stage`:

  character, currently executing stage from scheduler (read-only)

- `current_state`:

  character, currently executing state from scheduler (read-only)

- `memory_path`:

  character, path to shared memory directory (read-only)

- `store_path`:

  character, path to this context's conversation directory (read-only)

- `logger_path`:

  character, path to the log file (read-only)

- `attachment_path`:

  character, path to attachments directory (read-only)

## Methods

### Public methods

- [`AgentContext$set_scheduler()`](#method-TricobblerAgentContext-set_scheduler)

- [`AgentContext$new()`](#method-TricobblerAgentContext-new)

- [`AgentContext$init_resources()`](#method-TricobblerAgentContext-init_resources)

- [`AgentContext$logger()`](#method-TricobblerAgentContext-logger)

- [`AgentContext$with_activated()`](#method-TricobblerAgentContext-with_activated)

- [`AgentContext$record_result()`](#method-TricobblerAgentContext-record_result)

- [`AgentContext$last_results()`](#method-TricobblerAgentContext-last_results)

- [`AgentContext$get_attachment()`](#method-TricobblerAgentContext-get_attachment)

- [`AgentContext$list_attachments()`](#method-TricobblerAgentContext-list_attachments)

- [`AgentContext$has_attachment()`](#method-TricobblerAgentContext-has_attachment)

- [`AgentContext$read_logs()`](#method-TricobblerAgentContext-read_logs)

------------------------------------------------------------------------

### Method `set_scheduler()`

Associate a scheduler with this context

#### Usage

    AgentContext$set_scheduler(scheduler)

#### Arguments

- `scheduler`:

  Scheduler object to associate

------------------------------------------------------------------------

### Method `new()`

Initialize a new context

#### Usage

    AgentContext$new(id = NULL, path = NULL)

#### Arguments

- `id`:

  character, unique identifier (NULL to auto-generate)

- `path`:

  character, root directory path (NULL for default cache)

------------------------------------------------------------------------

### Method `init_resources()`

Create directory structure and initialize logging

#### Usage

    AgentContext$init_resources()

------------------------------------------------------------------------

### Method `logger()`

Write timestamped messages to log file

#### Usage

    AgentContext$logger(
      ...,
      caller = get_globals("active_agent"),
      level = c("INFO", "WARN", "ERROR", "FATAL"),
      verbose = c("cli", "base", "none")
    )

#### Arguments

- `...`:

  character, message components to paste together

- `caller`:

  object, the caller (scheduler, context, or agent)

- `level`:

  character, log level (INFO, WARN, ERROR, FATAL)

- `verbose`:

  character or logical, whether to verbose the log results to the R
  console. When logical `TRUE`, it is equivalent to `'cli'` (the
  default), which uses the cli package for colored/styled console
  outputs. If cli is not installed, falls back to `'base'`. When logical
  `FALSE` or character `'none'`, the logs will not be printed out. Use
  `'base'` to force plain
  [`cat()`](https://rdrr.io/r/base/cat.html)/[`message()`](https://rdrr.io/r/base/message.html)
  output without styling.

------------------------------------------------------------------------

### Method `with_activated()`

Execute expression with this context activated

#### Usage

    AgentContext$with_activated(expr, quoted = FALSE, env = parent.frame())

#### Arguments

- `expr`:

  expression, the expression to evaluate with the context active

- `quoted`:

  logical, whether `expr` is already quoted

- `env`:

  environment in which to evaluate the expression

------------------------------------------------------------------------

### Method `record_result()`

Save agent output with metadata

#### Usage

    AgentContext$record_result(
      result,
      stage,
      state,
      agent_id,
      current_attempt,
      description,
      ...
    )

#### Arguments

- `result`:

  object, the result from agent execution

- `stage`:

  character, stage name

- `state`:

  character, state name

- `agent_id`:

  character, agent identifier

- `current_attempt`:

  integer, attempt number

- `description`:

  character, human-readable result description

- `...`:

  additional metadata to store

------------------------------------------------------------------------

### Method `last_results()`

Retrieve the most recent result(s)

#### Usage

    AgentContext$last_results(items = 1, simplify = length(items) == 1)

#### Arguments

- `items`:

  integer, number of results to retrieve

- `simplify`:

  logical, return single result if items == 1

------------------------------------------------------------------------

### Method `get_attachment()`

Retrieve a specific attachment by its identifier

#### Usage

    AgentContext$get_attachment(attachment_id)

#### Arguments

- `attachment_id`:

  character, the attachment identifier (filename from record_result)

------------------------------------------------------------------------

### Method `list_attachments()`

List all available attachments

#### Usage

    AgentContext$list_attachments(reindex = FALSE)

#### Arguments

- `reindex`:

  logical, whether to reload the index from disk (default FALSE)

------------------------------------------------------------------------

### Method `has_attachment()`

Check if an attachment exists

#### Usage

    AgentContext$has_attachment(attachment_id)

#### Arguments

- `attachment_id`:

  character, the attachment identifier (filename from record_result)

#### Returns

logical, TRUE if the attachment exists on disk, FALSE otherwise

------------------------------------------------------------------------

### Method `read_logs()`

Read and parse log file contents

#### Usage

    AgentContext$read_logs(
      method = c("tail", "head"),
      skip_lines = 0L,
      max_lines = 300L,
      pattern = NULL,
      levels = c("TRACE", "DEBUG", "INFO", "WARN", "ERROR", "FATAL")
    )

#### Arguments

- `method`:

  character, read from "tail" (end) or "head" (beginning)

- `skip_lines`:

  integer, skipping lines relative to the end or start; If `method` is
  `"head"`, then `skip` skips the first several lines, otherwise
  skipping the last several lines; default is `0L` (no skipping).

- `max_lines`:

  integer, maximum number of lines to read (default 300)

- `pattern`:

  character, optional regex pattern to filter log content

- `levels`:

  character, log levels to include (default: all levels)

#### Returns

data.frame with columns: line_no, level, time, caller, content. Returns
empty data.frame if file missing or no matches.
