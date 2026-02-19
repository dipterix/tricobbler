# Execution Context for Workflow State Management

R6 class that manages the execution environment for workflow states.
Provides logging, result storage, and attachment management during
workflow execution. This is part of the Runtime Layer (Tier 2) alongside
[`Scheduler`](http://dipterix.org/tricobbler/reference/Scheduler.md).

## Public fields

- `debug`:

  logical, whether to print out agent calls

## Active bindings

- `id`:

  character, unique identifier for this context (read-only)

- `rootpath`:

  character, root directory for all context storage (read-only)

- `current_stage`:

  character, currently executing stage from the scheduler (read-only)

- `current_state`:

  character, currently executing state from the scheduler (read-only)

- `memory_path`:

  character, path to shared memory directory, used to store agent
  memories across the sessions (read-only)

- `is_default_context`:

  logical, whether this is the default context (read-only)

- `cache`:

  [`fastmap::fastmap()`](https://r-lib.github.io/fastmap/reference/fastmap.html)
  object, stores and persists temporary data

- `store_path`:

  character, path to this context's conversation directory (read-only)

- `logger_path`:

  character, path to the log file (read-only)

- `attachment_path`:

  character, path to attachments directory (read-only)

- `index`:

  [`AttachmentIndex`](http://dipterix.org/tricobbler/reference/AttachmentIndex.md),
  the attachment index (read-only). Available after `init_resources()`
  is called.

## Methods

### Public methods

- [`AgentContext$set_scheduler()`](#method-TricobblerAgentContext-set_scheduler)

- [`AgentContext$new()`](#method-TricobblerAgentContext-new)

- [`AgentContext$init_resources()`](#method-TricobblerAgentContext-init_resources)

- [`AgentContext$logger()`](#method-TricobblerAgentContext-logger)

- [`AgentContext$record_attachment()`](#method-TricobblerAgentContext-record_attachment)

- [`AgentContext$last_results()`](#method-TricobblerAgentContext-last_results)

- [`AgentContext$get_attachment()`](#method-TricobblerAgentContext-get_attachment)

- [`AgentContext$get_attachment_by_state()`](#method-TricobblerAgentContext-get_attachment_by_state)

- [`AgentContext$list_attachments()`](#method-TricobblerAgentContext-list_attachments)

- [`AgentContext$has_attachment()`](#method-TricobblerAgentContext-has_attachment)

- [`AgentContext$list_incomplete()`](#method-TricobblerAgentContext-list_incomplete)

- [`AgentContext$get_runtime_log()`](#method-TricobblerAgentContext-get_runtime_log)

- [`AgentContext$read_logs()`](#method-TricobblerAgentContext-read_logs)

- [`AgentContext$get_tools()`](#method-TricobblerAgentContext-get_tools)

- [`AgentContext$set_tool()`](#method-TricobblerAgentContext-set_tool)

- [`AgentContext$clear_tools()`](#method-TricobblerAgentContext-clear_tools)

- [`AgentContext$has_tools()`](#method-TricobblerAgentContext-has_tools)

------------------------------------------------------------------------

### Method `set_scheduler()`

Associate a scheduler with this context

#### Usage

    AgentContext$set_scheduler(scheduler)

#### Arguments

- `scheduler`:

  [`Scheduler`](http://dipterix.org/tricobbler/reference/Scheduler.md)
  object to associate

------------------------------------------------------------------------

### Method `new()`

Initialize a new context

#### Usage

    AgentContext$new(
      id = NULL,
      path = file.path(tempdir(), "tricobbler", "context")
    )

#### Arguments

- `id`:

  character, unique identifier (`NULL` to auto-generate)

- `path`:

  character, root directory path (defaults to a temporary directory
  under [`tempdir()`](https://rdrr.io/r/base/tempfile.html))

------------------------------------------------------------------------

### Method `init_resources()`

Create directory structure and initialize logging

#### Usage

    AgentContext$init_resources(debug = FALSE)

#### Arguments

- `debug`:

  logical, whether to enable verbose agent-call output

------------------------------------------------------------------------

### Method `logger()`

Write time-stamped messages to log file

#### Usage

    AgentContext$logger(
      ...,
      caller,
      level = c("INFO", "TRACE", "DEBUG", "WARN", "ERROR", "FATAL"),
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

### Method `record_attachment()`

Record an attachment result from a completed runtime

#### Usage

    AgentContext$record_attachment(runtime, succeed)

#### Arguments

- `runtime`:

  [`AgentRuntime`](http://dipterix.org/tricobbler/reference/AgentRuntime.md)
  object that produced the attachment

- `succeed`:

  logical, whether the agent execution succeeded

------------------------------------------------------------------------

### Method `last_results()`

Retrieve the most recent result(s)

#### Usage

    AgentContext$last_results(items = 1, simplify = length(items) == 1)

#### Arguments

- `items`:

  integer, number of results to retrieve

- `simplify`:

  logical, if `TRUE` and `items == 1`, return a single result instead of
  a list

------------------------------------------------------------------------

### Method `get_attachment()`

Retrieve a specific attachment by its identifier

#### Usage

    AgentContext$get_attachment(attachment_id)

#### Arguments

- `attachment_id`:

  character, the attachment identifier (filename from
  `record_attachment`). Alternatively, a
  [`StatePolicy`](http://dipterix.org/tricobbler/reference/StatePolicy.md)
  object to retrieve the latest attachment for that state.

------------------------------------------------------------------------

### Method `get_attachment_by_state()`

Retrieve the latest attachment from a specific state

#### Usage

    AgentContext$get_attachment_by_state(state, stage)

#### Arguments

- `state`:

  character, the name of the state

- `stage`:

  character, optional stage name to narrow down the search

------------------------------------------------------------------------

### Method `list_attachments()`

List all available attachments

#### Usage

    AgentContext$list_attachments(reindex = FALSE)

#### Arguments

- `reindex`:

  logical, currently unused (index is always live); kept for backward
  compatibility

------------------------------------------------------------------------

### Method `has_attachment()`

Check if an attachment exists

#### Usage

    AgentContext$has_attachment(attachment_id)

#### Arguments

- `attachment_id`:

  character, the attachment identifier

#### Returns

logical, TRUE if the attachment exists on disk, FALSE otherwise

------------------------------------------------------------------------

### Method `list_incomplete()`

Find incomplete executions (crashed or in-progress)

#### Usage

    AgentContext$list_incomplete(timeout_secs = NULL)

#### Arguments

- `timeout_secs`:

  numeric or `NULL`, seconds after which `"init"`/`"running"` entries
  are considered incomplete. If `NULL`, returns all `"init"`/`"running"`
  entries.

#### Returns

A `data.frame` of incomplete index entries

------------------------------------------------------------------------

### Method `get_runtime_log()`

Read the per-execution log file for a specific runtime

#### Usage

    AgentContext$get_runtime_log(attachment_id)

#### Arguments

- `attachment_id`:

  character, the attachment identifier (which is also the log file
  prefix for the runtime)

#### Returns

character vector of log lines, or `NULL` if not found

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

  character, read from `"tail"` (end of file) or `"head"` (beginning of
  file)

- `skip_lines`:

  integer, skipping lines relative to the end or start; If `method` is
  `"head"`, then `skip_lines` skips the first several lines, otherwise
  skipping the last several lines; default is `0L` (no skipping).

- `max_lines`:

  integer, maximum number of lines to read (default `300L`)

- `pattern`:

  character or `NULL`, optional regex pattern to filter log content

- `levels`:

  character, log levels to include (default: all levels)

#### Returns

`data.frame` with columns: `line_no`, `level`, `time`, `caller`,
`content`. Returns empty `data.frame` if file missing or no matches.

------------------------------------------------------------------------

### Method `get_tools()`

Get dynamic tool functions injected by agents

#### Usage

    AgentContext$get_tools(keys)

#### Arguments

- `keys`:

  character or missing, names of tools to retrieve. If omitted, all
  tools are returned.

------------------------------------------------------------------------

### Method `set_tool()`

Register a dynamic tool so agents can call it

#### Usage

    AgentContext$set_tool(key, tool)

#### Arguments

- `key`:

  character, name used to store and retrieve the tool

- `tool`:

  `ToolDef`, a ellmer tool created with
  [`ellmer::tool()`](https://ellmer.tidyverse.org/reference/tool.html)

------------------------------------------------------------------------

### Method `clear_tools()`

Remove previously registered tools

#### Usage

    AgentContext$clear_tools(keys)

#### Arguments

- `keys`:

  character or missing, names of tools to remove. If omitted, all tools
  are cleared.

------------------------------------------------------------------------

### Method `has_tools()`

Check whether tools are registered under given keys

#### Usage

    AgentContext$has_tools(keys)

#### Arguments

- `keys`:

  character, names of tools to check

#### Returns

logical vector, `TRUE` for each key with a registered tool
