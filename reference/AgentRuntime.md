# Agent Runtime Environment for State Execution

R6 class that provides an isolated execution environment for agent
execution. Contains references to the executing agent (who), the context
(where), and the policy (what). This class is instantiated per-execution
and passed to agents, enabling async-safe execution without relying on
global state.

## Details

### Purpose

The runtime serves three main purposes:

1.  **Async safety**: Captures execution context in closures, avoiding
    race conditions with global state when agents execute concurrently

2.  **Simplified API**: Agents receive a single `runtime` argument
    instead of separate `self`, `policy`, `context` arguments

3.  **Tool injection**: MCP tools that declare a `.runtime` parameter
    receive the runtime automatically via closure capture at
    instantiation time

### Tool Runtime Injection

When
[`mcptool_instantiate`](http://dipterix.org/tricobbler/reference/mcptool_instantiate.md)
creates tool wrappers, it checks if the underlying function has a
`.runtime` formal parameter. If so, the runtime is automatically
injected into calls. This allows downstream packages to create MCP tools
that access the execution context:

    my_mcp_tool <- function(arg1, arg2, .runtime = NULL) {
      if (!is.null(.runtime)) {
        .runtime$logger("Tool called with runtime access")
        ctx <- .runtime$context
      }
      # ... tool implementation
    }

## Active bindings

- `agent`:

  Agent object, the agent being executed (who)

- `context`:

  AgentContext object, the execution context (where)

- `policy`:

  StatePolicy object, the policy being executed (what)

- `attempt`:

  integer, retry count if failed

- `attachment_id`:

  character, attachment prefix

- `id`:

  character, short identifier for this execution to show in the context
  logs

## Methods

### Public methods

- [`AgentRuntime$new()`](#method-TricobblerAgentRuntime-new)

- [`AgentRuntime$logger()`](#method-TricobblerAgentRuntime-logger)

- [`AgentRuntime$run_async()`](#method-TricobblerAgentRuntime-run_async)

- [`AgentRuntime$run()`](#method-TricobblerAgentRuntime-run)

------------------------------------------------------------------------

### Method `new()`

Initialize a new runtime environment

#### Usage

    AgentRuntime$new(agent, context, policy, attempt = 0L)

#### Arguments

- `agent`:

  Agent object being executed

- `context`:

  AgentContext object for logging and storage

- `policy`:

  StatePolicy object being executed

------------------------------------------------------------------------

### Method `logger()`

Log a message with the agent as caller

#### Usage

    AgentRuntime$logger(
      ...,
      level = c("INFO", "TRACE", "DEBUG", "WARN", "ERROR", "FATAL"),
      verbose = c("cli", "base", "none"),
      public = TRUE,
      role = NA_character_
    )

#### Arguments

- `...`:

  character, message components to paste together

- `level`:

  character, log level (INFO, WARN, ERROR, FATAL, DEBUG)

- `verbose`:

  character or logical, verbosity setting

#### Returns

NULL invisibly

------------------------------------------------------------------------

### Method `run_async()`

#### Usage

    AgentRuntime$run_async()

------------------------------------------------------------------------

### Method `run()`

#### Usage

    AgentRuntime$run()
