# Agent Function Wrapper for State Execution

Creates an executable agent that can be registered with a
[`Scheduler`](http://dipterix.org/tricobbler/reference/Scheduler.md) to
execute state-specific logic. The agent wraps a user-defined function
with metadata (`id`, `description`) and result formatting. Agents are
the execution units in the Runtime Layer (Tier 2) that implement the
logic defined in
[`StatePolicy`](http://dipterix.org/tricobbler/reference/StatePolicy.md)
objects from the Policy Layer (Tier 1).

## Usage

``` r
Agent(
  .data = function() NULL,
  id = character(0),
  description = character(0),
  describe = tricobbler::mcp_describe
)
```

## Arguments

- .data:

  function, the agent implementation with signature
  `function(runtime, ...)`

- id:

  character, unique identifier for the agent (letters, digits,
  underscores, dashes only)

- description:

  character, human-readable description of what the agent does

- describe:

  function or `NULL`, result formatting function for logging; defaults
  to
  [`mcp_describe`](http://dipterix.org/tricobbler/reference/mcp_describe.md).
  If provided, should accept the agent's return value as its first
  argument and return a character string

## Value

An `Agent` object (S7 class inheriting from
[`function`](https://rdrr.io/r/base/function.html))

## Details

### Agent Function Signature

The wrapped function must have the following signature:

    function(runtime, ...) {
      # Agent implementation
      # runtime$agent - the Agent object itself
      # runtime$policy - the StatePolicy being executed
      # runtime$context - the AgentContext for logging
      # runtime$logger() - shorthand for logging
      # Return value will be logged to context via @describe
    }

**Required arguments:**

- `runtime`: An
  [`AgentRuntime`](http://dipterix.org/tricobbler/reference/AgentRuntime.md)
  object containing:

  - `runtime$agent`: Reference to the `Agent` object itself

  - `runtime$policy`: The
    [`StatePolicy`](http://dipterix.org/tricobbler/reference/StatePolicy.md)
    object being executed

  - `runtime$context`: The
    [`AgentContext`](http://dipterix.org/tricobbler/reference/AgentContext.md)
    object for logging and storage

  - `runtime$logger()`: Method to log messages

- `...`: Additional arguments (optional)

### Agent Execution Flow

When a
[`Scheduler`](http://dipterix.org/tricobbler/reference/Scheduler.md)
executes a state:

1.  Looks up the agent by `StatePolicy@agent_id`

2.  Creates an
    [`AgentRuntime`](http://dipterix.org/tricobbler/reference/AgentRuntime.md)
    with agent, policy, and context

3.  Calls the agent function with `(runtime)`

4.  Captures the return value

5.  Uses the `describe` function to format the result for logging

6.  Logs formatted result to
    [`AgentContext`](http://dipterix.org/tricobbler/reference/AgentContext.md)

### Result Description

The `describe` property controls how results are formatted for
AI-readable logs. By default, uses
[`mcp_describe`](http://dipterix.org/tricobbler/reference/mcp_describe.md)
which captures the printed results. The `describe` property serves three
purposes: first, the function allows the agent to format the output for
better AI-readability. The formatted results will be logged into a
public log file along with the scheduling messages; second, the agent
can choose to redact sensitive information and avoid insecure agents
from accessing those data - reading attachments requires permissions and
those agents (according to how they are implemented) should not access
the attachments; finally, because the output description will be
recorded into the public log file so the other agents can still refer to
the context without reading the corresponding attachment file. For
example, if a local agent's output contains user-sensitive data, its
`describe` function may hide those information by string replacement, or
simply show the data format and schema. Other online agents may still
see the redacted outputs from the log files and work on the output (such
as writing code or executing the tools).

## Examples

``` r

# Basic agent
simple_agent <- Agent(
  function(runtime) {
    runtime$logger("Executing simple task")
    return("Task completed")
  },
  id = "simple_agent",
  description = "A simple demonstration agent"
)


# Agent with custom result formatting function
analysis_agent <- Agent(
  function(runtime) {
    result <- list(
      status = "success",
      items_processed = 42,
      timestamp = Sys.time(),
      long_result = rnorm(100),
      sensitive_data = "my password"
    )
    return(result)
  },
  id = "analysis_agent",
  description = "Performs data analysis",
  describe = function(result) {
    result$sensitive_data <- "<password redacted...>"
    c(
      sprintf("Processed %d items at %s.\n",
              result$items_processed, result$timestamp),
      "Here are the snapshots with R str():",
      utils::capture.output(str(result))
    )
  }
)

# run these agents
manifest <- Manifest(
  master = MasterPolicy(
    name = "example",
    version = "0.0.1",
    stages = "executing"
  ),
  states = list(
    StatePolicy(name = "simply_policy",
                stage = "executing",
                agent_id = "simple_agent"),
    StatePolicy(name = "analysis_policy",
                stage = "executing",
                agent_id = "analysis_agent")
  )
)

scheduler <- Scheduler$new(manifest = manifest,
                           agents = list(simple_agent, analysis_agent))

if(interactive()) {
  scheduler$start()
  # TRACE 10:53:15 [scheduler]: Initializing resources
  # TRACE 10:53:15 [scheduler]: current stage: executing
  # TRACE 10:53:15 [scheduler]: starting state: simply_policy with
  #  agent simple_agent (attempt 0)
  # INFO 10:53:15 [Agent simple_agent]: Executing simple task
  # INFO 10:53:15 [context]: Following result recorded:
  #  Agent=simple_agent, stage=executing,
  #  state=simply_policy, attempt=0,
  #  identifier=[executing][simply_policy][simple_agent]_20T105315_0
  # INFO 10:53:15 [context]: "Task completed"
  # TRACE 10:53:15 [scheduler]: starting state: analysis_policy with
  #  agent analysis_agent (attempt 0)
  # INFO 10:53:15 [context]: Following result recorded:
  #  Agent=analysis_agent, stage=executing,
  #  state=analysis_policy, attempt=0,
  #  identifier=[executing][analysis_policy][analysis_agent]_20T105315_0
  # INFO 10:53:15 [context]: Processed 42 items at 10:53:15.
  # INFO 10:53:15 [context]: Here are the snapshots with R str():
  #  List of 5 $ status :
  #    chr "success" $ items_processed: num 42
  #    $ timestamp : POSIXct[1:1], format: "2026-02-03 10:53:15"
  #    $ long_result : num [1:100] 0.372 0.504 -1.584 -0.728 0.18 ...
  #    $ sensitive_data : chr "<password redacted...>"
  # TRACE 10:53:15 [scheduler]: current stage: ready
}

```
