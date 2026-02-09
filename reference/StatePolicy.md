# State-Level Policy Implementation for Workflow Stages

Represents a single workflow state. Inherits from
[`BasePolicy`](http://dipterix.org/tricobbler/reference/BasePolicy.md)
and adds a mandatory `stage` that must match one of the stages defined
in a
[`MasterPolicy`](http://dipterix.org/tricobbler/reference/MasterPolicy.md).
Includes priority and criticality flags for execution ordering when
multiple states share the same stage.

## Usage

``` r
StatePolicy(
  name = character(0),
  description = character(0),
  stage = character(0),
  agent_id = character(0),
  resources = character(0),
  accessibility = "all",
  parameters = list(),
  max_retry = 0L,
  priority = 100L,
  critical = FALSE,
  final = FALSE,
  on_failure = NA_character_,
  depends_on = StateDeps()
)
```

## Arguments

- name:

  character, name of the state policy (non-blank)

- description:

  character, human-readable description

- stage:

  character, must be a non-blank single string

- agent_id:

  character, unique identifier for the agent responsible for executing
  this state. Must contain only letters, digits, underscores, or dashes

- resources:

  character vector, tools that the agent may call during execution
  (default empty vector)

- accessibility:

  character, context accessibility level for the agent. Controls what
  context data the agent can read. One of `"all"` (full access to
  previous results and logs), `"logs"` (log descriptions only, no raw
  results), `"none"` (no access to previous context), or `"explicit"`
  (use `depends_on` to specify inputs; if `depends_on` is empty, behaves
  like `"logs"`). Default is `"all"`

- parameters:

  list, state-specific parameters passed to the agent. See **Parameters
  for Different Agent Types** in Details

- max_retry:

  integer, maximum total attempts (initial + retries) for this state
  during stage execution (default 0, meaning single attempt). The
  `max_retry` limit applies globally across all re-entries to this state
  within the same stage. If `on_failure` is set, this state will not
  retry locally but will jump to the failure handler immediately. If
  `on_failure` is `NA`, local retries up to `max_retry` will be
  attempted before moving to next state

- priority:

  integer, execution priority (0-999, default 100). Higher values run
  first (999 = highest priority, 0 = lowest). Used when multiple states
  share the same stage. `NA` or `NULL` are treated as 100

- critical:

  logical, if `TRUE`, states with lower priority will not execute if
  this state fails (default `FALSE`). Critical states must have
  `priority >= 1` and cannot share priority with other states in the
  same stage

- final:

  logical, if `TRUE` and the agent succeeds, skip remaining states in
  the stage (default `FALSE`)

- on_failure:

  character, name of the state to jump to on first failure (default `NA`
  to retry locally up to `max_retry` times). When set, failures trigger
  immediate jump to the specified state without local retries. The
  `max_retry` limit still applies globally to prevent infinite loops: if
  this state is re-entered and total attempts exceed `max_retry`,
  execution stops with an error. Common patterns: validation loops
  (`on_failure = "executor"`), alternative strategies
  (`on_failure = "slower_alternative"`), or repair chains
  (`on_failure = "repair_step"`)

- depends_on:

  `StateDeps` object or named list specifying explicit dependencies on
  prior state outputs. Each entry maps a parameter name to a dependency:
  `list(param = list(state = "state_name", field = "result", stage = NULL))`.
  Used with `accessibility = "explicit"` for `async` execution. See
  [`StateDeps`](http://dipterix.org/tricobbler/reference/StateDeps.md)
  for format details

## Details

### Stages vs States

- **Stage** (macro): The workflow phase name from `MasterPolicy@stages`
  (e.g., `"executing"`)

- **State** (micro): A concrete `StatePolicy` implementation of that
  stage

- Multiple states can reference the same stage for different execution
  patterns

### Priority System and Execution Patterns

When multiple states share the same stage, execution pattern depends on
priority:

- **Range**: 0 (lowest) to 999 (highest)

- **Default**: 100 (when `priority = NA` or `NULL`)

- **Execution order**: States run sequentially by priority (higher
  first)

### Critical Flag: Enforcing Sequential Execution

The `critical` flag enforces **sequential** execution with fail-fast
semantics:

- If `critical = TRUE`, this state **must** execute and succeed before
  any lower-priority states in the same stage can run

- If a critical state fails, lower-priority states are skipped entirely

- Critical states must have unique priority (enforced by
  [`Manifest`](http://dipterix.org/tricobbler/reference/Manifest.md))

- Critical states must have `priority >= 1` (cannot be lowest priority
  0)

- Critical states cannot share their priority value with other states in
  the same stage (enforced by
  [`Manifest`](http://dipterix.org/tricobbler/reference/Manifest.md)
  validation)

- **Use case**: Required validation gates that must pass before
  alternatives run

### When to Use Multiple States Per Stage

Create multiple `StatePolicy` objects for the same stage when you need:

1.  **Fallback chains**: Different priorities create ordered execution
    with alternative strategies

2.  **Alternative implementations**: Multiple states for the same stage
    (e.g., primary approach, then fallback, then last resort)

3.  **Critical validation gates**: A critical state must succeed before
    lower-priority alternatives execute (enforces sequential, fail-fast
    semantics)

4.  **Phased deployment**: Gradually shift priority as new
    implementations mature

### Parameters for Different Agent Types

The `parameters` list is interpreted differently based on the agent type
created via
[`as_agent`](http://dipterix.org/tricobbler/reference/as_agent.md):

**Deterministic Agents** (from functions or `MCP` tools):

- `args`: list, function arguments passed via `do.call(fun, args)`. Kept
  separate from other parameters to avoid conflicts with functions that
  have their own reserved arguments

The `accessibility` property affects how inputs are constructed:

- `"all"`: First argument is the previous agent's result
  (`last_attachment$result`), followed by `args`

- `"logs"`: First argument is the previous agent's description
  (`last_attachment$description`), followed by `args`

- `"none"`: Only `args` are passed (no context access)

**Debug Mode**: Set `context$debug <- TRUE` at runtime to enable debug
mode. In debug mode, agents print their calls and tools for inspection
but run as no-op (returning debug information instead of executing).

**AI Agents** (from ellmer `Chat` objects):

- `system_prompt`: character, additional system prompt appended to
  `@description`

- `user_prompt`: character, the user message to send to the `LLM`

- `keep_turns`: logical, if `TRUE`, retains conversation history across
  executions (default `FALSE`)

- `return_type`: an ellmer type indicator (e.g.,
  [`ellmer::type_object()`](https://ellmer.tidyverse.org/reference/type_boolean.html)).
  When provided, triggers `chat$chat_structured(type = return_type)` for
  structured output. Can also be specified in `YAML` via
  [`map_type_to_ellmer()`](http://dipterix.org/tricobbler/reference/map_type_to_ellmer.md)

## Examples

``` r
# Basic state
StatePolicy(
  name = "state1",
  stage = "idle",
  description = "initial idle state",
  agent_id = "agent1",
  parameters = list()
)
#> StatePolicy (S7 class) - `state1` (idle)
#> initial idle state

# Critical high-priority state
StatePolicy(
  name = "validator",
  stage = "executing",
  description = "critical validation step",
  agent_id = "validator_agent",
  priority = 900,
  critical = TRUE
)
#> StatePolicy (S7 class) - `validator` (executing)
#> critical validation step

# Deterministic agent with parameters
StatePolicy(
  name = "formatter",
  stage = "executing",
  description = "Format data to JSON",
  agent_id = "json_formatter",
  accessibility = "none",  # Only use args, no context
  parameters = list(
    args = list(x = list(a = 1, b = 2), pretty = TRUE)
  )
)
#> StatePolicy (S7 class) - `formatter` (executing)
#> Format data to JSON

# AI agent with structured output
StatePolicy(
  name = "planner",
  stage = "planning",
  description = "Break down the task into steps",
  agent_id = "llm_planner",
  parameters = list(
    system_prompt = "You are a task planning expert.",
    user_prompt = "Plan the following task: ...",
    keep_turns = FALSE,
    return_type = ellmer::type_object(
      steps = ellmer::type_array(items = ellmer::type_string())
    )
  )
)
#> StatePolicy (S7 class) - `planner` (planning)
#> Break down the task into steps

```
