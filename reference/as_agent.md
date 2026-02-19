# Convert Objects to Agent

Generic function to convert various object types into
[`Agent`](http://dipterix.org/tricobbler/reference/Agent.md) objects for
use with the
[`Scheduler`](http://dipterix.org/tricobbler/reference/Scheduler.md).
Supports conversion from ellmer Chat objects, functions, `MCP` tool
specifications, and package function references.

## Usage

``` r
as_agent(x, ...)
```

## Arguments

- x:

  Object to convert. Can be:

  - An `Agent` object (returned as-is)

  - An ellmer `Chat` object (creates an `LLM`-backed agent)

  - A function with signature `function(runtime)`, where `runtime` is an
    `AgentRuntime` object

  - A character string referencing a package function (`"pkg::fun"`)

  - An `MCP` tool definition (class `tricobbler_mcp_tool`)

- ...:

  Additional arguments passed to methods. Common arguments include `id`
  (character, unique agent identifier), `description` (character,
  human-readable description), and `describe` (function, result
  formatting for logging; defaults to
  [`mcp_describe`](http://dipterix.org/tricobbler/reference/mcp_describe.md)).

## Value

An `Agent` object.

## Chat Agent Parameters

When converting an ellmer Chat object, the following `parameters` (set
in
[`StatePolicy`](http://dipterix.org/tricobbler/reference/StatePolicy.md)
or
[`MasterPolicy`](http://dipterix.org/tricobbler/reference/MasterPolicy.md))
are recognized:

- `max_tokens`:

  integer, maximum tokens for the `LLM` response. Applied to the
  provider before each call. Useful for preventing output truncation
  with long tool-calling workflows (e.g. set to `16384`).

- `max_chat_errors`:

  integer, number of consecutive `LLM` call errors (e.g. truncated
  `JSON`, `API` timeouts) tolerated before the agent gives up. Defaults
  to `Inf`. Within this budget the error message is fed back to the
  `LLM` so it can self-correct.

- `system_prompt`:

  character, overrides the policy description.

- `user_prompt`:

  character, task prompt sent to the `LLM`.

- `keep_turns`:

  logical, preserve conversation history across retries (default
  `FALSE`).

- `return_type`:

  an ellmer type specification for structured output.

## See also

[`StatePolicy`](http://dipterix.org/tricobbler/reference/StatePolicy.md)
for details on how to set additional arguments via `parameters` for
deterministic agents (functions, `MCP` tools) and AI agents (ellmer Chat
objects).

## Examples

``` r
# From a simple function
my_func <- function(runtime) {
  runtime$logger("Hello from my agent!")
  return("done")
}
agent <- as_agent(my_func)


if (FALSE) { # \dontrun{
# From an ellmer Chat (requires API credentials)
chat <- ellmer::chat_openai(model = "gpt-4o-mini")
agent <- as_agent(chat)

# From a package function reference
agent <- as_agent("utils::read.csv", id = "mean_agent")
} # }
```
