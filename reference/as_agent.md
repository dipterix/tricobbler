# Convert Objects to Agent

Generic function to convert various object types into
[`Agent`](http://dipterix.org/tricobbler/reference/Agent.md) objects for
use with the
[`Scheduler`](http://dipterix.org/tricobbler/reference/Scheduler.md).
Supports conversion from ellmer Chat objects, functions, MCP tool
specifications, and package function references.

## Usage

``` r
as_agent(x, ...)
```

## Arguments

- x:

  Object to convert. Can be:

  - An `Agent` object (returned as-is)

  - An ellmer `Chat` object (creates an LLM-backed agent)

  - A function with signature `function(self, policy, context)`

  - A character string referencing a package function (`"pkg::fun"`)

  - An MCP tool definition (class `tricobbler_mcp_tool`)

- ...:

  Additional arguments passed to
  [`Agent`](http://dipterix.org/tricobbler/reference/Agent.md).

- id:

  Character. Unique identifier for the agent. Required for non-Agent
  inputs. For Chat objects, defaults to `"chat_<provider>_<model>"`.

- description:

  Character. Human-readable description. For Chat objects,
  auto-generated from model info if not provided.

- describe:

  Function or NULL. Result formatting function for logging. Defaults to
  [`mcp_describe`](http://dipterix.org/tricobbler/reference/mcp_describe.md).

## Value

An `Agent` object.

## See also

[`StatePolicy`](http://dipterix.org/tricobbler/reference/StatePolicy.md)
for details on how to set additional arguments via `parameters` for
deterministic agents (functions, MCP tools) and AI agents (ellmer Chat
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
