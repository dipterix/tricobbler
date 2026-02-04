# Instantiate from RAVE `MCP` tool definition

Create an ellmer tool object from a loaded RAVE `MCP` tool definition.
This allows using RAVE pipelines and tools directly via the ellmer
package.

## Usage

``` r
mcptool_instantiate(tool, ...)
```

## Arguments

- tool:

  An object of class `tricobbler_mcp_tool` (loaded via
  [`mcptool_read`](http://dipterix.org/tricobbler/reference/mcptool_read.md)
  or
  [`mcptool_load_all`](http://dipterix.org/tricobbler/reference/mcptool_load_all.md)).

- ...:

  Additional arguments passed to
  [`tool`](https://ellmer.tidyverse.org/reference/tool.html).

## Value

An [`ToolDef`](https://ellmer.tidyverse.org/reference/tool.html) object
ready to be registered with a chat session.
