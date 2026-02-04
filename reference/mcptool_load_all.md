# Load `MCP` Tool Definitions from Package or Directory

Load `MCP` Tool Definitions from Package or Directory

## Usage

``` r
mcptool_load_all(pkg, groups = NULL)
```

## Arguments

- pkg:

  Character. Package name or path to tools directory. If `pkg` contains
  path separators, space, or `.`, it is treated as a directory path.
  Otherwise, it is treated as a package name.

- groups:

  Character vector or NULL. Optional group filter(s) to load only tools
  from specific group(s). Groups are extracted from tool names following
  the pattern `pkg-mcp_tool_{group}_{action}`. Each group can be a
  regexp pattern. For example, "pipeline", "config", or
  "(pipeline\|config)". If `NULL` (default), all tools are loaded.

## Value

List of `MCP` tool definitions

## Examples

``` r
# Load all tools from package
tools <- mcptool_load_all("tricobbler")

# Load only pipeline group tools
pipeline_tools <- mcptool_load_all("tricobbler", groups = "pipeline")

# Load multiple groups
tools <- mcptool_load_all("tricobbler", groups = c("pipeline", "config"))

# Load from directory
path <- system.file("mcp", "tools", package = "tricobbler")
tools <- mcptool_load_all(path)
```
