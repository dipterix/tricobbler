# Build `MCP` Tool Definitions from Package Documentation

Helper function for developers to scan the package's R source files for
functions marked with `@keywords mcp-tool` and generates `YAML` tool
definitions in `inst/mcp/tools/`. Run this after documenting your
package.

## Usage

``` r
mcptool_build(path = ".", verbose = TRUE)
```

## Arguments

- path:

  Character. Path to package root directory (default: `.`)

- verbose:

  Logical. Print progress messages (default: `TRUE`)

## Value

Invisibly returns list of generated tool definitions

## Examples

``` r
if (FALSE) { # \dontrun{

# After documenting your package `devtools::document()`

mcptool_build()

} # }
```
