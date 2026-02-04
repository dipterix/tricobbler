# Find path to a specific `MCP` tool by name

Locates a `MCP` tool YAML file from either an installed package or a
local directory. Tool names use `pkg-function_name` format where the
first hyphen separates the package name from the function name. Returns
the file path with the tool definition attached as an attribute.

## Usage

``` r
mcptool_path(tool_name, tools_dir = "../tools")
```

## Arguments

- tool_name:

  Character. Tool name in format `pkg-function_name`. The first hyphen
  separates package name from function name (function names may contain
  hyphens).

- tools_dir:

  Character. Path to root directory for local tools look-up. Default is
  `"../tools"` (relative to workflows directory).

## Value

Character string containing the path to the tool YAML file, with
attribute `mcp_definition` containing the parsed tool definition. Throws
an error if tool not found.

## Examples

``` r
# Find tool from package
tool_path <- mcptool_path("tricobbler-mcp_tool_search_packages")
attr(tool_path, "mcp_definition")
#> $name
#> [1] "tricobbler-mcp_tool_search_packages"
#> 
#> $description
#> [1] "Search for R packages that match a keyword or pattern in their name, title, or description. Searches CRAN package database for matches. Useful for discovering packages related to a specific topic."
#> 
#> $parameters
#> $parameters$type
#> [1] "object"
#> 
#> $parameters$properties
#> $parameters$properties$max_results
#> $parameters$properties$max_results$type
#> [1] "integer"
#> 
#> $parameters$properties$max_results$description
#> [1] "Maximum number of results to return. Default is 20. Use this to limit output size for broad searches. Example values: 10, 50, 100."
#> 
#> $parameters$properties$max_results$default
#> [1] 20
#> 
#> $parameters$properties$max_results$examples
#> [1] "10"  "50"  "100"
#> 
#> 
#> $parameters$properties$query
#> $parameters$properties$query$type
#> [1] "string"
#> 
#> $parameters$properties$query$description
#> [1] "The search query. Can be a keyword, partial package name, or topic. The search is case-insensitive and matches against package names, titles, and descriptions. Examples: \"pipeline\", \"visualization\", \"data\"."
#> 
#> 
#> 
#> $parameters$required
#> [1] "query"
#> 
#> 
#> $category
#> [1] "discovery"
#> 
#> $returns
#> $returns$type
#> [1] "object"
#> 
#> $returns$properties
#> $returns$properties$error
#> $returns$properties$error$type
#> [1] "string"
#> 
#> $returns$properties$error$description
#> [1] "Character, error message if success is FALSE"
#> 
#> 
#> $returns$properties$returned
#> $returns$properties$returned$type
#> [1] "integer"
#> 
#> $returns$properties$returned$description
#> [1] "Integer, number of results returned (limited by max_results)"
#> 
#> 
#> $returns$properties$count
#> $returns$properties$count$type
#> [1] "integer"
#> 
#> $returns$properties$count$description
#> [1] "Integer, total number of packages matching the query"
#> 
#> 
#> $returns$properties$packages
#> $returns$properties$packages$type
#> [1] "object"
#> 
#> $returns$properties$packages$description
#> [1] "Data frame with columns package (package name), version (version string), title (package title), description (brief description), date (publication date), and maintainer (maintainer name)"
#> 
#> 
#> $returns$properties$query
#> $returns$properties$query$type
#> [1] "string"
#> 
#> $returns$properties$query$description
#> [1] "Character, the search query that was used"
#> 
#> 
#> $returns$properties$success
#> $returns$properties$success$type
#> [1] "boolean"
#> 
#> $returns$properties$success$description
#> [1] "Logical, whether the search completed successfully"
#> 
#> 
#> 
#> 
#> $implementation_example
#> [1] "# Example output from calling the tool\ntricobbler:::mcp_tool_search_packages(\"tricobbler\")\n#\n#> $success\n#> [1] TRUE\n#> \n#> $query\n#> [1] \"tricobbler\"\n#> \n#> $packages\n#> [1] package     version     title       description date        maintainer \n#> <0 rows> (or 0-length row.names)\n#> \n#> $count\n#> [1] 0\n#> \n#> $returned\n#> [1] 0\n#> \n#> $message\n#> [1] \"No packages found matching query: tricobbler\"\n#>\n"
#> 
#> $execution_time
#> [1] 0.5
#> 
#> attr(,"class")
#> [1] "tricobbler_mcp_tool" "list"               


if (FALSE) { # \dontrun{

# Simple tool name (looks in tools_dir only)
tool_path <- mcptool_path("my_tool", tools_dir = "./tools")

} # }
```
