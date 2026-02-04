# Write `MCP` tool definition to file

Writes a `MCP` tool definition to a file in either `YAML` or `Markdown`
format.

## Usage

``` r
mcptool_write(tool, path, method = c("yaml", "markdown"), ...)
```

## Arguments

- tool:

  A `tricobbler_mcp_tool` object

- path:

  Character. Path to the output file

- method:

  Character. Output format: `"yaml"` (default) or `"markdown"`

- ...:

  Additional arguments (currently unused)

## Value

Invisibly returns the input tool object

## Examples

``` r
# Load a tool from package
path <- mcptool_path("tricobbler-mcp_tool_search_packages")
tool <- mcptool_read(path)

# Write as YAML to temporary file
mcptool_write(tool, stdout(), method = "yaml")
#> name: tricobbler-mcp_tool_search_packages
#> description: Search for R packages that match a keyword or pattern in their name,
#>   title, or description. Searches CRAN package database for matches. Useful for discovering
#>   packages related to a specific topic.
#> parameters:
#>   type: object
#>   properties:
#>     max_results:
#>       type: integer
#>       description: 'Maximum number of results to return. Default is 20. Use this to
#>         limit output size for broad searches. Example values: 10, 50, 100.'
#>       default: 20.0
#>       examples:
#>       - '10'
#>       - '50'
#>       - '100'
#>     query:
#>       type: string
#>       description: 'The search query. Can be a keyword, partial package name, or topic.
#>         The search is case-insensitive and matches against package names, titles,
#>         and descriptions. Examples: "pipeline", "visualization", "data".'
#>   required: query
#> category: discovery
#> returns:
#>   type: object
#>   properties:
#>     error:
#>       type: string
#>       description: Character, error message if success is FALSE
#>     returned:
#>       type: integer
#>       description: Integer, number of results returned (limited by max_results)
#>     count:
#>       type: integer
#>       description: Integer, total number of packages matching the query
#>     packages:
#>       type: object
#>       description: Data frame with columns package (package name), version (version
#>         string), title (package title), description (brief description), date (publication
#>         date), and maintainer (maintainer name)
#>     query:
#>       type: string
#>       description: Character, the search query that was used
#>     success:
#>       type: boolean
#>       description: Logical, whether the search completed successfully
#> implementation_example: "# Example output from calling the tool\ntricobbler:::mcp_tool_search_packages(\"tricobbler\")\n#\n#>
#>   $success\n#> [1] TRUE\n#> \n#> $query\n#> [1] \"tricobbler\"\n#> \n#> $packages\n#>
#>   [1] package     version     title       description date        maintainer \n#>
#>   <0 rows> (or 0-length row.names)\n#> \n#> $count\n#> [1] 0\n#> \n#> $returned\n#>
#>   [1] 0\n#> \n#> $message\n#> [1] \"No packages found matching query: tricobbler\"\n#>\n"
#> execution_time: 0.5

# Write as Markdown
mcptool_write(tool, stdout(), method = "markdown")
#> # tricobbler-mcp_tool_search_packages
#> 
#> Search for R packages that match a keyword or pattern in their name, title, or description. Searches CRAN package database for matches. Useful for discovering packages related to a specific topic.
#> 
#> ## Parameters
#> 
#> - **max_results** (integer, *optional*)
#>   Maximum number of results to return. Default is 20. Use this to
#>   limit output size for broad searches. Example values: 10, 50, 100.
#>   Default: 20
#> 
#> - **query** (string, **required**)
#>   The search query. Can be a keyword, partial package name, or topic.
#>   The search is case-insensitive and matches against package names,
#>   titles, and descriptions. Examples: "pipeline", "visualization",
#>   "data".
#> 
#> ## Returns
#> 
#> Type: `object`
#> 
#> ### Properties
#> 
#> - `error` (string): Character, error message if success is FALSE
#>   Character, error message if success is FALSE
#> 
#> - `returned` (integer): Integer, number of results returned (limited by max_results)
#>   Integer, number of results returned (limited by max_results)
#> 
#> - `count` (integer): Integer, total number of packages matching the query
#>   Integer, total number of packages matching the query
#> 
#> - `packages` (object): Data frame with columns package (package name), version (version string), title (package title), description (brief description), date (publication date), and maintainer (maintainer name)
#>   Data frame with columns package (package name), version (version
#>   string), title (package title), description (brief description),
#>   date (publication date), and maintainer (maintainer name)
#> 
#> - `query` (string): Character, the search query that was used
#>   Character, the search query that was used
#> 
#> - `success` (boolean): Logical, whether the search completed successfully
#>   Logical, whether the search completed successfully
#> 
#> ## Metadata
#> 
#> - **Category**: discovery
#> 
```
