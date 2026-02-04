# List `MCP` tool names from a package or directory

Lists all available `MCP` tool names from a package or directory. This
is a lightweight wrapper around `mcptool_load_all` that returns only the
tool names without loading full definitions.

## Usage

``` r
mcptool_list(pkg, groups = NULL)
```

## Arguments

- pkg:

  Character. Package name or path to tools directory. If `pkg` contains
  path separators, space, or `'.'`, it is treated as a directory path.
  Otherwise, it is treated as a package name.

- groups:

  Character vector or NULL. Optional group filter(s) to list only tools
  from specific group(s). Groups are extracted from tool names following
  the pattern `pkg-mcp_tool_{group}_{action}`. Each group can be a
  regexp pattern. If `NULL` (default), all tools are listed.

## Value

Character vector of tool names (in format `pkg-function_name`). Returns
empty character vector if no tools found.

## Examples

``` r
# List all tools from package
mcptool_list("tricobbler")
#>  [1] "tricobbler-mcp_tool_config_set_outputs"       
#>  [2] "tricobbler-mcp_tool_context_attachment_exists"
#>  [3] "tricobbler-mcp_tool_context_attachment_get"   
#>  [4] "tricobbler-mcp_tool_context_attachment_list"  
#>  [5] "tricobbler-mcp_tool_context_logs_head"        
#>  [6] "tricobbler-mcp_tool_context_logs_search"      
#>  [7] "tricobbler-mcp_tool_context_logs_tail"        
#>  [8] "tricobbler-mcp_tool_docs_available_vignettes" 
#>  [9] "tricobbler-mcp_tool_docs_help_page"           
#> [10] "tricobbler-mcp_tool_docs_package_help_topics" 
#> [11] "tricobbler-mcp_tool_docs_vignette"            
#> [12] "tricobbler-mcp_tool_read_package_file"        
#> [13] "tricobbler-mcp_tool_run_r"                    
#> [14] "tricobbler-mcp_tool_search_package_files"     
#> [15] "tricobbler-mcp_tool_search_package_info"      
#> [16] "tricobbler-mcp_tool_search_packages"          

# List only pipeline group tools
mcptool_list("tricobbler", groups = "pipeline")
#> NULL

# List multiple groups
mcptool_list("tricobbler", groups = c("pipeline", "config"))
#> [1] "tricobbler-mcp_tool_config_set_outputs"

# Or list tools from directory
path <- system.file("mcp", "tools", package = "tricobbler")
mcptool_list(path)
#>  [1] "tricobbler-mcp_tool_config_set_outputs"       
#>  [2] "tricobbler-mcp_tool_context_attachment_exists"
#>  [3] "tricobbler-mcp_tool_context_attachment_get"   
#>  [4] "tricobbler-mcp_tool_context_attachment_list"  
#>  [5] "tricobbler-mcp_tool_context_logs_head"        
#>  [6] "tricobbler-mcp_tool_context_logs_search"      
#>  [7] "tricobbler-mcp_tool_context_logs_tail"        
#>  [8] "tricobbler-mcp_tool_docs_available_vignettes" 
#>  [9] "tricobbler-mcp_tool_docs_help_page"           
#> [10] "tricobbler-mcp_tool_docs_package_help_topics" 
#> [11] "tricobbler-mcp_tool_docs_vignette"            
#> [12] "tricobbler-mcp_tool_read_package_file"        
#> [13] "tricobbler-mcp_tool_run_r"                    
#> [14] "tricobbler-mcp_tool_search_package_files"     
#> [15] "tricobbler-mcp_tool_search_package_info"      
#> [16] "tricobbler-mcp_tool_search_packages"          
```
