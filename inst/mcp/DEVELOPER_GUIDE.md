# RAVE MCP Tools Developer Guide

This guide explains how to properly format roxygen2 documentation for automatic MCP tool generation.

## Quick Start

1. Document your R function with roxygen2 comments
2. Include required tags: `@description`, `@param`
3. Include optional tags: `@return`, `@examples`, `@keywords`
4. Run `mcptool_build()` to generate YAML files
5. Load tools with `mcptool_load_all()` for MCP server integration

## Workflow Overview

**You don't need to write YAML files manually.** The recommended workflow is:

1. **Write R functions** with proper roxygen2 documentation
2. **Run `mcptool_build()`** to automatically generate YAML files
3. **Load tools** with `mcptool_load_all()` for MCP server integration

This ensures consistency and reduces manual errors. The build process validates your documentation and extracts all necessary metadata automatically.

## Hidden Parameters

**Parameters starting with `.` are automatically excluded from MCP tool specifications.**

This allows you to pass internal context (like session state environments) without exposing these parameters to AI agents.

### Standard Parameter: `.state_env`

Use `.state_env` for session state. First, define a helper function for state management:

```r
#' Create MCP State Environment
#'
#' @description Creates a state management object for MCP tools to share
#' session-level data and cache results.
#'
#' @return A list with methods: get, set, has, remove, keys, reset, clear, size, as_list
#' @keywords internal
mcp_state_env <- function() {
  re <- new.env(parent = baseenv())
  with(re, {
    .store <- list()
    list(
      get = function(key, missing = NULL) {  
        if(key %in% names(.store)) {
          result <- .store[[key]]
        } else {
          result <- missing
        }
        return(result)
      },
      set = function(key, value) { .store[[key]] <<- value },
      has = function(key) { key %in% names(.store) },
      clear = function() { reset() },
      reset = function() { .store <<- list() },
      size = function() { return(length(.store)) },
      as_list = function() { .store },
      keys = function() { names(.store) },
      remove = function(keys) {
        .store <<- .store[!names(.store) %in% keys]
      }
    )
  })
}
```

**Alternative with fastmap**: If your package depends on or imports `fastmap`, you can use it directly for better performance:

```r
.state_env = fastmap::fastmap()
```

Then use `mcp_state_env()` as the default for `.state_env`:

```r
#' Search Pipeline Results
#'
#' @param query Character. Search query to execute
#' @param max_results Integer. Maximum number of results (default: 10)
#' @param .state_env Environment. Session state (auto-initialized)
#'
#' @keywords mcp-tool mcp-category-search
#' @noRd
mcp_search_results <- function(query, 
                                max_results = 10,
                                .state_env = mcp_state_env()) {
  
  # Check cache in session state
  cache_key <- paste0("search:", query)
  cached <- .state_env$get(cache_key)
  
  if (!is.null(cached)) {
    return(cached)
  }
  
  # Perform search
  results <- perform_search(query, max_results)
  
  # Cache results
  .state_env$set(cache_key, results)
  
  results
}
```

**What gets hidden:**
- Parameters starting with `.` (like `.state_env`) are excluded from YAML
- AI agents only see `query` and `max_results` parameters
- Tools can still access session state internally

**State operations:**
```r
# Store value
.state_env$set("key", value)

# Retrieve value
value <- .state_env$get("key", missing = NULL)

# Check existence
if (.state_env$has("key")) { ... }

# Remove value
.state_env$remove("key")

# Get all keys
keys <- .state_env$keys()

# Clear all
.state_env$reset()
```

**Workflow integration:**
```r
# Create session-scoped state
state <- mcp_state_env()
# Or use fastmap if available
# state <- fastmap::fastmap()

# Initialize with data
state$set("search_cache", list())
state$set("user_preferences", list())

# Instantiate workflow with state
tools <- mcpflow_instantiate("my-workflow", session_state = state)

# All tools automatically receive .state_env = state
```

## Complete Example

```r
#' Get RAVE Pipeline Information
#'
#' @description Retrieves detailed information about a specific RAVE pipeline
#' including its description, parameters, and current configuration.
#'
#' @param pipeline_name Character string, the name of the pipeline to query.
#'   Example values: \code{"power_explorer"}, \code{"notch_filter"},
#'   \code{"wavelet_module"}
#' @param include_settings Logical, whether to include current settings.
#'   Default is \code{TRUE}
#' @param format Character string, output format. Must be one of "summary"
#'   or "detailed"
#'
#' @return A list containing pipeline metadata, parameter definitions, and
#'   current settings (if requested)
#'
#' @examples
#'
#' # Load and query a pipeline
#' pipe <- ravepipeline::pipeline("power_explorer")
#' list(
#'   name = pipe$pipeline_name,
#'   description = pipe$description$Title,
#'   settings = pipe$get_settings()
#' )
#'
#' @keywords mcp-tool mcp-category-pipeline_management mcp-requires-approval
#' @noRd
#'
#' @export
mcp_get_rave_pipeline_info <- function(
  pipeline_name,
  include_settings = TRUE,
  format = c("summary", "detailed")
) {
  format <- match.arg(format)
  # Implementation...
}
```

## MCP Tool Functions Should Be Internal

All `mcp_tool_*` functions should be **internal** (not exported) to avoid cluttering the package namespace:

```r
#' Get RAVE Pipeline Information
#'
#' @description Retrieves detailed information about a specific RAVE pipeline
#' including its description, parameters, and current configuration.
#'
#' @param pipeline_name Character string, the name of the pipeline to query
#' @return A list containing pipeline metadata and settings
#' @keywords mcp-tool mcp-category-pipeline_management
#' @noRd
mcp_tool_pipeline_get_info <- function(pipeline_name) {
  # Implementation using exported functions
  pipe <- ravepipeline::pipeline(pipeline_name)
  list(
    name = pipe$pipeline_name,
    description = pipe$description$Title
  )
}
```

**Key points**:
- Add `@noRd` as the **last roxygen2 tag** (right before the function definition)
- This prevents generating `.Rd` documentation files
- Keeps the package API clean and focused on user-facing functions

**In `@examples`**:
- Show equivalent patterns using **exported functions** when possible
- This helps users understand how to achieve similar results in their own code
- Example: Use `ravepipeline::pipeline()` instead of internal `ravepipeline:::mcp_tool_pipeline_get_info()`

## Roxygen2 Tags Reference

### Required Tags

#### `@description`

The `@description` tag is **required** for all MCP tools. Provide a clear, concise description:

```r
#' Get RAVE Pipeline Information
#'
#' @description Retrieves detailed information about a specific RAVE pipeline
#' including its description, parameters, and current configuration.
```

**Important**: The parser only extracts text from the `@description` tag. The first comment line (title) is not automatically used as the description.

#### `@param`

Document each parameter with:
1. **Type prefix** (optional but recommended): "Character string,", "Logical,", "Integer,", etc.
2. **Description**: Clear explanation of the parameter
3. **Examples** (optional): Use `\code{}` blocks for per-parameter examples

**Type inference is case-insensitive**: "Character", "character", and "CHARACTER" are all recognized. See [Type Inference Best Practices](#type-inference-best-practices) for detailed rules and patterns.

**Basic parameter**:
```r
#' @param pipeline_name Character string, the pipeline name
```

Generates:
```yaml
pipeline_name:
  type: string
  description: the pipeline name
```

**With examples**:
```r
#' @param pipeline_name Character string, the pipeline name.
#'   Example values: \code{"power_explorer"}, \code{"notch_filter"}
```

Generates:
```yaml
pipeline_name:
  type: string
  description: the pipeline name
  examples:
  - power_explorer
  - notch_filter
```

**Array parameters** (multiple valid formats):
```r
#' @param target_names Character vector, names of targets to load
#' @param scores Numeric array, test scores for each subject
#' @param ids Integer[], array of subject IDs
#' @param tags Character[], list of tags
```

All of these generate `type: array` with appropriate `items.type`:
```yaml
target_names:
  type: array
  items:
    type: string
  description: names of targets to load
scores:
  type: array
  items:
    type: number
  description: test scores for each subject
ids:
  type: array
  items:
    type: integer
  description: array of subject IDs
```

Recognized patterns:
- "Type vector" (e.g., "Character vector", "Numeric vector")
- "Type array" (e.g., "Integer array")
- "Type[]" (e.g., "Character[]", "Logical[]")

**Type in \code{} blocks** (for explicit type specification):
```r
#' @param data \code{character} vector of file paths
#' @param counts \code{integer[]} array of counts
```

Generates:
```yaml
data:
  type: string
  description: vector of file paths
counts:
  type: array
  items:
    type: integer
  description: array of counts
```

The parser extracts types from `\code{type}` or `\code{type[]}` patterns.

**Multi-line parameter**:
```r
#' @param settings_json Character string, JSON string containing
#'   pipeline settings. Example: \code{"{\\"key\\": \\"value\\",
#'   \\"number\\": 123}"}
```

### Optional Tags

#### `@return` or `@returns`

Both `@return` and `@returns` are supported (they work identically). The parser can handle two formats:

**Simple return** (plain text description):
```r
#' @return A list containing pipeline metadata and settings
```

Generates:
```yaml
returns:
  type: object
  description: A list containing pipeline metadata and settings
```

**Structured return** (using `\describe{}` with `\item{}`):
```r
#' @return A list containing:
#' \describe{
#'   \item{success}{Logical, whether the operation succeeded}
#'   \item{message}{Character, human-readable status message}
#'   \item{data}{List, result data}
#'   \item{count}{Integer, number of items processed}
#' }
```

Generates:
```yaml
returns:
  type: object
  properties:
    success:
      type: boolean
      description: whether the operation succeeded
    message:
      type: string
      description: human-readable status message
    data:
      type: object
      description: result data
    count:
      type: integer
      description: number of items processed
```

**How it works**:
- The parser detects `\describe{}` blocks and extracts `\item{name}{description}` entries
- Type inference is applied to each item's description (same rules as `@param`)
- Handles nested braces correctly in both names and descriptions
- Multi-line `@return` content is supported (continues until next `@tag`)

#### `@examples`

**REQUIRED for all MCP tools.** Provide runnable R code examples. The `mcptool_build()` function executes these examples and captures their output to generate the `implementation_example` field in YAML.

**Purpose**: Examples serve as implementation templates for AI assistants and usage references for developers.

**Basic example**:
```r
#' @examples
#'
#' # List all available pipelines
#' pipelines <- ravepipeline::pipeline_list()
#' list(pipelines = pipelines, count = length(pipelines))
```

This generates a YAML field with both the code and its output:
```yaml
implementation_example: |
  # List all available pipelines
  pipelines <- ravepipeline::pipeline_list()
  list(pipelines = pipelines, count = length(pipelines))
  #
  #> $pipelines
  #> [1] "power_explorer" "notch_filter" "wavelet_module"
  #> 
  #> $count
  #> [1] 3
execution_time: 0.023
```

**Best practices**:
- **Write runnable code**: Examples are executed during build, ensure they work
- **Show realistic usage**: Use actual function calls, not abstract toy examples
- **Avoid `\dontrun{}`**: Only use when code requires external resources
- **Use exported functions**: Prefer `pkg::function()` over `pkg:::internal()` when possible
- **Keep examples focused**: Demonstrate typical use cases, not edge cases

**What NOT to do**:
```r
# ❌ INCORRECT - JSON responses are ignored
#' @examples
#' # MCP example response:
#' # {"status": "success"}

# ❌ INCORRECT - Unnecessary \dontrun{}
#' @examples
#' \dontrun{
#'   result <- mcp_tool()
#' }

# ✅ CORRECT - Runnable code
#' @examples
#' result <- ravepipeline::pipeline_list()
#' print(result$count)
```

#### `@keywords`

Add metadata keywords using the `mcp-` prefix:

**Tool marker and category**:
```r
#' @keywords mcp-tool mcp-category-pipeline_management
#' @keywords mcp-tool mcp-category-data_processing
```

**Safety flags**:
```r
#' @keywords mcp-tool mcp-dangerous
#' @keywords mcp-tool mcp-requires-approval
```

**Combined keywords** (recommended):
```r
#' @keywords mcp-tool mcp-category-execution mcp-dangerous mcp-requires-approval
```

All MCP tools should include `mcp-tool` keyword. Multiple keywords can be on the same line.

## Type Inference Best Practices

### Specify Types Clearly

Use standard R type prefixes (case-insensitive):

```r
#' @param name Character string, user's name
#' @param age Integer, user's age in years
#' @param score Numeric, test score (0-100)
#' @param active Logical, whether user is active
#' @param metadata List, additional metadata
```

### Array Types

Indicate arrays using vector notation:

```r
#' @param tags Character vector, list of tags
#' @param scores Numeric vector, array of scores
#' @param ids Integer[], array of IDs
```

All generate `type: array` with appropriate `items.type`.

### Avoid Ambiguity

The parser strips type prefixes from descriptions. Be explicit:

**Good**:
```r
#' @param mode Character string, processing mode
```
-> `description: "processing mode"`

**Avoid**:
```r
#' @param mode processing mode (character)
```
-> May not infer type correctly

## Enum and Default Values

### Function Formals

Define enums using default vectors:

```r
my_function <- function(
  format = c("json", "yaml", "csv"),
  mode = "auto"
) { ... }
```

**Singular parameter names** (format):
- Enum: `["json", "yaml", "csv"]`
- Default: `"json"` (first value)

**Plural parameter names** (formats):
- Would be treated as accepting multiple values
- No automatic default selection

### Override with Documentation

You can document defaults explicitly:

```r
#' @param timeout Integer, timeout in seconds. Default is 30
```

If the formal is `timeout = 30`, the parser extracts `default: 30`.

## Examples Best Practices

### Per-Parameter Examples

Add examples directly in parameter descriptions:

```r
#' @param pipeline_name Character string, pipeline identifier.
#'   Common values: \code{"power_explorer"}, \code{"wavelet_module"}
```

Use `\code{}` blocks for each example value.

### Multi-Line Examples

For complex examples like JSON:

```r
#' @param settings_json Character string, JSON configuration.
#'   Example: \code{"{
#'     \\"sampling_rate\\": 2000,
#'     \\"filter_order\\": 4
#'   }"}
```

The parser handles nested braces in `\code{}` blocks.

## Multi-Line Descriptions

Parameter descriptions can span multiple lines:

```r
#' @param complex_param This parameter has a long description
#'   that continues on the next line, and includes examples:
#'   \code{"example1"}, \code{"example2"}
#' @param next_param This is the next parameter
```

**Rules**:
- Continuation lines start with spaces and no `@tag`
- Ends when next `@tag` is encountered or end of comment block

## Common Patterns

### File Paths

```r
#' @param path Character string, absolute path to file.
#'   Example: \code{"/path/to/file.txt"}
```

### JSON Strings

```r
#' @param json_data Character string, JSON-formatted data.
#'   Example: \code{"{\\"key\\": \\"value\\"}"}
```

### Optional Parameters

```r
#' @param optional_param Character string, optional description.
#'   If not provided, defaults to \code{NULL}
```

If not in `required` field of formals, it won't be in YAML `required` array.

### Flags

```r
#' @param verbose Logical, whether to print progress messages.
#'   Default is \code{FALSE}
```

## Building and Loading Tools

### Generate YAML Files

```r
# Generate all MCP tool YAML files
mcptool_build()

# Output location: inst/mcp/tools/
```

### Tool Groups

Organize related tools into groups using the naming convention `mcp_tool_{group}_{action}`:

**Naming Pattern**:
```r
# Pipeline group tools
mcp_tool_pipeline_list <- function(...) { }
mcp_tool_pipeline_load <- function(...) { }
mcp_tool_pipeline_get_info <- function(...) { }

# Config group tools
mcp_tool_config_set_outputs <- function(...) { }
mcp_tool_config_set_options <- function(...) { }
```

**File naming**:
- `ravepipeline-mcp_tool_pipeline_list.yaml`
- `ravepipeline-mcp_tool_pipeline_load.yaml`
- `ravepipeline-mcp_tool_config_set_outputs.yaml`

**Benefits**:
- Logical organization by domain/category
- Easy filtering and discovery
- Clear naming hierarchy
- Improved maintainability

**Example groups**:
- `pipeline` - Pipeline management operations
- `config` - Configuration and settings
- `data` - Data access and manipulation
- `analysis` - Analysis and processing

### Load Tools for MCP Server

```r
# Load all tool definitions
tools <- mcptool_load_all("ravepipeline")

# Load only pipeline group tools
pipeline_tools <- mcptool_load_all("ravepipeline", groups = "pipeline")

# Load multiple groups
tools <- mcptool_load_all("ravepipeline", groups = c("pipeline", "config"))

# Returns a list of tool definitions suitable for MCP integration
```

### Tool Discovery

List available MCP tools:

```r
# Get all tool names
tool_names <- mcptool_list("ravepipeline")

# Get only pipeline group tools
pipeline_names <- mcptool_list("ravepipeline", groups = "pipeline")

# Get only config group tools
config_names <- mcptool_list("ravepipeline", groups = "config")

# List multiple groups
selected_tools <- mcptool_list("ravepipeline", groups = c("pipeline", "config"))
```

**Group filtering with regexp**:

The `groups` parameter accepts regexp patterns for flexible matching:

```r
# Match all pipeline "get" tools (get_info, get_script, get_helpers, etc.)
mcptool_list("ravepipeline", groups = "pipeline_get")

# Match pipeline or config groups using alternation
mcptool_list("ravepipeline", groups = "(pipeline|config)")

# Match any config-related tools
mcptool_list("ravepipeline", groups = "config")
```

## Testing Your Documentation

### Validation Checklist

- [ ] All parameters documented with `@param`
- [ ] Types clearly specified (Character string, Logical, Integer, etc.)
- [ ] Examples provided using `\code{}` blocks
- [ ] `@return` or `@returns` included if function returns value
- [ ] Required parameters match function signature
- [ ] Default values documented for optional parameters
- [ ] `@keywords` added for categorization and safety flags

### Manual Testing

1. **Build tools**:
   ```r
   mcptool_build()
   ```

2. **Check generated YAML**:
   ```r
   yaml_file <- system.file(
     "mcp/tools/ravepipeline-mcp_tool_{groupname}_{your_function}.yaml",
     package = "ravepipeline"
   )
   cat(readLines(yaml_file), sep = "\n")
   ```

3. **Validate schema**:
   - Check `type` fields are valid JSON Schema types
   - Verify `required` array matches function signature
   - Confirm examples are properly extracted
   - Ensure descriptions are clean (no type prefixes)

## Troubleshooting

### Type Not Inferred

**Problem**: Parameter shows `type: string` when it should be `number`

**Solution**: Add type keyword to description:
```r
#' @param count Integer, number of items
```

### Examples Not Extracted

**Problem**: `\code{}` examples don't appear in YAML

**Solution**: 
- Ensure `\code{}` braces are balanced
- Check for nested braces (parser handles them, but verify syntax)
- Place examples after type prefix

### Multi-Line Description Truncated

**Problem**: Only first line of parameter description appears

**Solution**: 
- Ensure continuation lines are indented
- No blank lines within parameter description
- Next `@tag` ends the description

### Enum Not Detected

**Problem**: Function has default vector but no `enum` in YAML

**Solution**:
- Check function formals use `c("value1", "value2")` syntax
- Verify parameter name is singular (plural names treated differently)
- Ensure formals are parseable (balanced parentheses)

## Advanced Topics

### Custom Type Mapping

If you need types beyond standard inference, explicitly state them:

```r
#' @param timestamp Integer, Unix timestamp in milliseconds
#' @param coordinates Numeric vector, [latitude, longitude] pair
```

### Nested Objects

For complex structured parameters:

```r
#' @param config List, configuration object with fields: 'mode'
#'   (character), 'threshold' (numeric), 'enabled' (logical)
```

This generates `type: object`. You can add structure in description.

### Conditional Parameters

Document parameter relationships:

```r
#' @param use_cache Logical, whether to use cached results
#' @param cache_dir Character string, cache directory. Only used
#'   if \code{use_cache = TRUE}
```

## Complete Workflow Example

1. **Write function with documentation**:
   ```r
   #' Analyze Pipeline Results
   #'
   #' @param pipeline_name Character string, pipeline identifier
   #' @param include_plots Logical, generate visualizations
   #' @return Analysis report with statistics and optional plots
   #' @keywords mcp-tool mcp-category-analysis
   #' @export
   analyze_results <- function(pipeline_name, include_plots = FALSE) {
     # Implementation
   }
   ```

2. **Build tools**:
   ```r
   mcptool_build()
   ```

3. **Verify output**:
   ```r
   tools <- mcptool_load_all()
   str(tools$analyze_results)
   ```

4. **Integrate with MCP server**:
   ```r
   # Tools are ready for MCP protocol integration
   ```

## Style Guidelines

- **Be concise**: Descriptions should be clear and brief
- **Use examples liberally**: Help users understand expected values
- **Document edge cases**: Mention special values (NULL, empty string, etc.)
- **Consistent terminology**: Use same terms across related functions
- **Update examples**: Keep examples current with function behavior
