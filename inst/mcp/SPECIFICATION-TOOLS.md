# RAVE MCP Tools YAML Specification

This document describes the YAML format specification for RAVE MCP (Model Context Protocol) tools generated from roxygen2 documentation.

## Overview

RAVE MCP tools are automatically generated from R function documentation using roxygen2 comments. The build system (`mcptool_build()`) parses roxygen2 tags and extracts metadata, parameters, types, examples, and other information to create JSON Schema-compliant YAML files.

Generated YAML files contain:
- Tool names, descriptions, and categories
- Parameter schemas (types, required, descriptions, examples)
- Return value schemas
- Safety annotations (dangerous tools, approval requirements)
- Implementation examples with execution timing

## YAML File Structure

Each MCP tool is defined in a separate YAML file with the following structure:

```yaml
name: function_name
description: Tool description
parameters:
  type: object
  properties:
    param1:
      type: string
      description: Parameter description
      enum: [option1, option2]  # Optional
      default: value            # Optional
      examples: [example1, example2]  # Optional
    param2:
      type: array
      items:
        type: string
      description: Array parameter
  required: [param1]
category: Category Name        # Optional
implementation_example: "..."  # Required
execution_time: 0.023          # Optional
returns:                       # Optional
  type: object
  properties:
    field1:
      type: string
      description: Description
```

## Field Descriptions

### Top-Level Fields

- **name** (required): The function name, exactly as defined in R
- **description** (required): Brief description of what the tool does (from `@description` or `@title`)
- **parameters** (required): JSON Schema object defining the function parameters
- **category** (optional): Tool category extracted from `@keywords mcp-category-NAME`
- **implementation_example** (required): Runnable R code examples with captured output from `@examples` section
- **execution_time** (optional): Time in seconds taken to execute the examples
- **returns** (optional): Return value schema from `@return` or `@returns` section

### Parameter Schema Fields

Each parameter in `parameters.properties` contains:

- **type** (required): JSON Schema type (string, number, integer, boolean, object, array)
- **description** (required): Clean parameter description with R type prefixes removed
- **enum** (optional): Array of valid values, extracted from function formals like `c("option1", "option2")`
- **default** (optional): Default value serialized to JSON format (NULL -> null, TRUE -> true, etc.)
- **examples** (optional): Array of example values extracted from `\code{}` blocks in parameter descriptions

### Array Types

For array parameters, use the `items` field:

```yaml
param_name:
  type: array
  items:
    type: string  # or number, integer, boolean, object
  description: Description
```

### Required Parameters

The `required` field is an array listing all mandatory parameters:

```yaml
required: [param1, param2, param3]
```

## Hidden Parameters

**Parameters starting with `.` are automatically excluded from MCP tool specifications.**

This feature allows passing internal context without exposing it to AI agents in the generated YAML.

### Use Case

The standard hidden parameter is `.state_env` for session state:

```r
#' @param query Character. Search query (exposed to AI)
#' @param .state_env Environment. Session state (hidden from AI)
```

When `mcptool_build()` processes this function, the generated YAML will only include the `query` parameter. The `.state_env` parameter is completely excluded from the MCP tool specification.

### State Environment Object

The `.state_env` parameter is typically an environment-like object provided by the MCP system with the following methods:

**Available methods**:
- `get(key, missing = NULL)` - Retrieve a value by key, return `missing` if not found
- `set(key, value)` - Store a value by key
- `has(key)` - Check if a key exists (returns logical)
- `remove(keys)` - Remove one or more keys
- `keys()` - Get all stored keys
- `reset()` - Clear all stored data
- `clear()` - Alias for `reset()`
- `size()` - Get number of stored items
- `as_list()` - Return all data as a list

**Usage in tools**:
```r
my_tool <- function(query, .state_env = NULL) {
  # Check cache
  if (!is.null(.state_env) && .state_env$has("cache")) {
    cached <- .state_env$get("cache")
    if (query %in% names(cached)) {
      return(cached[[query]])
    }
  }
  
  # Perform operation
  result <- expensive_operation(query)
  
  # Cache result
  if (!is.null(.state_env)) {
    .state_env$set("last_query", query)
    .state_env$set("last_result", result)
  }
  
  result
}
```

**Note**: The actual implementation of the state environment is provided by the MCP system (e.g., via `mcptool_state_factory()` or similar). Developers writing MCP tools should see [DEVELOPER_GUIDE.md](DEVELOPER_GUIDE.md) for how to debug.

### Generated YAML

```yaml
parameters:
  type: object
  properties:
    query:
      type: string
      description: Search query
  required: [query]
# .state_env is NOT included
```

## Type Inference Rules

The parser infers JSON Schema types from R documentation:

1. **Keywords** (case-insensitive):
   - "character", "string" -> `string`
   - "numeric", "number", "double" -> `number`
   - "integer" -> `integer`
   - "logical", "boolean" -> `boolean`
   - "list" -> `object`

2. **Array Notation**:
   - "Character[]", "character vector", "character array" -> `type: array, items: {type: string}`
   - "Numeric[]", "numeric vector" -> `type: array, items: {type: number}`
   - "Integer[]", "integer vector" -> `type: array, items: {type: integer}`

3. **Default Type**: If no keywords match, defaults to `string`

## Roxygen2 Markup Handling

The parser automatically cleans roxygen2/Rd markup:

- `\code{value}` -> `value`
- `\link{function}` -> `function`
- `\item{label}{text}` -> `label: text`
- LaTeX commands (e.g., `\eqn{}`, `\deqn{}`) are stripped

## Type Prefix Stripping

R-style type prefixes are removed from descriptions for cleaner API documentation:

**Removed prefixes**:
- "Character string,"
- "Character vector,"
- "Character[],"
- "Character,"
- "Logical,"
- "Integer,"
- "Numeric,"
- "Double,"
- "List,"
- "Data frame,"
- "Matrix,"
- "Array[],"
- "Array,"
- "Vector,"
- "Function,"
- "Environment,"

**Note**: Array notation in type field is preserved (e.g., `Character[]` becomes `type: array, items: {type: string}`)

## Keyword Metadata

Keywords are extracted from `@keywords` tags and must use the `mcp-` prefix:

- **Category**: `@keywords mcp-category-NAME` (e.g., `mcp-category-discovery`, `mcp-category-setup`)
- **Dangerous flag**: `@keywords mcp-dangerous`
- **Requires approval**: `@keywords mcp-requires-approval`
- **Tool marker**: `@keywords mcp-tool` (marks function as an MCP tool)

Example:
```r
#' @keywords mcp-tool mcp-category-pipeline_management mcp-requires-approval
```

## Example Extraction

### Per-Parameter Examples

Examples are extracted from parameter descriptions using `\code{}` blocks:

```r
#' @param pipeline_name Character string, the pipeline name.
#'   Example values: \code{"power_explorer"}, \code{"notch_filter"},
#'   \code{"wavelet_module"}
```

Results in:

```yaml
pipeline_name:
  type: string
  description: the pipeline name
  examples: ["power_explorer", "notch_filter", "wavelet_module"]
```

### Implementation Examples

Runnable code examples are extracted from `@examples` sections, executed, and their output is captured:

**In roxygen2 documentation**:
```r
#' @examples
#' # List available pipelines
#' pipelines <- ravepipeline::pipeline_list()
#' list(pipelines = pipelines, count = length(pipelines))
```

**Generated in YAML**:
```yaml
implementation_example: |
  # List available pipelines
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

**Processing**:
- All non-comment R code is extracted from `@examples`
- Code is executed in a clean environment
- Output is captured using `mcp_describe()` with truncation
- Combined code + output becomes `implementation_example` field
- Execution time is recorded in `execution_time` field

## Enum Detection

The parser detects enumeration values from function formals:

```r
my_function <- function(mode = c("summary", "details")) {
  # ...
}
```

Generates:

```yaml
mode:
  type: string
  enum: ["summary", "details"]
  default: "summary"
```

**Detection rules**:
- Must be a vector: `c("a", "b", "c")`
- Singular parameter name -> first value is default
- Plural parameter name -> all values are valid, no default

## Default Value Serialization

R values are converted to JSON-compatible format:

| R Value | JSON Value |
|---------|------------|
| `NULL` | `null` |
| `TRUE` | `true` |
| `FALSE` | `false` |
| `"string"` | `"string"` |
| `123` | `123` |
| `c("a", "b")` | `["a", "b"]` (when used as enum) |

## Multi-Line Parameter Support

Parameter descriptions can span multiple lines:

```r
#' @param long_param This is a very long parameter description
#'   that continues on the next line
#'   and even more lines
#' @param next_param Another parameter
```

The parser correctly collects all continuation lines before the next `@tag`.

## File Naming Convention

YAML files are named using the pattern:

```
{package}-{function_name}.yaml
```

Example: `ravepipeline-mcp_tool_pipeline_load.yaml`

Note: The tool name in the YAML uses hyphen format: `pkg-function_name` (e.g., `ravepipeline-mcp_tool_pipeline_load`). The first hyphen separates the package name from the function name.

## Tool Groups

MCP tools can be organized into groups using a structured naming convention:

```
{package}-mcp_tool_{group}_{action}.yaml
```

**Group Naming Pattern**: `mcp_tool_{group}_{action}`

- **group**: Category or domain (e.g., `pipeline`, `config`)
- **action**: Specific operation (e.g., `list`, `load`, `get_info`, `set_settings`)

**Examples**:
- Pipeline group: `ravepipeline-mcp_tool_pipeline_list.yaml`
- Config group: `ravepipeline-mcp_tool_config_set_outputs.yaml`

### Filtering Tools by Group

Tools can be filtered by group using the `groups` parameter in `mcptool_list()` and `mcptool_load_all()`:

```r
# List all pipeline group tools
mcptool_list("ravepipeline", groups = "pipeline")

# List multiple groups
mcptool_list("ravepipeline", groups = c("pipeline", "config"))

# Load only pipeline group tools
tools <- mcptool_load_all("ravepipeline", groups = "pipeline")
```

**Group Filtering**:
- The `groups` parameter accepts character vectors or NULL
- Each group can be a regexp pattern
- Pattern matching: `^[a-zA-Z0-9]+-mcp_tool_{group}`
- Multiple groups are combined (OR logic)
- NULL (default) returns all tools

**Example Use Cases**:
```r
# Get all pipeline management tools
mcptool_list("ravepipeline", groups = "pipeline")

# Get configuration tools only
mcptool_list("ravepipeline", groups = "config")

# Use regexp for partial matching
mcptool_list("ravepipeline", groups = "pipeline_get")
```

## Output Directory

All generated YAML files are stored in:

```
inst/mcp/tools/
```

## JSON Schema Compliance

All generated YAML files conform to JSON Schema Draft 7 specification:
- `type` field is required for all properties
- `enum` values must be arrays
- `required` must be an array
- Array types must include `items` specification
