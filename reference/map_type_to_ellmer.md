# Convert Type Definitions to ellmer Type Indicators

Converts JSON Schema type definitions (from YAML or lists) to ellmer
type indicator objects for use with `chat$chat_structured()`.

## Usage

``` r
map_type_to_ellmer(type_def)
```

## Arguments

- type_def:

  A list representing a JSON Schema type definition, or a character
  string specifying a simple type (e.g., "string", "integer"). Supported
  types: "string", "integer", "number", "boolean", "array", "object".
  Arrays can specify `items` for element types. Objects can specify
  `properties` for nested structure.

## Value

An ellmer type indicator object (e.g.,
[`ellmer::type_string()`](https://ellmer.tidyverse.org/reference/type_boolean.html),
[`ellmer::type_object()`](https://ellmer.tidyverse.org/reference/type_boolean.html)).

## See also

[`StatePolicy`](http://dipterix.org/tricobbler/reference/StatePolicy.md)
for how `return_type` uses this function,
[`type_string`](https://ellmer.tidyverse.org/reference/type_boolean.html)
and related functions for ellmer type indicators

## Examples

``` r
if (FALSE) { # \dontrun{
# Simple string type
map_type_to_ellmer("string")

# Complex object type from YAML-like definition
map_type_to_ellmer(list(
  type = "object",
  properties = list(
    name = list(type = "string", description = "User name"),
    age = list(type = "integer", description = "User age")
  )
))
} # }
```
