# Validated Dependency Declarations for State Policies

S7 class representing a named list of dependency declarations used by
`StatePolicy@depends_on`. Each entry maps a parameter name to a prior
state's output field.

## Usage

``` r
StateDeps(..., .list = list())
```

## Details

### Dependency Entry Format

Each entry in a `StateDeps` object is a named list element where:

- **Name**: The parameter name passed to the agent function

- **state**: (required) Name of the state to depend on

- **field**: (optional) What to extract - `"result"` (default) or
  `"description"`

- **stage**: (optional) Stage of the dependency. If `NULL` or omitted,
  the dependency must be in the same stage with higher priority. If
  specified, must be an earlier stage in the workflow

### Same-Stage vs Cross-Stage Dependencies

- **Same-stage** (stage = NULL): The dependent state must have higher
  priority (lower number = runs later). This ensures the dependency
  executes before the dependent within parallel priority groups

- **Cross-stage** (stage = "earlier_stage"): The dependency is in a
  previous stage. Since stages execute sequentially, the dependency is
  guaranteed to complete before the dependent stage begins

### Validation Rules (Property-Level)

The property-level validator checks structural correctness:

- Must be a named list (names become parameter names)

- Each entry must be a list with required `state` field

- `state` must be a single non-blank character string

- `field` (if present) must be `"result"` or `"description"`

- `stage` (if present) must be a single character string or NULL

- Parameter names must be valid R identifiers

### Cross-Validation (Manifest-Level)

Additional validation is performed at the `Manifest` level to check:

- Referenced states exist in the manifest

- Same-stage dependencies have higher priority

- Cross-stage dependencies reference earlier stages

## Examples

``` r
# Same-stage dependencies (state must have higher priority)
deps <- StateDeps(
  validation_result = list(state = "validator"),
  parsed_data = list(state = "parser", field = "result")
)

# Cross-stage dependency
deps2 <- StateDeps(
  plan = list(state = "planner", field = "result", stage = "planning")
)

# Empty dependencies (valid)
empty_deps <- StateDeps()
```
