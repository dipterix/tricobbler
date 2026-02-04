# Get the Active Context

Retrieves the currently active `AgentContext` from the global state. If
no context is explicitly activated, returns a default context that is
automatically initialized.

## Usage

``` r
get_active_context()
```

## Value

An `AgentContext` object representing the current execution context.
