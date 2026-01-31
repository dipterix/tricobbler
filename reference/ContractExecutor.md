# ContractExecutor

Callable function that executes a stage. It wraps the actual execution
logic (LLM call, script, etc.) as a function. Inherits from
[`S7::class_function`](https://rconsortium.github.io/S7/reference/base_classes.html)
so the object itself is callable while still carrying additional
attributes.

## Usage

``` r
ContractExecutor(
  .data = function() NULL,
  name = "",
  description = "",
  metadata = list()
)
```

## Arguments

- .data:

  Internal data argument injected by S7 (unused).

- name:

  Name of the executor (character, length 1).

- description:

  Description of the executor (character).

- metadata:

  List of arbitrary metadata for the executor.
