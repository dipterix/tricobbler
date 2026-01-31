# Contract

Master user-orchestrator agreement that contains all stage contracts and
validates completeness against the manifest.

## Usage

``` r
Contract(
  manifest = Manifest(),
  stage_contracts = list(),
  global_tools = list(),
  global_context = list(),
  total_timeout_seconds = integer(0),
  total_budget_dollars = integer(0)
)
```

## Arguments

- manifest:

  `Manifest` object describing the workflow.

- stage_contracts:

  List of `StageContract` objects.

- global_tools:

  List of tools available globally.

- global_context:

  List of global context items.

- total_timeout_seconds:

  Numeric total timeout for the contract.

- total_budget_dollars:

  Numeric total budget for the contract.
