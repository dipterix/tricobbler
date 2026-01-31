# StageContract

Defines the agreement for a single workflow stage, specifying who
(executor), what tools, and acceptance criteria.

## Usage

``` r
StageContract(
  state_policy = StatePolicy(),
  executor = ContractExecutor(),
  tools = list(),
  parameters = list(),
  output_schema = NULL,
  validator = NULL,
  max_retries = 3L,
  timeout_seconds = integer(0),
  fallback = NULL
)
```

## Arguments

- state_policy:

  `StatePolicy` object defining the state.

- executor:

  `ContractExecutor` object that runs the stage.

- tools:

  List of tools available to the stage.

- parameters:

  List of additional parameters for the executor.

- output_schema:

  `OutputSchema` describing expected output.

- validator:

  Function to validate output (optional).

- max_retries:

  Integer, max retry attempts.

- timeout_seconds:

  Numeric, timeout for the stage.

- fallback:

  Optional fallback `ContractExecutor`.
