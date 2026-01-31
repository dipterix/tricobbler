# State-Level Policy Implementation for Workflow Stages

Represents a single workflow state. Inherits from `BasePolicy` and adds
a mandatory `stage` that must match one of the stages defined in a
`MasterPolicy`. Includes priority and criticality flags for execution
ordering when multiple states share the same stage.

## Usage

``` r
StatePolicy(
  name = character(0),
  description = character(0),
  stage = character(0),
  parameters = list(),
  priority = 100L,
  critical = FALSE
)
```

## Arguments

- name:

  Character. Name of the state policy (non-blank).

- description:

  Character. Human-readable description.

- stage:

  Character. Must be a non-blank single string.

- parameters:

  List. Optional state-specific parameters.

- priority:

  Integer. Execution priority (0-999, default 100). Higher values run
  first (999 = highest priority, 0 = lowest). Used when multiple states
  share the same stage. NA or NULL are treated as 100.

- critical:

  Logical. If `TRUE`, states with lower priority won't execute if this
  state fails (default `FALSE`). Critical states must have
  `priority >= 1` and cannot share priority code with other states.

## Details

### Stages vs States

- **Stage** (macro): The workflow phase name from `MasterPolicy@stages`
  (e.g., "executing")

- **State** (micro): A concrete `StatePolicy` implementation of that
  stage

- Multiple states can reference the same stage for different execution
  patterns

### Priority System and Execution Patterns

When multiple states share the same stage, execution pattern depends on
priority:

- **Range**: 0 (lowest) to 999 (highest)

- **Default**: 100 (when `priority = NA` or `NULL`)

- **Equal priority**: States run in parallel (truly concurrent
  execution)

- **Different priorities**: States run sequentially (higher priority
  first)

### Critical Flag: Enforcing Sequential Execution

The `critical` flag enforces **sequential** execution with fail-fast
semantics:

- If `critical = TRUE`, this state **must** execute and succeed before
  any lower-priority states in the same stage can run

- If a critical state fails, lower-priority states are skipped entirely

- Critical states **prevent parallel execution** - you cannot have
  parallel states when one is marked critical (enforced by priority
  uniqueness)

- Critical states must have `priority >= 1` (cannot be lowest priority
  0)

- Critical states cannot share their priority value with other states in
  the same stage (enforced by `Manifest` validator)

- **Use case**: Required validation gates that must pass before
  alternatives run

### When to Use Multiple States Per Stage

Create multiple `StatePolicy` objects for the same stage when you need:

1.  **Parallel alternatives**: Multiple states with equal priority for
    concurrent execution (e.g., A/B testing, redundant processing)

2.  **Sequential fallback chains**: Different priorities create ordered
    execution with fallbacks (e.g., primary approach → fallback → last
    resort)

3.  **Critical validation gates**: Critical state must succeed before
    lower-priority alternatives execute (enforces sequential, fail-fast
    semantics)

4.  **Staged rollout**: Gradually shift priority as new implementations
    mature

## Examples

``` r
# Basic state
StatePolicy(
  name = "state1",
  stage = "idle",
  description = "initial idle state",
  parameters = list()
)
#> StatePolicy (S7 class) - `state1` (idle)
#> initial idle state

# Critical high-priority state
StatePolicy(
  name = "validator",
  stage = "executing",
  description = "critical validation step",
  priority = 900,
  critical = TRUE
)
#> StatePolicy (S7 class) - `validator` (executing)
#> critical validation step
```
