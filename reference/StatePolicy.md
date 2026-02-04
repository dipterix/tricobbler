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
  agent_id = character(0),
  resources = character(0),
  parameters = list(),
  max_retry = 0L,
  priority = 100L,
  critical = FALSE,
  final = FALSE,
  on_failure = NA_character_
)
```

## Arguments

- name:

  character, name of the state policy (non-blank)

- description:

  character, human-readable description

- stage:

  character, must be a non-blank single string

- agent_id:

  character, unique identifier for the agent responsible for executing
  this state. Must contain only letters, digits, underscores, or dashes

- resources:

  character vector, tools that the agent may call during execution
  (default empty vector)

- parameters:

  list, optional state-specific parameters

- max_retry:

  integer, maximum total attempts (initial + retries) for this state
  during stage execution (default 0, meaning single attempt). The
  `max_retry` limit applies globally across all re-entries to this state
  within the same stage. If `on_failure` is set, this state will not
  retry locally but will jump to the failure handler immediately. If
  `on_failure` is NA, local retries up to `max_retry` will be attempted
  before moving to next state

- priority:

  integer, execution priority (0-999, default 100). Higher values run
  first (999 = highest priority, 0 = lowest). Used when multiple states
  share the same stage. NA or NULL are treated as 100

- critical:

  logical, if TRUE, states with lower priority will not execute if this
  state fails (default FALSE). Critical states must have priority \>= 1
  and cannot share priority code with other states

- final:

  logical, if TRUE and validation succeeds, skip remaining states in the
  workflow (default FALSE)

- on_failure:

  character, name of the state to jump to on first failure (default NA
  to retry locally up to `max_retry` times). When set, failures trigger
  immediate jump to the specified state without local retries. The
  `max_retry` limit still applies globally to prevent infinite loops: if
  this state is re-entered and total attempts exceed `max_retry`,
  execution stops with an error. Common patterns: validation loops
  (`on_failure = "executor"`), alternative strategies
  (`on_failure = "slower_alternative"`), or repair chains
  (`on_failure = "repair_step"`)

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

- **Execution order**: States run sequentially by priority (higher
  first)

### Critical Flag: Enforcing Sequential Execution

The `critical` flag enforces **sequential** execution with fail-fast
semantics:

- If `critical = TRUE`, this state **must** execute and succeed before
  any lower-priority states in the same stage can run

- If a critical state fails, lower-priority states are skipped entirely

- Critical states must have unique priority (enforced by `Manifest`)

- Critical states must have `priority >= 1` (cannot be lowest priority
  0)

- Critical states cannot share their priority value with other states in
  the same stage (enforced by `Manifest` validation)

- **Use case**: Required validation gates that must pass before
  alternatives run

### When to Use Multiple States Per Stage

Create multiple `StatePolicy` objects for the same stage when you need:

1.  **Fallback chains**: Different priorities create ordered execution
    with alternative strategies

2.  **Alternative implementations**: Multiple states for the same stage
    execution with alternative strategies (e.g., primary approach -\>
    fallback -\> last resort)

3.  **Critical validation gates**: Critical state must succeed before
    lower-priority alternatives execute (enforces sequential, fail-fast
    semantics)

4.  **Phased deployment**: Gradually shift priority as new
    implementations mature

## Examples

``` r
# Basic state
StatePolicy(
  name = "state1",
  stage = "idle",
  description = "initial idle state",
  agent_id = "agent1",
  parameters = list()
)
#> StatePolicy (S7 class) - `state1` (idle)
#> initial idle state

# Critical high-priority state
StatePolicy(
  name = "validator",
  stage = "executing",
  description = "critical validation step",
  agent_id = "validator_agent",
  priority = 900,
  critical = TRUE
)
#> StatePolicy (S7 class) - `validator` (executing)
#> critical validation step
```
