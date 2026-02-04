# Workflow Execution Scheduler

R6 class that orchestrates workflow execution by managing stage
progression, state execution, and agent coordination. This is the
primary runtime component in the Runtime Layer (Tier 2) that brings
together the immutable policy definitions (`Manifest`) with executable
agents.

## Public fields

- `manifest`:

  Manifest, the workflow blueprint

- `agents`:

  [`fastmap::fastmap()`](https://r-lib.github.io/fastmap/reference/fastmap.html)
  object, registry of Agent objects keyed by agent_id

- `context`:

  Context, the execution environment for logging and storage

- `current_stage`:

  character, the currently executing stage name

- `stage_started`:

  `POSIXct`, timestamp when current stage started

- `current_state`:

  character, the currently executing state name

- `state_started`:

  `POSIXct`, timestamp when current state started

- `suspended`:

  logical, whether execution is paused

## Methods

### Public methods

- [`Scheduler$new()`](#method-TricobblerScheduler-new)

- [`Scheduler$validate()`](#method-TricobblerScheduler-validate)

- [`Scheduler$start()`](#method-TricobblerScheduler-start)

- [`Scheduler$init_resources()`](#method-TricobblerScheduler-init_resources)

- [`Scheduler$run_stage()`](#method-TricobblerScheduler-run_stage)

- [`Scheduler$suspend()`](#method-TricobblerScheduler-suspend)

- [`Scheduler$stop()`](#method-TricobblerScheduler-stop)

------------------------------------------------------------------------

### Method `new()`

Initialize scheduler with manifest blueprint and agents

#### Usage

    Scheduler$new(manifest, agents = list(), context = AgentContext$new())

#### Arguments

- `manifest`:

  Manifest object, the workflow blueprint

- `agents`:

  list, collection of Agent objects

- `context`:

  Context object, execution environment (default: new
  [`AgentContext`](http://dipterix.org/tricobbler/reference/AgentContext.md))

------------------------------------------------------------------------

### Method `validate()`

Verify that all required agents are registered

#### Usage

    Scheduler$validate(on_error = c("error", "quiet"))

#### Arguments

- `on_error`:

  character, action on validation failure ("error" or "quiet")

------------------------------------------------------------------------

### Method [`start()`](https://rdrr.io/r/stats/start.html)

Start the workflow execution from the ready stage

#### Usage

    Scheduler$start()

------------------------------------------------------------------------

### Method `init_resources()`

Initialize resources and prepare for execution

#### Usage

    Scheduler$init_resources(reset_context = FALSE)

#### Arguments

- `reset_context`:

  logical, whether to reset the context storage

------------------------------------------------------------------------

### Method `run_stage()`

Execute a specific workflow stage

#### Usage

    Scheduler$run_stage(stage = NULL)

#### Arguments

- `stage`:

  character, stage name (NULL to run next stage)

------------------------------------------------------------------------

### Method `suspend()`

Suspend the workflow execution

#### Usage

    Scheduler$suspend()

------------------------------------------------------------------------

### Method [`stop()`](https://rdrr.io/r/base/stop.html)

Stop the workflow execution

#### Usage

    Scheduler$stop()
