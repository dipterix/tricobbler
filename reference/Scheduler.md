# Synchronous Workflow Scheduler

R6 class that orchestrates sequential execution of workflow stages and
states. Manages the `lifecycle` of agent runtimes through priority-based
dispatch, dependency resolution, retry logic, and critical-state
suspension. This is the synchronous (blocking) scheduler; see
[`AsyncScheduler`](http://dipterix.org/tricobbler/reference/AsyncScheduler.md)
for the promise-based variant.

## Public fields

- `manifest`:

  [`Manifest`](http://dipterix.org/tricobbler/reference/Manifest.md),
  the workflow blueprint

- `agents`:

  [`fastmap::fastmap()`](https://r-lib.github.io/fastmap/reference/fastmap.html)
  object, registry of
  [`Agent`](http://dipterix.org/tricobbler/reference/Agent.md) objects
  keyed by `agent_id`

- `context`:

  [`AgentContext`](http://dipterix.org/tricobbler/reference/AgentContext.md),
  the execution environment for logging and storage

- `current_stage`:

  character, the currently executing stage name

- `stage_started`:

  `POSIXct` or `NULL`, timestamp when the current stage started

- `suspended`:

  logical, whether execution is paused

- `suspend_info`:

  list or `NULL`, context captured when the scheduler suspends
  (state_name, stage, error). Cleared on resume/skip.

- `draining`:

  logical, when `TRUE` a `final` state has completed; no new runtimes
  are dispatched and any remaining queued items are abandoned

- `max_concurrency`:

  integer, maximum number of states to dispatch per advance cycle
  (default: `100L`)

- `runtime_map`:

  [`fastmap::fastmap()`](https://r-lib.github.io/fastmap/reference/fastmap.html)
  object, all runtimes for the current stage before joining the ready
  queue

- `ready_queue`:

  [`fastmap::fastqueue()`](https://r-lib.github.io/fastmap/reference/fastqueue.html)
  object, priority-sorted queue of runtimes ready to execute

- `waiting_pool`:

  [`fastmap::fastmap()`](https://r-lib.github.io/fastmap/reference/fastmap.html)
  object, tracks currently executing runtimes

- `completed_map`:

  [`fastmap::fastmap()`](https://r-lib.github.io/fastmap/reference/fastmap.html)
  object, maps state names to completion records with status
  (`"finished"`, `"errored"`, or `"skipped"`)

- `retry_map`:

  [`fastmap::fastmap()`](https://r-lib.github.io/fastmap/reference/fastmap.html)
  object, maps state names to integer attempt counts for retry
  scheduling

## Methods

### Public methods

- [`Scheduler$new()`](#method-TricobblerScheduler-new)

- [`Scheduler$on()`](#method-TricobblerScheduler-on)

- [`Scheduler$off()`](#method-TricobblerScheduler-off)

- [`Scheduler$dispatch_event()`](#method-TricobblerScheduler-dispatch_event)

- [`Scheduler$validate()`](#method-TricobblerScheduler-validate)

- [`Scheduler$stop()`](#method-TricobblerScheduler-stop)

- [`Scheduler$start()`](#method-TricobblerScheduler-start)

- [`Scheduler$init_resources()`](#method-TricobblerScheduler-init_resources)

- [`Scheduler$init_stage()`](#method-TricobblerScheduler-init_stage)

- [`Scheduler$enqueue_runtime()`](#method-TricobblerScheduler-enqueue_runtime)

- [`Scheduler$execute_runtime()`](#method-TricobblerScheduler-execute_runtime)

- [`Scheduler$retry_runtime()`](#method-TricobblerScheduler-retry_runtime)

- [`Scheduler$start_stage()`](#method-TricobblerScheduler-start_stage)

- [`Scheduler$get_incomplete_size()`](#method-TricobblerScheduler-get_incomplete_size)

- [`Scheduler$advance()`](#method-TricobblerScheduler-advance)

- [`Scheduler$run_stage()`](#method-TricobblerScheduler-run_stage)

- [`Scheduler$suspend()`](#method-TricobblerScheduler-suspend)

------------------------------------------------------------------------

### Method `new()`

Initialize scheduler with manifest blueprint and agents

#### Usage

    Scheduler$new(manifest, agents = list(), context = AgentContext$new())

#### Arguments

- `manifest`:

  [`Manifest`](http://dipterix.org/tricobbler/reference/Manifest.md)
  object, the workflow blueprint

- `agents`:

  list, collection of
  [`Agent`](http://dipterix.org/tricobbler/reference/Agent.md) objects

- `context`:

  [`AgentContext`](http://dipterix.org/tricobbler/reference/AgentContext.md)
  object, execution environment (default: new
  [`AgentContext`](http://dipterix.org/tricobbler/reference/AgentContext.md))

------------------------------------------------------------------------

### Method `on()`

Register a listener for a `lifecycle` event

#### Usage

    Scheduler$on(type, handler, id = NULL, after = TRUE)

#### Arguments

- `type`:

  character, event type (e.g., `"suspend"`, `"runtime.resolved"`,
  `"stage.completed"`, `"runtime.dispatch"`)

- `handler`:

  function, callback receiving the event list. For `"suspend"` events
  the handler may return an action string (`"resume"`, `"skip"`,
  `"abort"`, or `"restart_stage"`).

- `id`:

  character or `NULL`, optional listener ID for replacement or removal;
  auto-generated via `digest::digest(handler)` when `NULL`

- `after`:

  logical, if `TRUE` (default) append the handler after existing
  handlers; if `FALSE` prepend it

#### Returns

character, the listener ID (invisibly)

------------------------------------------------------------------------

### Method `off()`

Remove a registered `lifecycle` listener by ID

#### Usage

    Scheduler$off(id)

#### Arguments

- `id`:

  character, the listener ID returned by `$on()`

------------------------------------------------------------------------

### Method `dispatch_event()`

Emit a `lifecycle` event to registered listeners

#### Usage

    Scheduler$dispatch_event(type, message = type, ...)

#### Arguments

- `type`:

  character, event type identifier

- `message`:

  character, human-readable event description

- `...`:

  additional named fields attached to the event

------------------------------------------------------------------------

### Method `validate()`

Verify that all required agents are registered

#### Usage

    Scheduler$validate(on_error = c("error", "quiet"))

#### Arguments

- `on_error`:

  character, action on validation failure: `"error"` (throw an error) or
  `"quiet"` (return `FALSE` silently)

------------------------------------------------------------------------

### Method [`stop()`](https://rdrr.io/r/base/stop.html)

Stop the workflow execution, clearing all in-progress work and
invalidating outstanding promises. After calling
[`stop()`](https://rdrr.io/r/base/stop.html), the scheduler is in the
`"ready"` state and [`start()`](https://rdrr.io/r/stats/start.html) may
be called immediately.

#### Usage

    Scheduler$stop()

------------------------------------------------------------------------

### Method [`start()`](https://rdrr.io/r/stats/start.html)

Start the workflow execution

#### Usage

    Scheduler$start(debug = FALSE)

#### Arguments

- `debug`:

  logical, whether to enable verbose agent-call output

------------------------------------------------------------------------

### Method `init_resources()`

Initialize resources and prepare for execution

#### Usage

    Scheduler$init_resources(reset_context = FALSE, debug = FALSE)

#### Arguments

- `reset_context`:

  logical, whether to reset the context storage

- `debug`:

  logical, whether to enable verbose agent-call output

------------------------------------------------------------------------

### Method `init_stage()`

Initialize a single stage by creating runtimes for all state policies in
that stage

#### Usage

    Scheduler$init_stage(stage)

#### Arguments

- `stage`:

  character, stage name to initialize

------------------------------------------------------------------------

### Method `enqueue_runtime()`

Move ready runtimes from `runtime_map` to the `ready_queue` based on
dependency satisfaction

#### Usage

    Scheduler$enqueue_runtime()

------------------------------------------------------------------------

### Method `execute_runtime()`

Dispatch runtimes from `ready_queue` to `waiting_pool` for execution (up
to `max_concurrency`)

#### Usage

    Scheduler$execute_runtime()

#### Returns

integer, number of runtimes dispatched

------------------------------------------------------------------------

### Method `retry_runtime()`

Re-`enqueue` runtimes from `retry_map` back into `runtime_map` with
incremented attempt counts

#### Usage

    Scheduler$retry_runtime()

------------------------------------------------------------------------

### Method `start_stage()`

Prepare and validate a stage before execution

#### Usage

    Scheduler$start_stage(stage)

#### Arguments

- `stage`:

  character, stage name to start

------------------------------------------------------------------------

### Method `get_incomplete_size()`

Count incomplete work items for the current stage

#### Usage

    Scheduler$get_incomplete_size()

#### Returns

integer, total items across runtime_map, ready_queue, waiting_pool, and
retry_map

------------------------------------------------------------------------

### Method `advance()`

Drive the dispatch loop for the current stage.

#### Usage

    Scheduler$advance()

#### Details

Loops synchronously: retry, `enqueue`, then execute; then checks for
stage completion. Continues until no more work remains or the stage is
`cancelled` or suspended.

------------------------------------------------------------------------

### Method `run_stage()`

Execute a single workflow stage using the queue + waiting-pool dispatch
model

#### Usage

    Scheduler$run_stage(stage)

#### Arguments

- `stage`:

  character, the stage name to execute

------------------------------------------------------------------------

### Method `suspend()`

Suspend the workflow execution

#### Usage

    Scheduler$suspend(
      error = NULL,
      state_name = NA_character_,
      stage = self$current_stage,
      runtime_summary = NULL
    )

#### Arguments

- `error`:

  condition or character, the error that caused suspension (default:
  `private$.last_error`)

- `state_name`:

  character, the state that caused suspension

- `stage`:

  character, the stage in which suspension occurred (default:
  `self$current_stage`)

- `runtime_summary`:

  list or `NULL`, lightweight summary of the runtime that failed
  (policy, agent, attempt, attachment_id). Used by `"resume"` to
  re-create the runtime and by `"skip"` to record the skipped state.

#### Returns

character, the chosen action (invisibly)
