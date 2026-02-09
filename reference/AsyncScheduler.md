# `Async` Workflow Execution Scheduler

R6 class that extends
[`Scheduler`](http://dipterix.org/tricobbler/reference/Scheduler.md)
with promise-based asynchronous execution. Within each stage,
independent states are dispatched concurrently via
`AgentRuntime$run_async()`, bounded by `max_concurrency`. Stages are
executed sequentially.

## Details

### `Async` Dispatch Cycle

Each stage follows a promise-driven dispatch loop:

1.  [`start()`](https://rdrr.io/r/stats/start.html) validates the
    manifest, initializes resources, and iterates through stages
    sequentially using
    [`coro::async`](https://coro.r-lib.org/reference/async.html)

2.  `run_stage(stage)` initializes runtimes for all state policies in
    the stage, creates a stage-level promise, and calls `advance()`

3.  `advance()` drives the cycle: retry failed runtimes, `enqueue` ready
    runtimes, then dispatch up to `max_concurrency` concurrent
    executions

4.  When a runtime settles (fulfills or rejects), its promise callback
    processes the result (success, retry, redirect, or suspend) and
    calls `advance()` again

5.  When no incomplete work remains, `advance()` resolves the stage
    promise and the next stage begins

### Differences from [`Scheduler`](http://dipterix.org/tricobbler/reference/Scheduler.md)

- [`start()`](https://rdrr.io/r/stats/start.html) returns a
  [`promises::promise`](https://rstudio.github.io/promises/reference/promise.html)
  instead of blocking

- `execute_runtime()` dispatches multiple runtimes concurrently (up to
  `max_concurrency`)

- [`stop()`](https://rdrr.io/r/base/stop.html) rejects the active stage
  promise in addition to resetting state

- `suspend()` resolves actions via the event dispatcher and manipulates
  promise settlement functions

## Super class

`tricobbler::TricobblerScheduler` -\> `TricobblerAsyncScheduler`

## Methods

### Public methods

- [`AsyncScheduler$stop()`](#method-TricobblerAsyncScheduler-stop)

- [`AsyncScheduler$start()`](#method-TricobblerAsyncScheduler-start)

- [`AsyncScheduler$execute_runtime()`](#method-TricobblerAsyncScheduler-execute_runtime)

- [`AsyncScheduler$advance()`](#method-TricobblerAsyncScheduler-advance)

- [`AsyncScheduler$run_stage()`](#method-TricobblerAsyncScheduler-run_stage)

- [`AsyncScheduler$suspend()`](#method-TricobblerAsyncScheduler-suspend)

Inherited methods

- [`tricobbler::TricobblerScheduler$dispatch_event()`](http://dipterix.org/tricobbler/reference/TricobblerScheduler.html#method-dispatch_event)
- [`tricobbler::TricobblerScheduler$enqueue_runtime()`](http://dipterix.org/tricobbler/reference/TricobblerScheduler.html#method-enqueue_runtime)
- [`tricobbler::TricobblerScheduler$get_incomplete_size()`](http://dipterix.org/tricobbler/reference/TricobblerScheduler.html#method-get_incomplete_size)
- [`tricobbler::TricobblerScheduler$init_resources()`](http://dipterix.org/tricobbler/reference/TricobblerScheduler.html#method-init_resources)
- [`tricobbler::TricobblerScheduler$init_stage()`](http://dipterix.org/tricobbler/reference/TricobblerScheduler.html#method-init_stage)
- [`tricobbler::TricobblerScheduler$initialize()`](http://dipterix.org/tricobbler/reference/TricobblerScheduler.html#method-initialize)
- [`tricobbler::TricobblerScheduler$off()`](http://dipterix.org/tricobbler/reference/TricobblerScheduler.html#method-off)
- [`tricobbler::TricobblerScheduler$on()`](http://dipterix.org/tricobbler/reference/TricobblerScheduler.html#method-on)
- [`tricobbler::TricobblerScheduler$retry_runtime()`](http://dipterix.org/tricobbler/reference/TricobblerScheduler.html#method-retry_runtime)
- [`tricobbler::TricobblerScheduler$start_stage()`](http://dipterix.org/tricobbler/reference/TricobblerScheduler.html#method-start_stage)
- [`tricobbler::TricobblerScheduler$validate()`](http://dipterix.org/tricobbler/reference/TricobblerScheduler.html#method-validate)

------------------------------------------------------------------------

### Method [`stop()`](https://rdrr.io/r/base/stop.html)

Stop the workflow execution, clearing all in-progress work and rejecting
the active stage promise. After calling
[`stop()`](https://rdrr.io/r/base/stop.html), the scheduler is in the
`"ready"` state and [`start()`](https://rdrr.io/r/stats/start.html) may
be called immediately.

#### Usage

    AsyncScheduler$stop()

------------------------------------------------------------------------

### Method [`start()`](https://rdrr.io/r/stats/start.html)

Start the workflow execution

#### Usage

    AsyncScheduler$start()

#### Returns

A `promise` that resolves when all stages complete. Callers may block
via [`later::run_now()`](https://later.r-lib.org/reference/run_now.html)
or `await()` inside a
[`coro::async`](https://coro.r-lib.org/reference/async.html) context.

------------------------------------------------------------------------

### Method `execute_runtime()`

Dispatch queued runtimes as `async` promises.

#### Usage

    AsyncScheduler$execute_runtime()

#### Returns

integer, number of runtimes dispatched (invisibly)

------------------------------------------------------------------------

### Method `advance()`

Drive the next `async` dispatch cycle.

#### Usage

    AsyncScheduler$advance()

#### Details

Called by promise callbacks when a runtime settles. Runs: retry,
`enqueue`, then execute. If no incomplete work remains, resolves the
stage promise.

------------------------------------------------------------------------

### Method `run_stage()`

Execute a single workflow stage asynchronously.

#### Usage

    AsyncScheduler$run_stage(stage)

#### Arguments

- `stage`:

  character, the stage name to execute

#### Returns

A promise that resolves when all states in the stage have completed

------------------------------------------------------------------------

### Method `suspend()`

Suspend the workflow execution (`async` version).

#### Usage

    AsyncScheduler$suspend(
      error = NULL,
      state_name = NA_character_,
      stage = self$current_stage,
      runtime_summary = NULL
    )

#### Arguments

- `error`:

  condition or character, the error that caused suspension

- `state_name`:

  character, the state that caused suspension

- `stage`:

  character, the stage in which suspension occurred

- `runtime_summary`:

  list or `NULL`, lightweight summary of the failed runtime

#### Returns

character, the chosen action (invisibly)
