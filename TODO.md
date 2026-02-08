# TriCobbler Async Architecture TODO

This document outlines the implementation plan for async/parallel execution support in tricobbler.

## Overview

**Goal:** Enable concurrent agent execution using R promises, allowing multiple LLM calls to run simultaneously without blocking. Each agent should return a `promise` object

**Architecture Decision:** Use `coro` + `promises` (not `future`) because:
- LLM agents are I/O-bound (waiting for API responses), not CPU-bound
- Single-process async avoids serialization overhead of multi-process approaches
- `ellmer::Chat` supports `chat$chat_async()`, `chat$stream_async()`, `chat$chat_structured_async()` returning promises
- `chat$clone()` is supported for parallel Chat agent isolation
- Simpler thread-safety model (single event loop vs file locking)

---

## Phase 1: Foundation - Explicit Dependencies [Done]

**Why:** Current `last_results(1)` returns chronologically last result. With parallel execution, result order is non-deterministic. Agents must explicitly declare which states they depend on.

### 1.1 Add `depends_on` Property to StatePolicy

**Location:** `R/class-policy.R`

**Steps:**
1. Add new S7 property to `StatePolicy`:
   ```r
   depends_on = S7::new_property(
     class = StateDeps,  # Implemented as separate S7 class
     default = StateDeps()
   )
   ```
2. Each dependency entry should be a named list element (key = parameter name):
   - `state`: Name of the state this depends on (character)
   - `field`: What to extract - `"result"` or `"description"` (character)
   - `stage`: (Optional) Stage name if depending on earlier stage
3. Add validator to `StateDeps` to ensure entries have required fields (format)
4. Add validator to property `states` in `Manifest` to ensure the dependency entries are valid (logic)
5. Update `manifest_write()` and `manifest_read()` for serialization

**Example manifest:**
```yaml
states:
  - name: aggregator
    stage: executing
    priority: 50
    agent_id: aggregator_agent
    accessibility: explicit
    depends_on:
      validation_result:
        state: validator
        field: result
      parsed_data:
        state: parser
        field: result
```

### 1.2 Add `accessibility = "explicit"` Level

**Location:** `R/class-policy.R`, `R/class-baseagent.R`

**Steps:**
1. Update `accessibility` property validator to accept `"explicit"` value
2. Modify `as_agent()` wrapper to handle `"explicit"` accessibility:
   - Build inputs from `depends_on` mapping instead of `last_results()` (for deterministic agents such as functions or MCP tools)
   - For AI agents, include the `depends_on` as its chat context (into user prompts, see item 3)
   - Use new `get_attachment_by_state()` method (see 1.3)
3. Keep `"all"`, `"logs"`, `"none"` working:
   - "none": agent have no access to logs nor attachment; under this situation, `depends_on` only indicates that this agent should await for the dependents to finish, but cannot access their contents
   - "logs": agent have access to logs and attachment list but not contents; under this situation, `depends_on` only indicates that this agent should await for the dependents to finish, but cannot access their contents
   - "explicit": agent have access to logs, attachment list, but only with the attachments from `depends_on` list. The MCP tool to read attachments will not be provided. However, all the `depends_on` descriptions will be included in the user prompt
   - "all": agent will be provided tools to access all logs and attachments, the user prompt also includes `depends_on` descriptions

### 1.3 Add `get_attachment_by_state()` to Context

**Location:** `R/class-context.R`

**Steps:**
1. Add new public method:
   ```r
   get_attachment_by_state = function(state, stage) {
     # Find attachment matching state and stage
     # Return most recent match if multiple exist (retries)
   }
   ```
2. Use `private$.results` index to locate attachment by state name
3. Load and return the attachment object

---

## Phase 2: Globals Isolation [Done]

**Why:** Current globals (`active_context`, `active_agent`, `active_policy`) in `R/aaa.R` are shared across all agents. With parallel execution, concurrent agents overwrite each other's globals, causing MCP tools to receive wrong context.

### 2.1 Create AgentRuntime Class

**Location:** `R/class-runtime.R`

**Implementation:**
1. Created `AgentRuntime` R6 class with per-execution state:
   ```r
   AgentRuntime <- R6::R6Class(
     private = list(
       .agent = NULL,        # Current Agent (who)
       .context = NULL,      # AgentContext (where)
       .policy = NULL,       # StatePolicy (what)
       .execution_id = NULL  # Unique ID for this execution
     ),
     active = list(
       agent = function() { private$.agent },
       context = function() { private$.context },
       policy = function() { private$.policy },
       execution_id = function() { private$.execution_id }
     ),
     public = list(
       logger = function(...) { ... },  # Convenience method
       run = function(attempt) { ... },       # Sync execution
       run_async = function(attempt) { ... }  # Async execution
     )
   )
   ```
2. Runtime is instantiated per-execution in Scheduler and passed to agents
3. Encapsulates execution logic with `run()` and `run_async()` methods using `coro::async`/`await`

### 2.2 Update MCP Tools for Explicit Runtime

**Location:** `R/mcp-tooldef-config.R`, `R/mcp-tooldef-context.R`, `R/mcp-tools.R`

**Implementation:**
1. Added `.runtime` parameter to all MCP tools:
   ```r
   mcp_tool_context_logs_tail <- function(max_lines, skip_lines, .runtime = NULL) {
     ctx <- if (!is.null(.runtime)) .runtime$context else NULL
     # ...
   }
   ```
2. `mcptool_instantiate()` detects `.runtime` parameter via `formals()` inspection
3. Runtime is injected via closure capture at tool instantiation time:
   ```r
   mcptool_instantiate <- function(tool, ..., runtime = NULL) {
     has_runtime_param <- ".runtime" %in% names(formals(impl))
     wrapper_fun <- function() {
       if (has_runtime_param && !is.null(runtime)) {
         call[[".runtime"]] <- runtime
       }
       # ...
     }
   }
   ```
4. Removed globals infrastructure (`set_globals`, `get_globals`, `get_active_context`)

### 2.3 Update Agent Function Signature

**Location:** `R/class-baseagent.R`

**Implementation:**
1. Agent functions now require `runtime` as the first parameter:
   ```r
   function(runtime, ...) {
     # runtime$agent - the Agent object itself
     # runtime$policy - the StatePolicy being executed
     # runtime$context - the Context for logging
     # runtime$logger() - shorthand for logging
   }
   ```
2. Validator enforces `runtime` as first argument name
3. Scheduler creates runtime and calls `runtime$run()`, records results via `do.call()`

---

## Phase 3: Per-Runtime Logging & Atomic Attachments [Done]

**Why:** Each `AgentRuntime` owns its execution lifecycle completely—one runtime = one attempt = one attachment. Per-runtime log files enable crash inspection and clean separation of concerns without file locking (unnecessary since R promises/coro are single-threaded).

**Key insight:** One `AgentRuntime` = one execution attempt = one attachment. The `attachment_id` (format `[stage][state][agent_id]_YYMMDDTHHMMSS_{attempt}`) is the primary key across log files, `.rds` attachments, and the SQLite index.

### 3.1 Move `attempt` to AgentRuntime Initializer [Done]

**Location:** `R/class-runtime.R`

**Implementation:**
1. `AgentRuntime$new(agent, context, policy, attempt = 0L)` — attempt is validated and stored at construction
2. `attachment_id` is generated immediately in the constructor:
   ```r
   private$.attachment_id <- sprintf(
     "[%s][%s][%s]_%s_%d",
     policy@stage, policy@name, agent@id,
     format(now, "%y%m%dT%H%M%S"), attempt
   )
   ```
3. Runtime registers itself in `AttachmentIndex` with status `"init"` at construction
4. `run()` and `run_async()` take no arguments — attempt is already known
5. Scheduler creates a fresh `AgentRuntime` per retry attempt, passing the cumulative retry count

### 3.2 Per-Runtime Log File [Done]

**Location:** `R/class-runtime.R`

**Implementation:**
1. `$logger(...)` writes to `{attachment_path}/{attachment_id}.log` via `log_to_file()`, and optionally mirrors to the main context log when running in the main process (`Sys.getpid()` check)
2. `$logger()` supports `level`, `verbose`, `public`, and `role` parameters — reuses the shared `log_to_file()` helper from `R/helper-logger.R`
3. No separate `$get_logs()` on runtime — context provides `$get_runtime_log(attachment_id)` instead

### 3.3 Timing Capture & Result Recording [Done]

**Location:** `R/class-runtime.R`

**Implementation:**
1. `private$.create_run_impl()` captures `time_started <- Sys.time()` at execution start and passes `started` and `duration` to `.record_result()`
2. `private$.record_result(result, succeed, ...)` handles all result finalization:
   - Calls `agent@describe(result)` for human-readable description
   - Saves `.rds` attachment to `{attachment_path}/{attachment_id}.rds`
   - Logs result summary: `Status=finished` or `Status=errored`
   - Calls `context$record_attachment(runtime, succeed)` to update index
3. Index status transitions: `init` → `running` (at execution start) → `finished`/`errored` (at result recording)

### 3.4 SQLite-Backed AttachmentIndex [Done]

**Location:** `R/class-attachment-index.R` (new file)

**Implementation:** `AttachmentIndex` R6 class backed by RSQLite, replacing the previous text-based `.results` data.frame in `AgentContext`.

1. **Schema:**
   ```sql
   CREATE TABLE attachment_index (
     attachment_id TEXT PRIMARY KEY,
     stage         TEXT NOT NULL,
     state         TEXT NOT NULL,
     agent_id      TEXT NOT NULL,
     attempt       INTEGER NOT NULL DEFAULT 0,
     status        TEXT NOT NULL DEFAULT 'init',
     succeed       INTEGER,
     created_at    REAL NOT NULL,
     updated_at    REAL NOT NULL
   )
   ```
   Indexes on `(state, stage)` and `(status)` for common queries.

2. **Connection lifecycle:** Open/close per operation via `private$.with_db(callback)` — negligible overhead for ~3-15 writes per scheduler run, and safe against connection leaks.

3. **Backend abstraction:** All SQLite calls are isolated in `$.with_db()` and `$.init_db()`. To swap to duckdb or another backend, only these two private methods need modification.

4. **Public API:**
   - `$register(attachment_id, stage, state, agent_id, attempt)` — INSERT OR REPLACE with status `"init"`
   - `$update_status(attachment_id, status)` — set status to any of `init/running/finished/errored/skipped`
   - `$mark_finished(attachment_id, succeed)` — convenience: sets status + succeed flag
   - `$get(attachment_id)` — single-row lookup
   - `$list(status = NULL)` — all entries, most recent first, optional status filter
   - `$query(state, stage, status)` — filter by state/stage/status
   - `$list_incomplete(timeout_secs = NULL)` — entries with status `init` or `running`, optionally past a timeout
   - `$exists(attachment_id)` — fast existence check
   - `$get_db_path()` — returns the SQLite file path

5. **Status lifecycle:** `init` → `running` → `finished` | `errored` (and `skipped` reserved for future use)

### 3.5 Simplified AgentContext [Done]

**Location:** `R/class-context.R`

**Implementation:**
1. `private$.results` (data.frame) replaced with `private$.index` (AttachmentIndex)
2. `init_resources()` creates `AttachmentIndex$new(db_path)` at `{attachment_path}/index.sqlite`
3. `$index` active binding exposes the index (read-only)
4. `record_attachment(runtime, succeed)` delegates to `index$mark_finished()` — logs `status=finished` or `status=errored`
5. `last_results()`, `get_attachment()`, `get_attachment_by_state()`, `list_attachments()` all query the index
6. New `$list_incomplete(timeout_secs)` — delegates to `index$list_incomplete()`
7. New `$get_runtime_log(attachment_id)` — reads `{attachment_path}/{attachment_id}.log`

### 3.6 Scheduler Integration [Done]

**Location:** `R/class-scheduler.R`

**Implementation:**
1. Creates fresh `AgentRuntime$new(agent, context, policy, attempt = init_retry_count)` per attempt
2. Calls `runtime$run()` which internally handles the full lifecycle (init → running → record result → update index)
3. Retry logic remains in scheduler (`retry_map` tracks cumulative failure count per state)
4. Fixed bare `context` → `self$context` reference bug

### 3.7 Bug Fixes Applied

1. **Column name mismatch:** `init_resources()` used `attachment_id` but `record_attachment()` used `filename` — fixed by SQLite schema where column names are defined once
2. **MCP fallback mismatch:** Empty fallback data.frame used `current_attempt` but actual data used `attempt` — fixed by consistent schema
3. **Test references:** All `$filename` references in tests updated to `$attachment_id`
4. **RSQLite added to DESCRIPTION Imports**, `@importFrom RSQLite SQLite` added to namespace

### Dependencies Added

- `RSQLite (>= 2.3.0)` in DESCRIPTION Imports
- `R/class-attachment-index.R` added to Collate (before `class-context.R`)

---

## Phase 4: Async Scheduler [Done]

**Why:** Enable concurrent execution of independent states. All agent-level async primitives are already in place (`AgentRuntime$run_async`, `chat_async`/`chat_structured_async` dispatch, `StateDeps`, `AttachmentIndex`). This phase adds the scheduler orchestration layer.

**Architecture:** Queue + Waiting Pool (replaces the original rigid priority-group model). States enter a ready-queue when their `depends_on` are satisfied, get dispatched into a bounded waiting pool, and the scheduler drains the pool as promises resolve — feeding newly-unblocked states into the queue without waiting for an entire priority tier to finish.

**Key insight:** R's `coro`/`promises` async is single-threaded cooperative multitasking, not true parallelism. All R code runs on one thread; concurrency only happens during I/O waits (e.g., waiting for LLM API responses). This means:
- No race conditions on R-side data structures (no mutexes needed)
- SQLite open/close-per-operation is safe without WAL mode
- The event loop yields control only at `await` points
- `chat$clone(deep = TRUE)` is still needed because multiple agents sharing the same `Chat` would corrupt conversation state when interleaved at `await` boundaries

### 4.1 Direct Function-Call Lifecycle Event System [Done]

**Location:** `R/class-event-dispatcher.R`, `R/helper-conditions.R`

**Why:** The scheduler needs to communicate lifecycle events (suspend, state completed, stage completed, dispatch) to callers. Rather than requiring users to wrap `scheduler$start()` in `withCallingHandlers()`, the Scheduler owns an internal `EventDispatcher` that manages a listener registry. Users call `scheduler$on("suspend", handler)` to register listeners.

**Design decision:** The original plan specified a condition-based approach (`signalCondition`/`withCallingHandlers`/`withRestarts`/`$wrap()`). This was abandoned because `coro::async` does **not** support `withCallingHandlers()` inside coroutine bodies — conditions raised after an `await` point escape the handler frame. Instead, the dispatcher uses **direct function-call dispatch**: `$emit(event)` iterates a snapshot of the handler list and calls each `handler(event)` directly. This is compatible with `coro::async` and avoids the coroutine limitation entirely.

**Architecture:**
- `EventDispatcher` (internal R6 class) owns a per-type ordered handler list and provides `$on()`, `$off()`, `$emit()`, `$has()`, `$clear()` methods
- The Scheduler composes an `EventDispatcher` instance (has-a, not is-a) and delegates `$on()` / `$off()` as public methods
- `$emit(event)` returns the **first non-NULL** handler return value (first-wins semantics) — used by `suspend` to capture the chosen action string from a listener
- No `$wrap()` method — no conditions, no restarts, no `withCallingHandlers`/`withRestarts`

**Implementation:**

1. **Event constructors** in `R/helper-conditions.R`:
   - `tricobbler_event(type, message, ...)` — creates a plain list with `class = c("tricobbler_<type>", "tricobbler_lifecycle")` and all fields spread flat. These are **not** R conditions; they are plain list event objects passed to `$emit()`.

2. **Lifecycle event types** (emitted by the Scheduler via `dispatch_event()`):

   | Event type | When emitted | Key fields | Semantics |
   |---|---|---|---|
   | `suspend` | Critical state failed, retries exhausted | `state_name, stage, error` | First-wins: handler return value becomes the suspend action |
   | `runtime.resolved` | A runtime promise resolved successfully | `runtime, result` | Informational |
   | `runtime.exhausted` | A state exhausted all retries | `runtime, result` | Informational |
   | `runtime.errored` | A state failed but will retry | `runtime, result` | Informational |
   | `runtime.redirect` | `on_failure` redirect triggered | `runtime, result, target` | Informational |
   | `runtime.dispatch` | A state is about to be dispatched | `state_name, stage, attempt` | Informational |
   | `runtime.skipped` | A state was skipped (depends on skipped/failed) | `state_name, stage` | Informational |
   | `runtime.final` | A `final = TRUE` state completed; draining | `state_name` | Informational |
   | `stage.completed` | All states in a stage finished | `stage` | Informational |
   | `init_resources` | Resources being initialized | — | Informational |
   | `init_stage.begin` / `init_stage.end` | Stage initialization | `stage` | Informational |
   | `enqueue_runtime.begin` / `enqueue_runtime.end` / `enqueue_runtime.changed` | Queue cycle events | — | Informational |

3. **EventDispatcher R6 class** in `R/class-event-dispatcher.R`:
   - Uses `fastmap::fastmap()` for the top-level type registry and reverse-lookup map
   - Per-type handlers stored in a plain list to guarantee deterministic execution order
   - `digest::digest(list(handler, type))` auto-generates deterministic IDs, preventing duplicate registration
   - User-supplied `id` enables explicit replacement (upsert semantics)
   - `$on(type, handler, id, after)` — `after = TRUE` appends (default), `after = FALSE` prepends
   - `$off(id, types)` — removes by ID; reverse-lookup avoids needing the type
   - `$has(type, id)` — checks whether a listener is registered
   - `$clear(type)` — clears one or all types; also called from `finalize()`

4. **Composed into Scheduler:**
   ```r
   # Delegate listener registration
   scheduler$on("runtime.resolved", function(event) {
     cat("Done:", event$state_name, "\n")
   })

   # Auto-skip on suspend (CI/tests):
   scheduler$on("suspend", function(event) {
     "skip"  # return value is captured as the action
   })

   # Custom suspend logic:
   scheduler$on("suspend", function(event) {
     if (event$state_name == "non_essential") "skip" else "abort"
   })

   # Replace a handler by supplying the same id:
   scheduler$on("suspend", my_new_handler, id = "my_suspend")
   ```

**Why direct-call over condition-based:**
- Compatible with `coro::async` (conditions are not preserved across `await` points)
- Simpler mental model — `scheduler$on("event", fn)` with return-value semantics
- Registration persists across multiple `start()` calls
- Multiple listeners for the same event compose naturally
- No need to understand R's condition/restart system

**Dependencies:** `coro`, `promises`, `fastmap`, and `digest` are already in Imports. No new dependencies needed.

### 4.2 `suspend()` with Error Propagation and Action Dispatch [Done]

**Location:** `R/class-scheduler.R`

**Implementation:**
1. **Error propagation:** `execute_runtime()`'s `onFulfilled` callback checks `result$succeed`. When a critical state exhausts retries, the error from the result is passed directly to `self$suspend(error = result$error, ...)`. `private$.last_error` is set inline.
2. **`suspend()` implementation** (`Scheduler$suspend(error, state_name, stage, runtime_summary)`):
   - Sets `self$suspended = TRUE`
   - Captures context in `self$suspend_info` (state_name, stage, error, timestamp) for inspection
   - Logs the error with full traceback via `utils::capture.output(traceback(error))`
   - Emits `"suspend"` event via `self$dispatch_event()` — the return value (first non-NULL handler return) is captured as the action string
   - If no listener returns an action, falls through to interactive `utils::menu()` prompt or `"abort"` in non-interactive mode
   - Executes the chosen action inline via `switch(action, ...)`:
     - `"resume"`: clears suspended state, re-adds state to `runtime_map` with `attempt = 0`, calls `advance()`
     - `"skip"`: clears suspended state, marks state as `"skipped"` in `completed_map`, calls `private$skip_dependents()` to transitively skip downstream dependents, calls `advance()`
     - `"restart_stage"`: clears suspended state, calls `self$start_stage(stage)` to reset all per-stage structures, re-snapshots `.stage_flag`, calls `advance()`
     - `"abort"`: clears suspended state, rejects the stage promise via `private$.stage_reject()`
3. **`suspend_info` field:** Public field set during suspension, cleared on resume/skip/restart/abort. Contains `state_name`, `stage`, `error`, `timestamp` for external inspection.

### 4.3 `max_concurrency` and Async `start()` [Done]

**Location:** `R/class-scheduler.R`

**Implementation:**
1. `max_concurrency` field (integer, default `100L`) — max simultaneous promises in the waiting pool
2. `start()` is itself async — it uses `coro::async` internally to loop over stages, calling `await(self$run_stage(stage))` for each. It returns a promise (via `impl()$then(...)`) that resolves when all stages complete.
3. **No separate `start_async()`** — the original plan to have both `start()` (sync) and `start_async()` was abandoned. `start()` is the single entry point and is inherently async. Callers block by draining the event loop (e.g., `later::run_now()`) or `await` inside a `coro::async` context.
4. Stages are always executed sequentially; `run_stage()` runs all states within a stage concurrently (via queue + pool), returns a promise that resolves when the stage finishes.

### 4.4 Queue + Waiting Pool Dispatch [Done]

**Location:** `R/class-scheduler.R`

**Implementation:** Five per-stage data structures on the Scheduler instance (all reset by `start_stage()`):

| Structure | Type | Purpose |
|---|---|---|
| `runtime_map` | `fastmap` | States not yet dependency-cleared; keyed by state name → `AgentRuntime` |
| `ready_queue` | `fastmap::fastqueue` | Dependency-resolved states sorted by priority (descending); FIFO within same priority |
| `waiting_pool` | `fastmap` | In-flight promises; keyed by state name → `list(runtime, promise)` |
| `completed_map` | `fastmap` | Finished states; keyed by state name → `list(policy, agent, attempt, attachment_id, status)` where status ∈ {"finished", "errored", "skipped"} |
| `retry_map` | `fastmap` | Failed states awaiting retry; keyed by state name → runtime summary |

**Dispatch cycle** (event-driven via `advance()`):

```
init_stage(stage)
  └→ builds runtime_map from manifest
  └→ enqueue_runtime() seeds ready_queue

run_stage(stage) returns a promise
  └→ execute_runtime() kicks off first batch
  └→ each promise resolution → advance()
       └→ retry_runtime()    — re-creates runtimes from retry_map
       └→ enqueue_runtime()  — moves dep-cleared states to ready_queue
       └→ execute_runtime()  — dispatches up to max_concurrency
       └→ check completion   — resolves stage promise if done
```

**Key methods:**
- `init_stage(stage)` — creates `AgentRuntime` for each state in the stage, populates `runtime_map`
- `enqueue_runtime()` — checks `depends_on` resolution against `completed_map`; moves cleared states from `runtime_map` to `ready_queue` sorted by priority. Skips enqueuing when `self$draining` or `self$suspended`.
- `execute_runtime()` — pops from `ready_queue` up to `max_concurrency - waiting_pool$size()` slots; creates promise chain with `onFulfilled`/`onRejected` callbacks; adds to `waiting_pool`. Implements critical-state priority barrier (see 4.5).
- `advance()` — the event-driven driver; called from promise callbacks. Guards against cancellation (`.run_flag != .stage_flag`) and suspension. Detects stage completion when `get_incomplete_size() == 0` or draining with empty waiting pool.
- `run_stage(stage)` — calls `start_stage()`, returns a `promises::promise()` whose resolve/reject are stored in `private$.stage_resolve`/`.stage_reject` and settled by `advance()`.

**No `promise_race` needed:** Each promise's `.then()` callback calls `advance()` directly, which drives the next dispatch cycle. This is simpler and avoids the `promise_race` identification problem.

### 4.5 Retry, `on_failure`, and Critical Priority Barrier [Done]

**Location:** `R/class-scheduler.R`

**Retry logic** (in `execute_runtime()`'s `onFulfilled` callback and `retry_runtime()`):
1. **State fails, retries remain, `on_failure` is `NA`:** State is added to `retry_map`. `retry_runtime()` (called by `advance()`) creates a new `AgentRuntime` with `attempt + 1` and places it in `runtime_map`. `enqueue_runtime()` then moves it to `ready_queue` when dependencies are met.
2. **State fails, retries remain, `on_failure` is set:** The `on_failure` target is created as a new `AgentRuntime` in `runtime_map` (if not already completed or in-flight). The failed state is NOT retried — the redirect replaces retry.
3. **State fails, retries exhausted, `critical = TRUE`:** `self$suspend()` is called with the error and runtime summary. Suspend handler decides the action (see 4.2).
4. **State fails, retries exhausted, `critical = FALSE`:** Marked `"errored"` in `completed_map`. Downstream dependents proceed (they see the errored state as a satisfied dependency).

**Critical-state priority barrier** (in `execute_runtime()`):
- Before dispatching, all queued items are scanned. If any has `critical = TRUE`, only states at priority ≥ the highest critical state's priority are dispatched. Lower-priority states are moved back to `runtime_map` for re-evaluation after the critical state settles.
- This prevents lower-priority work from starting while a critical state is pending, since a critical failure could suspend the entire stage.

**Skipped vs Errored distinction:**
- `"errored"`: State was executed and failed (attachment exists with the error condition as result)
- `"skipped"`: State was never executed (e.g., depends on a skipped or critical-failed state)

**`skip_dependents()` (private):** When a state is skipped (via suspend → skip action), recursively finds all same-stage states whose `depends_on` references the skipped state. Marks them as `"skipped"` in `completed_map`, removes them from `runtime_map` and `ready_queue`, and emits `"runtime.skipped"` events.

### 4.6 Suspend with Direct-Call Dispatch and Interactive Fallback [Done]

**Location:** `R/class-scheduler.R`

**How `suspend()` works with the direct-call event system:**

When `suspend()` is called (from `execute_runtime()`'s promise callback after a critical state exhausts retries):
1. Sets `self$suspended = TRUE`, captures `suspend_info`
2. Logs the error with full traceback
3. Calls `self$dispatch_event(type = "suspend", ...)` which invokes `EventDispatcher$emit()` — iterates handlers and returns the first non-NULL return value
4. If a registered handler returns an action string (e.g., `"skip"`, `"resume"`, `"abort"`, `"restart_stage"`), that value is captured
5. If **no handler returns an action** (`NULL`), falls through to:
   - `interactive()` → `utils::menu()` with 4 choices: Retry state, Skip, Abort, Restart stage
   - Non-interactive → `"abort"`
6. The chosen action is executed inline via `switch(action, ...)` (see 4.2 for action details)

**For tests/CI — register a listener:**
```r
scheduler$on("suspend", function(event) {
  "skip"  # or "abort", "resume", "restart_stage"
})
scheduler$start()  # no wrapping needed, no blocking menu
```

**Key difference from original plan:** No conditions, no restarts, no `$wrap()`. The handler return value is the action, not `invokeRestart()`. This is simpler and compatible with `coro::async`.

### 4.7 Clone Chat for Async Isolation [Done]

**Location:** `R/generic-as_agent.R`

**Implementation:**
1. Inside `agent_fun` (the closure created by `as_agent_from_chat`), the chat is **always** deep-cloned at the top:
   ```r
   chat <- chat$clone(deep = TRUE)
   ```
2. This ensures each execution gets its own `Chat` instance with independent conversation state, system prompt, and tool bindings — whether sync or async.
3. The clone happens unconditionally (not gated on `runtime$status`) to guarantee isolation in all modes and prevent subtle state leakage between retries.

### 4.8 `final` States Drain Pool [Done]

**Location:** `R/class-scheduler.R`

**Implementation:**
1. When a `final = TRUE` state's promise resolves successfully, `self$draining` is set to `TRUE` and the `ready_queue` is cleared
2. `enqueue_runtime()` and `execute_runtime()` check `self$draining` and skip all work when `TRUE`
3. `advance()` detects drain completion when `self$draining == TRUE` and `waiting_pool$size() == 0` — it then clears remaining items in `runtime_map`/`ready_queue`/`retry_map` and resolves the stage promise
4. In-flight promises in the waiting pool are allowed to finish; their results are recorded normally

**Rationale:** Promise cancellation is complex and error-prone. Letting in-flight work complete is simpler and the results may be useful for debugging.

### 4.9 Edge Cases and Potential Bugs

The following issues were identified during review. They are documented here for discussion before fixing.

#### Bug 1: `onRejected` in `execute_runtime()` doesn't clean up waiting pool

**Location:** `R/class-scheduler.R`, `execute_runtime()` promise chain

**Issue:** `AgentRuntime$run_async()` catches agent errors internally (in `onFulfilled`/`onRejected` of the inner promise chain) and always calls `.record_result()`. This means the outer `.then()` in `execute_runtime()` always receives `onFulfilled`. However, if `.record_result()` itself throws (e.g., disk full, SQLite error, `agent@describe()` throws), the outer `onRejected` fires — and it only calls `self$advance()` without removing the state from `waiting_pool` or recording it in `completed_map`. The state effectively vanishes from all maps, causing the stage to never complete (it hangs waiting for an item that will never resolve).

**Fix:** The `onRejected` callback should:
1. Remove the state from `waiting_pool`
2. Mark it as `"errored"` in `completed_map`
3. Then call `self$advance()`

#### Bug 2: `on_failure` target with unsatisfied dependencies can deadlock

**Location:** `R/class-scheduler.R`, `execute_runtime()` on_failure redirect

**Issue:** When `on_failure` fires, the target state is added to `runtime_map` with a fresh `AgentRuntime`. But `enqueue_runtime()` checks the target's `depends_on` — if the target has unmet dependencies (other than the failed state), it will stay blocked in `runtime_map` indefinitely, preventing the stage from completing.

**Note:** The code already has a TODO comment: "need to check if the on_failure depends on the current runtime when manifest is created". This validation should be added to the `Manifest` validator (Phase 5).

**Fix options:**
- Add manifest validation: `on_failure` targets must not have `depends_on` entries that could be unsatisfied when the source state fails (e.g., deps must be a subset of the source state's own deps, or deps on earlier stages only)
- Or: at redirect time, automatically satisfy the failed state's dependency in `completed_map` (it's already there as errored) — but the target may have *other* deps too

#### Bug 3: Non-critical exhausted states satisfy downstream dependencies silently

**Location:** `R/class-scheduler.R`, `execute_runtime()` failure handling

**Issue:** When a non-critical state exhausts retries, it's marked `"errored"` in `completed_map`. This *satisfies* downstream `depends_on` checks in `enqueue_runtime()`. Dependents are enqueued and execute, receiving the errored state's attachment (which contains the error condition as `result`). The dependent agent may not expect or handle an error object as input.

**Assessment:** This is documented design ("Dependents receive the error — the agent function can inspect it"), but it's a footgun. Function agents using `accessibility = "explicit"` will receive an error condition object as their parameter value, which will likely cause a secondary failure. Chat agents receive a textual `mcp_describe()` of the error, which is more graceful.

**Potential improvements:**
- Add `skip_dependents()` call for non-critical exhaustion (opt-in via a policy flag?)
- Or: document clearly that agents downstream of non-critical states should handle error inputs

#### Bug 4: `start()` return value is unclear

**Location:** `R/class-scheduler.R`, `start()`

**Issue:** `start()` uses `coro::async` internally and calls `impl()$then(onFulfilled, onRejected)`. The `$then()` returns a promise, but `start()` doesn't explicitly return it — the promise is returned implicitly (last expression). The `@return` docs say "A promise that resolves when all stages complete," which is correct, but the `$then()` wrapping means any rejection is already handled (printed as `message()`), so the returned promise always resolves. Callers who chain `.then(onRejected = ...)` on the return value will never see failures.

**Assessment:** Minor — the current pattern (print + resolve) is fine for interactive use. For programmatic use, callers should use `scheduler$on("suspend", ...)` to handle failures.

---

## Phase 5: Manifest Validation for Async

**Why:** Prevent invalid configurations that cause undefined behavior in async mode.

### 5.1 Critical and Final State Validation

**Location:** `R/class-manifest.R` (Manifest validator)

**Steps:**
1. Validate that `critical = TRUE` states have unique priority within their stage — prevents concurrent critical states where both could fail simultaneously
2. Validate that `final = TRUE` states have unique priority within their stage — prevents race between two final states
3. Error at manifest construction time with clear message

### 5.2 Dependency Validation [Done]

Already implemented in `Manifest` validator:
- `depends_on` references must exist in manifest
- Same-stage deps must have strictly higher priority (guarantees execution order)
- Circular dependency detection

---

## Phase 6: Error Handling & Timeouts

### 6.1 Add `timeout` Property to StatePolicy

**Location:** `R/class-policystate.R`

**Steps:**
1. Add property: `timeout = S7::new_property(class = S7::class_integer, default = NA_integer_)` (seconds)
2. In `run_stage_async` dispatch, wrap the runtime promise with `promises::promise_race()`:
   ```r
   if (!is.na(policy@timeout)) {
     state_promise <- promises::promise_race(
       runtime$run_async(),
       promises::promise(function(resolve, reject) {
         later::later(
           function() reject(simpleError("Timeout after N seconds")),
           delay = policy@timeout
         )
       })
     )
   } else {
     state_promise <- runtime$run_async()
   }
   ```
3. Timeout rejection is handled as a normal failure — retry logic applies

### 6.2 Partial Failure Policy

**Decision:** Default to optimistic — continue dispatching unless the failed state is `critical`.

- Non-critical failure: state is recorded as `"errored"` in `completed_set` with the error condition as its result value. Dependents receive the error condition and can inspect it.
- Critical failure: `suspend()` is called. The suspend handler (interactive or programmatic) decides: resume, skip, or abort.
- No separate configuration needed — the `critical` flag on `StatePolicy` already provides per-state control.

---

## Resolved Design Questions

### Q1: `on_failure` with Different Priorities → Resolved

The queue model eliminates priority-tier boundaries. `on_failure` targets are simply enqueued into the ready queue and dispatched when their dependencies are met. Priority only affects ordering within the queue, not grouping.

- **Same priority:** Safe — the `on_failure` target is enqueued and dispatched in the next loop iteration. If it's already running (was dispatched concurrently), skip the jump and log a warning.
- **Higher priority (already ran):** The target is re-enqueued and dispatched immediately (nothing blocks it). This is a re-execution, not a re-visit of a priority tier.
- **Lower priority (hasn't run yet):** Natural fit — it runs when dispatched from the queue in priority order.

### Q2: Partial Parallel Failure → Resolved

Default optimistic: continue unless `critical`. See Phase 6.2.

### Q3: Result Ordering → Resolved

Accepted as expected async behavior. `list_attachments()` returns results ordered by `created_at` timestamp (actual completion time). Priority order is recorded in the policy metadata for reference but does not govern result ordering.

---

## Testing Strategy

### Unit Tests
- `tests/testthat/test-event-dispatcher.R` — EventDispatcher: `$on()`, `$off()`, `$emit()`, `$wrap()`, condition propagation, restart invocation
- `tests/testthat/test-lifecycle-conditions.R` — Condition constructors, class hierarchy, `testthat::expect_condition()` integration
- `tests/testthat/test-async-scheduler.R` — Queue + pool dispatch with mock agents
- `tests/testthat/test-async-dependencies.R` — `depends_on` blocks dispatch until dependency completes
- `tests/testthat/test-async-suspend.R` — Suspend via condition system: listener-invoked restarts (resume/skip/abort/restart_stage), interactive fallback, no-listener default
- `tests/testthat/test-async-retry.R` — Retry and `on_failure` re-enqueue in async mode
- `tests/testthat/test-async-final.R` — `final = TRUE` drains pool and stops dispatch
- `tests/testthat/test-async-timeout.R` — Timeout via `promise_race`

### Integration Tests
- Concurrent LLM calls complete faster than sequential (wallclock comparison)
- `AgentRuntime` isolation: concurrent agents don't corrupt each other's Chat state
- SQLite `AttachmentIndex` handles concurrent writes without corruption

### Mock Agent Helpers
```r
# Async mock that simulates I/O delay
mock_async_agent <- function(delay_secs = 0.1, result = "ok", fail = FALSE) {
  as_agent(function(runtime) {
    p <- promises::promise(function(resolve, reject) {
      later::later(function() {
        if (fail) reject(simpleError("mock failure"))
        else resolve(result)
      }, delay = delay_secs)
    })
    return(p)  # run_async will await this
  }, id = paste0("mock_", sample.int(1e6, 1)))
}
```

---

## Migration Path

### Backward Compatibility

```r
# Sync mode (default - unchanged)
scheduler <- Scheduler$new(manifest, agents)
scheduler$start()  # Blocking, sequential execution

# With lifecycle listeners
scheduler <- Scheduler$new(manifest, agents)
scheduler$on("state_completed", function(cond) {
  cat("Done:", cond$data$state_name, "\n")
})
scheduler$on("suspend", function(cond) {
  invokeRestart("tricobbler_skip")  # auto-skip for CI
})
scheduler$start()

# Async mode (opt-in)
scheduler <- Scheduler$new(manifest, agents, max_concurrency = 4L)
promise <- scheduler$start_async()  # Returns promise
# Run the event loop (in interactive R or plumber/shiny):
coro::async_collect(promise)
# Or in a script:
promises::promise_all(promise)
later::run_now()  # Drive the event loop
```

### Deprecation Schedule

1. **v1.x:** `accessibility = "all"` / `"logs"` continue working, emit deprecation warning in async mode
2. **v2.x:** Require `accessibility = "explicit"` with `depends_on` for agents that read previous results in async mode
