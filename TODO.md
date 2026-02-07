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

## Phase 4: Async Scheduler

**Why:** Enable concurrent execution of independent states. All agent-level async primitives are already in place (`AgentRuntime$run_async`, `chat_async`/`chat_structured_async` dispatch, `StateDeps`, `AttachmentIndex`). This phase adds the scheduler orchestration layer.

**Architecture:** Queue + Waiting Pool (replaces the original rigid priority-group model). States enter a ready-queue when their `depends_on` are satisfied, get dispatched into a bounded waiting pool, and the scheduler drains the pool as promises resolve — feeding newly-unblocked states into the queue without waiting for an entire priority tier to finish.

**Key insight:** R's `coro`/`promises` async is single-threaded cooperative multitasking, not true parallelism. All R code runs on one thread; concurrency only happens during I/O waits (e.g., waiting for LLM API responses). This means:
- No race conditions on R-side data structures (no mutexes needed)
- SQLite open/close-per-operation is safe without WAL mode
- The event loop yields control only at `await` points
- `chat$clone(deep = TRUE)` is still needed because multiple agents sharing the same `Chat` would corrupt conversation state when interleaved at `await` boundaries

### 4.1 Implement Condition-Based Lifecycle Event System

**Location:** New `R/class-event-dispatcher.R`, `R/helper-conditions.R`

**Why:** The scheduler needs to communicate lifecycle events (suspend, state completed, stage completed, dispatch) to callers. Rather than requiring users to wrap `scheduler$start()` in `withCallingHandlers()`, the Scheduler owns an internal `EventDispatcher` that manages a listener registry. Users call `scheduler$on("suspend", handler)` to register listeners, and `start()` / `start_async()` internally wraps execution in `withCallingHandlers()` + `withRestarts()` built from the registry.

**Architecture:**
- R's condition system (`signalCondition` / `withCallingHandlers`) provides the dispatch mechanism — zero overhead when no listeners are registered, stack-based, synchronous inline execution
- `EventDispatcher` (internal R6 class) wraps the registry and provides `$on()`, `$off()`, `$emit()`, and `$wrap()` methods
- The Scheduler composes an `EventDispatcher` instance (has-a, not is-a) and delegates `$on()` / `$off()` as public methods
- `$wrap(expr)` returns `expr` wrapped in `withCallingHandlers()` + `withRestarts()` built from the current registry — the Scheduler calls this around `run_stage()` / `run_stage_async()` inside `start()` / `start_async()`

**Steps:**

1. **Create condition constructors** in `R/helper-conditions.R`:
   ```r
   # Base condition constructor — fields are spread flat on the condition object
   # so handlers use cond$state_name, not cond$data$state_name
   tricobbler_condition <- function(type, message, ..., call = NULL) {
     structure(
       class = c(
         paste0("tricobbler_", type),
         "tricobbler_lifecycle",
         "condition"
       ),
       list(message = message, ..., call = call)
     )
   }
   ```

2. **Define lifecycle condition classes:**

   | Condition class | When signaled | Fields (flat on `cond$`) | Restarts available |
   |---|---|---|---|
   | `tricobbler_suspend` | Critical state failed, retries exhausted | `scheduler, state_name, stage, error` | `tricobbler_resume`, `tricobbler_skip`, `tricobbler_abort`, `tricobbler_restart_stage` |
   | `tricobbler_state_completed` | A state finished (success or failure) | `scheduler, state_name, stage, succeed, attachment_id` | (informational, no restarts) |
   | `tricobbler_stage_completed` | All states in a stage finished | `scheduler, stage` | (informational) |
   | `tricobbler_dispatch` | A state is about to be dispatched | `scheduler, state_name, stage, attempt` | (informational) |

3. **Implement `EventDispatcher` R6 class** in `R/class-event-dispatcher.R`:

   Uses `fastmap::fastmap()` for both the top-level type registry and per-type handler maps — O(1) lookup, no partial-matching footguns. `digest::digest()` auto-generates deterministic IDs from handler functions, preventing duplicate registration of the same closure. User-supplied `id` enables explicit replacement (upsert semantics).

   ```r
   EventDispatcher <- R6::R6Class(
     "EventDispatcher",
     private = list(
       # Registry: fastmap of fastmaps
       # Top-level keys = event types (e.g. "suspend", "state_completed")
       # Each value = fastmap of {id -> handler_function}
       .listeners = NULL,
       # Reverse lookup: id -> type (for off() without requiring type)
       .id_to_type = NULL,
       # Ensure a per-type fastmap exists
       .ensure_type = function(type) {
         if (!private$.listeners$has(type)) {
           private$.listeners$set(type, fastmap::fastmap())
         }
       }
     ),
     public = list(
       initialize = function() {
         private$.listeners <- fastmap::fastmap()
         private$.id_to_type <- fastmap::fastmap()
       },
       
       # Register a listener for condition type
       # id: user-supplied or auto-generated via digest::digest(handler)
       # If id already exists under this type, the handler is replaced (upsert)
       # Returns the id (invisibly) for later removal via off()
       on = function(type, handler, id = NULL) {
         stopifnot(is.function(handler))
         if (is.null(id) || is.na(id)) {
           id <- digest::digest(handler)
         }
         private$.ensure_type(type)
         private$.listeners$get(type)$set(id, handler)
         private$.id_to_type$set(id, type)
         invisible(id)
       },
       
       # Remove a listener by ID
       # Looks up the type from reverse map — no need to pass type
       off = function(id) {
         type <- private$.id_to_type$get(id)
         if (!is.null(type) && private$.listeners$has(type)) {
           private$.listeners$get(type)$remove(id)
           private$.id_to_type$remove(id)
         }
         invisible(self)
       },
       
       # Signal a condition — registered listeners are invoked inline
       # For conditions with restarts (e.g. suspend), this is called
       # from within the withRestarts() block set up by wrap()
       emit = function(type, message = type, ...) {
         signalCondition(tricobbler_condition(type, message, ...))
       },
       
       # Wrap an expression with withCallingHandlers built from registry
       # Also installs withRestarts for suspend-type conditions
       wrap = function(expr, restarts = list()) {
         # Build handler list from registry
         handlers <- list()
         for (type in private$.listeners$keys()) {
           type_map <- private$.listeners$get(type)
           if (type_map$size() == 0L) next
           condition_class <- paste0("tricobbler_", type)
           # Combine multiple listeners for same type into one handler
           handlers[[condition_class]] <- local({
             listener_funs <- type_map$as_list()
             function(cond) {
               for (fn in listener_funs) {
                 fn(cond)
               }
             }
           })
         }
         
         # Install restarts, then handlers, then evaluate
         wrapped <- if (length(restarts) > 0L) {
           do.call(withRestarts, c(list(expr), restarts))
         } else {
           expr
         }
         
         if (length(handlers) > 0L) {
           do.call(withCallingHandlers, c(list(wrapped), handlers))
         } else {
           wrapped
         }
       }
     )
   )
   ```

4. **Compose into Scheduler:**
   ```r
   Scheduler <- R6::R6Class(
     ...
     private = list(
       .dispatcher = NULL,  # EventDispatcher instance
       ...
     ),
     public = list(
       initialize = function(manifest, agents, max_concurrency = Inf) {
         ...
         private$.dispatcher <- EventDispatcher$new()
       },
       
       # Delegate listener registration
       on = function(type, handler, id = NULL) {
         private$.dispatcher$on(type, handler, id = id)
       },
       off = function(id) {
         private$.dispatcher$off(id)
       },
       
       start = function() {
         # ... existing guards, init_resources ...
         
         # Define restarts for suspend conditions
         suspend_restarts <- list(
           tricobbler_resume = function() "resume",
           tricobbler_skip = function() "skip",
           tricobbler_abort = function() "abort",
           tricobbler_restart_stage = function() "restart_stage"
         )
         
         # Wrap the stage loop with dispatcher
         private$.dispatcher$wrap(
           self$run_stage(),
           restarts = suspend_restarts
         )
       }
     )
   )
   ```

5. **User-facing API:**
   ```r
   scheduler <- Scheduler$new(manifest, agents)
   
   # Register listeners before starting
   scheduler$on("state_completed", function(cond) {
     cat("Done:", cond$state_name, "\n")
   })
   
   # Auto-skip on suspend (CI/tests):
   scheduler$on("suspend", function(cond) {
     invokeRestart("tricobbler_skip")
   })
   
   # Custom suspend logic:
   scheduler$on("suspend", function(cond) {
     if (cond$state_name == "non_essential") {
       invokeRestart("tricobbler_skip")
     } else {
       invokeRestart("tricobbler_abort")
     }
   })
   
   # Replace a handler by supplying the same id:
   scheduler$on("suspend", my_new_handler, id = "my_suspend")
   
   # Just start — no wrapping needed
   scheduler$start()
   ```

6. **Default suspend behavior** (when no `tricobbler_suspend` listener is registered):
   - The condition propagates past `withCallingHandlers` (no handler installed for it)
   - `suspend()` falls through to a default: `interactive()` → `utils::menu()` prompt; non-interactive → `"abort"`
   - See 4.6 for the fallback implementation

**Why this over raw `withCallingHandlers` wrapping:**
- Users don't need to understand R's condition system — `scheduler$on("suspend", fn)` is familiar
- Registration persists across multiple `start()` calls (e.g., after resume)
- Multiple listeners for the same event compose naturally
- The Scheduler controls the `withRestarts()` installation — users just call `invokeRestart()` from their handler
- Advanced users can still use external `withCallingHandlers()` wrapping if they prefer — conditions propagate up the stack regardless
- Testable: `testthat::expect_condition(scheduler$start(), class = "tricobbler_suspend")` still works

**Dependencies:** `coro`, `promises`, `fastmap`, and `digest` are already in Imports. No new dependencies needed.

### 4.2 Fix `suspend()` and Populate `.last_error`

**Location:** `R/class-scheduler.R`

**Why:** `suspend()` currently calls `.NotYetImplemented()` which crashes the scheduler. Also, `private$.last_error` is never populated — `AgentRuntime` handles errors internally and the scheduler never captures them.

**Steps:**
1. **Propagate errors from runtime to scheduler:** After `runtime$run()` or awaiting `runtime$run_async()`, check if the policy is critical and if `runtime$status == "errored"`, set `private$.last_error <- runtime$last_error`. Only errors causing suspension need to be propagated.
2. **Replace `.NotYetImplemented()` in `suspend()`** with real implementation:
   - Set `self$suspended = TRUE` — the dispatch loop (both sync and async) checks this flag; when `TRUE`, no new states are dispatched, in-flight promises drain naturally
   - Log the error with full traceback
   - Emit `tricobbler_suspend` condition via `private$.dispatcher$emit()` (see 4.1)
   - If a registered listener invokes a restart (`tricobbler_resume`, `tricobbler_skip`, `tricobbler_abort`, `tricobbler_restart_stage`), the corresponding action is taken
   - If no listener handles it, fall through to interactive menu or abort (see 4.6)

### 4.3 Add `max_concurrency` and `start_async()` to Scheduler

**Location:** `R/class-scheduler.R`

**Steps:**
1. Add `max_concurrency` field (integer, default `Inf`) — max simultaneous promises in the waiting pool
2. Add `start_async()` public method:
   ```r
   start_async = function() {
     # Same guards as start(): validate agents, init_resources, reset flags
     # Wrap run_stage_async() with dispatcher (same as start wraps run_stage)
     # Returns a promise that resolves when all stages complete
   }
   ```
3. Keep `start()` unchanged — fully synchronous, uses `run_stage()` as before

**Important:** Stages are always executed sequentially, even in async mode. `run_stage_async()` runs all states within a single stage concurrently (via the queue + pool), then `await`s until all states complete before advancing to the next stage. `start_async()` simply wraps the sequential stage loop in `coro::async()` so it returns a promise instead of blocking. Both `start()` and `start_async()` use `private$.dispatcher$wrap()` to install condition handlers and restarts from registered listeners.

### 4.4 Implement Queue + Waiting Pool Dispatch

**Location:** `R/class-scheduler.R`

**Strategy:** `run_stage_async(stage)` manages a dispatch loop with three data structures:

```
ready_queue:    [state_C(pri=100), state_A(pri=80), state_B(pri=80)]
                 ↓ dispatch up to max_concurrency (2 in this toy example)
waiting_pool:   {state_C: <promise>, state_A: <promise>}
                 ↓ promise_race resolves one
completed_set:  {state_C: TRUE, state_D: <error_condition>}
                 ↓ re-evaluate blocked states
ready_queue:    [state_D: (pri=120), state_B(pri=80), state_E(pri=50)]  ← `state_E` depends_on state_C, now satisfied; `state_D` enters retry, but since it has higher priority, it is queued ahead of `state_E` and `state_B`. `state_B` was not executed due to max_concurrency reached: it might still not be executed if there is only one slot available (`state_D` will run first)
```

**Steps:**
1. Create `run_stage_async = coro::async(function(stage = NULL) { ... })`:
   - `all_states <- extract_manifest_state(manifest, stage)` — sorted by priority descending
   - `policy_map` (fastmap) — keyed by state name → `{agent, policy}`
   - `ready_queue` — ordered list; initially seeded with states whose `depends_on` are empty or only reference earlier stages (already in `completed_set` from previous stage runs)
   - `waiting_pool` — named list of active promises, keyed by state name
   - `completed_set` (fastmap) — state name → outcome: `TRUE` (success), error condition (failure), or `"skipped"` (not run)
   - `retry_map` — list, state name → integer attempt count (same semantics as sync path)
   - `blocked_states` — states waiting for intra-stage dependencies

2. **Seed the ready queue:** For each state in `all_states`, check if all `depends_on` entries are satisfied:
   - Cross-stage deps: query `context$get_attachment_by_state(state, stage)` — if found, satisfied
   - Same-stage deps: check `completed_set` — if present, satisfied (even if errored)
   - If all satisfied → push to `ready_queue`; otherwise → push to `blocked_states`

3. **Dispatch loop** (`while` ready_queue non-empty OR waiting_pool non-empty):
   ```
   a. Check self$suspended → if TRUE, skip dispatching, just drain pool
   b. While ready_queue non-empty AND length(waiting_pool) < max_concurrency:
      - Pop highest-priority state from ready_queue
      - Create AgentRuntime(agent, context, policy, attempt = retry_map[[name]])
      - Launch runtime$run_async() → promise
      - Attach then()/catch() handlers to the promise that push to a
        resolution channel (a simple list + flag checked after await)
      - Add to waiting_pool
   c. await(promise_race(waiting_pool)) — yields until any promise resolves
   d. For each resolved promise:
      - Remove from waiting_pool
      - Record outcome in completed_set
      - Handle failure: retry logic (see 4.5)
      - Handle success: if policy@final, set a drain flag (finish pool, skip further dispatch)
      - Re-evaluate blocked_states: move newly-ready states to ready_queue
   e. Check critical failure → suspend if needed
   ```

4. After loop exits, advance to next stage: `self$run_stage_async()` (recursive, same as sync)

**`promise_race` mechanics:** `coro::async` returns a promise, so `promises::promise_race(pool_as_list)` works directly. When one resolves, identify which state it was by matching the promise or using the `then()`/`catch()` callback pattern to record the state name. Prefer the callback approach — attach a `.then` and `.catch` to each promise at dispatch time that writes `{state_name, result, succeed}` into a `private$.resolved_queue`, then `await` a sentinel promise that resolves when `resolved_queue` is non-empty.

### 4.5 Retry and `on_failure` in Queue Model

**Location:** `R/class-scheduler.R` (within `run_stage_async`)

**Rules (same as sync path, adapted for queue):**
1. **State fails, retries remain, `on_failure` is `NA`:** Re-enqueue the same state into `ready_queue` with incremented `retry_map`. It will be re-dispatched next iteration.
2. **State fails, retries remain, `on_failure` is set:** Enqueue the `on_failure` target into `ready_queue` (if not already in pool or completed). The failed state's retry count is incremented. The `on_failure` target's own retry count applies independently.
3. **State fails, retries exhausted, `critical = TRUE`:** Call `self$suspend()`. The suspend handler decides whether to resume, skip, or abort (see 4.6).
4. **State fails, retries exhausted, `critical = FALSE`:** Add to `completed_set` with the error condition as value. Dependents receive the error — the agent function can inspect the dependency value and decide how to handle it. The state is NOT marked `"skipped"`; it is `"errored"`.
5. **`on_failure` target already running or completed:** Skip the jump — the target will produce its own result. Log a warning.
6. **`on_failure` across priorities:** Handled naturally. The `on_failure` target is just enqueued into `ready_queue` sorted by its own priority. No special cases needed since the queue model doesn't enforce strict priority tiers.

**Skipped vs Errored distinction:**
- `"errored"`: State was executed and failed (attachment exists with the error condition as result)
- `"skipped"`: State was never executed (e.g., scheduler aborted before reaching it, or dependent of a suspend-abort)

### 4.6 Suspend Implementation and Interactive Fallback

**Location:** `R/class-scheduler.R`

**How `suspend()` works with the event system (from 4.1):**

`start()` and `start_async()` call `private$.dispatcher$wrap()` which installs:
- `withCallingHandlers(...)` — routes `tricobbler_suspend` (and other lifecycle conditions) to registered listeners
- `withRestarts(...)` — installs `tricobbler_resume`, `tricobbler_skip`, `tricobbler_abort`, `tricobbler_restart_stage` restarts that listeners can invoke

When `suspend()` is called:
1. Sets `self$suspended = TRUE`
2. Logs the error
3. Emits `tricobbler_suspend` condition via `private$.dispatcher$emit("suspend", ...)`
4. If a registered listener calls `invokeRestart("tricobbler_skip")` (etc.), the restart returns the action string (e.g., `"skip"`) and `start()` processes it via `private$.process_suspend_action()`
5. If **no listener handles the condition** (no `invokeRestart` called), the signal falls through and `suspend()` enters the default fallback

**`suspend()` implementation:**
```r
suspend = function() {
  self$suspended <- TRUE
  self$context$logger(
    "Scheduler suspended at ", self$current_stage,
    " -> ", self$current_state, caller = self
  )
  
  # Emit condition — if a listener invokes a restart, withRestarts()
  # (installed by dispatcher$wrap in start()) catches it and returns
  # the action string. If nobody handles it, emit() returns normally
  # and we fall through to the default.
  private$.dispatcher$emit("suspend",
    message = paste0("Scheduler suspended: ", self$current_state),
    data = list(
      scheduler = self,
      state_name = self$current_state,
      stage = self$current_stage,
      error = private$.last_error
    )
  )
  
  # Default fallback: no listener handled the suspend
  action <- if (interactive()) {
    private$.interactive_suspend_menu()
  } else {
    "abort"
  }
  
  private$.process_suspend_action(action)
}
```

**Note on restart flow:** When a listener calls `invokeRestart("tricobbler_resume")`, execution jumps out of `emit()` → out of `suspend()` → to the `withRestarts()` block installed by `dispatcher$wrap()` inside `start()`. The restart function returns the action string (e.g., `"resume"`), and `start()` calls `private$.process_suspend_action(action)` to handle it. This means `suspend()` itself does NOT process the action when a restart is invoked — execution never returns to the fallback code below `emit()`. The fallback only runs when no listener calls a restart.

**`private$.process_suspend_action(action)`:**
```r
.process_suspend_action = function(action) {
  switch(action,
    "resume" = {
      self$suspended <- FALSE
      # Re-enqueue the failed state with retry_map reset
    },
    "skip" = {
      self$suspended <- FALSE
      # Add to completed_set as errored, let dispatch continue
    },
    "restart_stage" = {
      self$suspended <- FALSE
      # Clear stage attachment indices, re-run entire stage
    },
    "abort" = {
      self$stop()
    }
  )
}
```

**Interactive fallback** (`private$.interactive_suspend_menu()`):
```r
.interactive_suspend_menu = function() {
  message("Error: ", conditionMessage(private$.last_error))
  choice <- utils::menu(
    title = "Scheduler suspended. What would you like to do?",
    choices = c(
      "Resume        - Retry the failed state with fresh attempts",
      "Skip          - Mark as errored and continue",
      "Abort         - Stop the scheduler entirely",
      "Restart stage - Clear stage and re-run from scratch",
      "Inspect       - Enter browser(), then re-prompt"
    )
  )
  action <- c("resume", "skip", "abort", "restart_stage", "inspect")[choice]
  if (identical(action, "inspect")) {
    browser()
    return(private$.interactive_suspend_menu())  # re-prompt
  }
  action
}
```

**For tests/CI — register a listener:**
```r
scheduler$on("suspend", function(cond) {
  invokeRestart("tricobbler_skip")  # or "tricobbler_abort"
})
scheduler$start()  # no wrapping needed, no blocking menu
```

**Sync and async parity:** Same `suspend()` logic for both paths. `signalCondition` is synchronous and runs handlers inline, so it works identically whether called from `run_stage()` (sync) or from within `run_stage_async()` (async — R code between `await` points is synchronous).

### 4.7 Clone Chat for Async Isolation

**Location:** `R/generic-as_agent.R`

**Steps:**
1. Inside `agent_fun` (the closure created by `as_agent_from_chat`), add at the top:
   ```r
   if (identical(runtime$status, "running (async)")) {
     chat <- chat$clone(deep = TRUE)
   }
   ```
2. This ensures each concurrent execution gets its own `Chat` instance with independent conversation state, system prompt, and tool bindings
3. Sync mode (`runtime$status == "running"`) continues using the original object — no cloning overhead

### 4.8 `final` States in Async Mode

**Decision:** When a `final = TRUE` state succeeds while other same-priority states are in-flight:
- Let in-flight promises in the waiting pool **finish** (their results are recorded normally)
- **Stop dispatching** new states from the ready queue
- After pool drains, skip remaining stages

**Rationale:** Promise cancellation is complex and error-prone. Letting in-flight work complete is simpler and the results may be useful for debugging. The cost is minimal since these agents are already running.

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
