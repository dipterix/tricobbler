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

## Phase 3: Per-Runtime Logging & Atomic Attachments

**Why:** Instead of file locking (unnecessary since R promises/coro are single-threaded), we redesign `AgentRuntime` to own its execution lifecycle completely. Each runtime gets its own log file and attachment, enabling crash inspection and clean separation of concerns.

**Key insight:** One `AgentRuntime` = one execution attempt = one attachment. The runtime ID becomes the attachment ID.

### 3.1 Move `attempt` to AgentRuntime Initializer

**Location:** `R/class-runtime.R`

**Current:** Scheduler tracks attempt count and passes to `runtime$run(attempt)`

**New:** Runtime receives `attempt` at construction, generates `execution_id` immediately

**Steps:**
1. Update `AgentRuntime$new()` signature:
   ```r
   initialize = function(agent, context, policy, attempt = 0L) {
     private$.agent <- agent
     private$.context <- context
     private$.policy <- policy
     private$.attempt <- attempt
     private$.start_time <- Sys.time()
     private$.execution_id <- sprintf(
       "%s_%s_%s_%d",
       policy@stage,
       policy@name,
       format(private$.start_time, "%Y%m%d%H%M%OS3"),
       attempt
     )
     # Create per-runtime log file immediately
     private$.log_path <- file.path(
       context$attachment_folder,
       paste0(private$.execution_id, ".log")
     )
     file.create(private$.log_path)
   }
   ```
2. Remove `attempt` parameter from `run()` and `run_async()`
3. Update Scheduler to create new runtime per retry attempt

### 3.2 Add Per-Runtime Log File

**Location:** `R/class-runtime.R`

**Rationale:** Each runtime writes to its own `.log` file using append mode (`a+`). No cross-process conflicts even with mirai/future workers, since each worker has its own runtime instance.

**Steps:**
1. Add private fields:
   ```r
   private = list(
     .log_path = NULL,      # Path to {execution_id}.log
     .start_time = NULL,    # Sys.time() at creation
     .attempt = NULL        # Attempt number
   )
   ```
2. Add `$log(...)` method (replaces current `$logger()`):
   ```r
   log = function(..., level = "INFO") {
     timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%OS3")
     msg <- paste0("[", timestamp, "] [", level, "] ", paste(..., collapse = ""))
     cat(msg, "\n", file = private$.log_path, append = TRUE)
   }
   ```
3. Add `$get_logs()` method:
   ```r
   get_logs = function() {
     if (file.exists(private$.log_path)) {
       readLines(private$.log_path)
     } else {
       character(0)
     }
   }
   ```

### 3.3 Add Timing Capture & Result Finalization

**Location:** `R/class-runtime.R`

**Steps:**
1. Modify `run()` and `run_async()` to capture timing:
   ```r
   run = function() {
     # ... existing execution logic ...
     duration_secs <- as.numeric(
       difftime(Sys.time(), private$.start_time, units = "secs")
     )
     # Include timing in result_list
     list(
       result = result,
       succeed = succeed,
       stage = private$.policy@stage,
       state = private$.policy@name,
       agent_id = private$.policy@agent_id,
       current_attempt = private$.attempt,
       start_time = private$.start_time,
       duration_secs = duration_secs,
       execution_id = private$.execution_id
     )
   }
   ```
2. Add `$finalize(result_list)` method that:
   - Appends final summary line to log: `[COMPLETE] duration=X.Xs status=success/failed`
   - Saves `.rds` attachment file
   - Calls `context$record_result()` to update index
   ```r
   finalize = function(result_list) {
     # Log completion
     status <- if (result_list$succeed) "success" else "failed"
     self$log(
       sprintf("duration=%.3fs status=%s", result_list$duration_secs, status),
       level = "COMPLETE"
     )
     # Save attachment
     rds_path <- file.path(
       private$.context$attachment_folder,
       paste0(private$.execution_id, ".rds")
     )
     saveRDS(result_list, rds_path)
     # Update index (only after .rds exists)
     private$.context$record_result(result_list)
   }
   ```

### 3.4 Simplify AgentContext

**Location:** `R/class-context.R`

**Steps:**
1. Keep `$logger()` for scheduler-level logs only (main thread)
2. Simplify `record_result()` to only update index (runtime handles file creation)
3. Add inspection methods:
   ```r
   list_incomplete = function() {
     # Find .log files without matching .rds (crashed/in-progress runs)
     logs <- list.files(self$attachment_folder, pattern = "\\.log$")
     rds <- list.files(self$attachment_folder, pattern = "\\.rds$")
     log_ids <- sub("\\.log$", "", logs)
     rds_ids <- sub("\\.rds$", "", rds)
     setdiff(log_ids, rds_ids)
   }
   
   get_runtime_log = function(execution_id) {
     log_path <- file.path(self$attachment_folder, paste0(execution_id, ".log"))
     if (file.exists(log_path)) readLines(log_path) else NULL
   }
   ```

### 3.5 Update Scheduler for New Runtime Lifecycle

**Location:** `R/class-scheduler.R`

**Steps:**
1. Create fresh `AgentRuntime` per attempt:
   ```r
   for (attempt in seq_len(max_retries)) {
     runtime <- AgentRuntime$new(agent, context, policy, attempt = attempt - 1L)
     result_list <- runtime$run()
     runtime$finalize(result_list)
     if (result_list$succeed) break
   }
   ```
2. Remove attempt tracking from scheduler (runtime owns it)

### Benefits

1. **Crash inspection:** If agent crashes mid-execution, `.log` file exists without `.rds` — can inspect what happened
2. **No file locking needed:** Each runtime writes only to its own files
3. **Deferred main-thread writes:** Index update happens after promise resolution in main process
4. **Self-contained execution:** Runtime ID = attachment ID, simplifies mental model
5. **Worker process safe:** mirai/future workers can log to their own `.log` files without conflict

---

## Phase 4: Async Scheduler

**Why:** Enable concurrent execution of independent states within a priority level.

### 4.1 Add Dependencies to DESCRIPTION

**Steps:**
1. Add to Imports:
   ```
   coro (>= 1.0.4),
   promises (>= 1.2.0),
   later (>= 1.3.0)
   ```

### 4.2 Implement Priority-Grouped Execution

**Location:** `R/class-scheduler.R`

**Strategy:**
- Group states by priority level
- Execute priority groups sequentially (high to low)
- Within each group, launch all states as promises concurrently
- `await_all()` before moving to next priority level

**Steps:**
1. Add `async` flag to `Scheduler$new()` and `start()`
2. Create `run_stage_async()` method:
   ```r
   run_stage_async = coro::async(function(stage = NULL) {
     states <- extract_manifest_state(self$manifest, stage)
     priority_groups <- split(states, sapply(states, `@`, "priority"))
     
     for (priority in sort(as.integer(names(priority_groups)), decreasing = TRUE)) {
       group <- priority_groups[[as.character(priority)]]
       
       # Launch all same-priority states as promises
       promises <- lapply(group, function(policy) {
         self$run_state_async(policy)
       })
       
       # AWAIT ALL before next priority level
       results <- coro::await_all(promises)
       
       # Check for critical failures
       if (any_critical_failed(results)) {
         self$suspend()
         break
       }
     }
   })
   ```
3. Create `run_state_async()` method that:
   - Creates `ExecutionContext` for isolation
   - Calls agent with promise-returning pattern
   - Handles errors, records results

### 4.3 Async Chat Agent Wrapper

**Location:** `R/class-baseagent.R`

**Steps:**
1. Create `as_agent_from_chat_async()` variant:
   ```r
   agent_fun <- coro::async(function(self, policy, context, exec_ctx) {
     # ... prompt/tool setup ...
     
     # ASYNC call
     result_promise <- if (!is.null(return_type)) {
       chat$chat_structured_async(messages, type = return_type)
     } else {
       chat$chat_async(messages)
     }
     
     result <- coro::await(result_promise)
     return(result)
   })
   ```
2. Clone chat object for parallel execution: `chat <- chat$clone()`

### 4.4 Wrap Sync Agents for Async Mode

**Steps:**
1. Create `async_wrap()` helper that wraps synchronous functions in `coro::async()`
2. Deterministic function agents continue working in async scheduler

---

## Phase 5: Manifest Validation

**Why:** Prevent invalid configurations that cause undefined behavior in async mode.

### 5.1 Critical State Validation

**Location:** `R/class-policy.R` (Manifest validator)

**Steps:**
1. Validate that `critical = TRUE` states have unique priority within their stage
2. Error if critical state shares priority with other states

### 5.2 Final State Validation

**Steps:**
1. Add similar validation for `final = TRUE` states
2. Final states must have unique priority (race condition if parallel)

### 5.3 Dependency Validation

**Steps:**
1. Validate `depends_on` references exist in manifest
2. Validate dependent state has lower priority than dependency (enforces execution order)
3. Detect circular dependencies

---

## Phase 6: Error Handling & Timeouts

### 6.1 Add `timeout` Property to StatePolicy

**Location:** `R/class-policy.R`

**Steps:**
1. Add property: `timeout = S7::new_property(class = S7::class_integer, default = NA_integer_)`
2. Implement in async scheduler using `promises::promise_race()` with timeout promise

### 6.2 Promise Rejection Handling

**Location:** `R/class-scheduler.R`

**Steps:**
1. Wrap parallel executions with error capture
2. Aggregate errors from all parallel states
3. Decide policy: continue on partial failure or abort?

---

## Open Questions

### Q1: `on_failure` with Different Priorities

**Current behavior:** `on_failure` jumps to named recovery state when current state fails.

**Async complexity:** If `on_failure` target has different priority:

**Case A: `on_failure` target has HIGHER priority**
- Example: State at priority 50 fails → jumps to state at priority 100
- The priority 100 level has already completed
- **Question:** Should we re-execute that priority level? Or inject the failure handler into the current level?
- **Consideration:** If we await for the failure handler before proceeding, we break the "all same-priority complete before next level" model
- **Possible solution:** Require `on_failure` targets to have same or lower priority? Or allow higher priority but document the await behavior?

**Case B: `on_failure` target has LOWER priority**
- Example: State at priority 100 fails → jumps to state at priority 50
- The priority 50 level hasn't executed yet
- **Behavior:** Natural fit - failure handler will run when its priority level executes
- **Question:** Should the failure handler run INSTEAD of the failed state's normal successor? Or in addition to?

**Case C: `on_failure` target has SAME priority**
- Example: State A at priority 100 fails → jumps to state B at priority 100
- **Risk:** If both are in same parallel group, state B might already be running!
- **Possible solution:** States with `on_failure` pointing to same-priority states cannot be in parallel group?

**NEEDS CAREFUL DESIGN DECISION - document chosen behavior clearly**

### Q2: Partial Parallel Failure Policy

If 3 of 4 parallel states succeed and 1 fails:
- **Option A:** Continue to next priority level (optimistic)
- **Option B:** Abort entire stage (pessimistic)
- **Option C:** Configurable per-state or per-manifest

**Recommendation:** Default to Option A unless failed state is `critical`

### Q3: Result Ordering

With parallel execution, result completion order differs from priority order.
- Log timestamps will reflect actual completion time
- `list_attachments()` order may not match priority order
- **Recommendation:** Accept this as expected async behavior, document it

---

## Testing Strategy

### Unit Tests
- `tests/testthat/test-async-execution.R` - Parallel state execution
- `tests/testthat/test-explicit-dependencies.R` - `depends_on` resolution
- `tests/testthat/test-async-errors.R` - Error propagation and timeouts

### Integration Tests
- Concurrent LLM calls complete faster than sequential
- `ExecutionContext` isolation prevents cross-talk
- File locking prevents index corruption under parallel writes

### Stress Tests
- 100+ parallel log writes without corruption
- 10+ concurrent Chat agents with separate contexts

---

## Migration Path

### Backward Compatibility

```r
# Sync mode (default - unchanged)
scheduler <- Scheduler$new(manifest, agents)
scheduler$start()  # Blocking, sequential

# Async mode (opt-in)
scheduler <- Scheduler$new(manifest, agents, async = TRUE)
coro::async_run(scheduler$start())  # Non-blocking, parallel
```

### Deprecation Schedule

1. **v1.x:** `accessibility = "all"` / `"logs"` continue working, emit deprecation warning in async mode
2. **v2.x:** Require `accessibility = "explicit"` with `depends_on` for agents that read previous results in async mode
