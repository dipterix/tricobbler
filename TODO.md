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

## Phase 1: Foundation - Explicit Dependencies

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

## Phase 2: Globals Isolation

**Why:** Current globals (`active_context`, `active_agent`, `active_policy`) in `R/aaa.R` are shared across all agents. With parallel execution, concurrent agents overwrite each other's globals, causing MCP tools to receive wrong context.

### 2.1 Create ExecutionContext Wrapper

**Location:** New file `R/class-execution-context.R` or add to `R/class-context.R`

**Steps:**
1. Create lightweight R6 class to hold per-execution state:
   ```r
   ExecutionContext <- R6::R6Class(
     public = list(
       context = NULL,      # AgentContext (shared)
       agent = NULL,        # Current Agent
       policy = NULL,       # Current StatePolicy
       execution_id = NULL  # Unique ID for this execution
     )
   )
   ```
2. This object is passed explicitly to agent functions, not stored in globals

### 2.2 Update MCP Tools for Explicit Context

**Location:** `R/mcp-tooldef-config.R`, `R/mcp-tooldef-package.R`, `R/mcp-tools.R`

**Steps:**
1. Add optional `.exec_ctx` parameter to all MCP tools:
   ```r
   mcp_tool_context_logs_tail <- function(max_lines, skip_lines, .exec_ctx = NULL) {
     ctx <- if (!is.null(.exec_ctx)) .exec_ctx$context else get_active_context()
     # ...
   }
   ```
2. Tools continue working with globals for backward compatibility (sync mode)
3. In async mode, scheduler passes `ExecutionContext` explicitly

### 2.3 Update Agent Wrappers

**Location:** `R/class-baseagent.R`

**Steps:**
1. Modify `as_agent()` wrappers to accept `exec_ctx` parameter
2. Pass `exec_ctx` to MCP tools when provided
3. For Chat agents: use `exec_ctx$policy` instead of `get_globals("active_policy")`

---

## Phase 3: Thread-Safety

**Why:** Parallel agents write to shared Context storage. Without synchronization, file writes can corrupt index or interleave log lines.

### 3.1 Add File Locking to Context

**Location:** `R/class-context.R`

**Dependency:** Add `filelock` to DESCRIPTION Imports

**Steps:**
1. In `record_result()` (around line 401):
   - Acquire lock on index file before read-modify-write
   - Use `filelock::lock()` with timeout
   - Release lock in `on.exit()`
2. In `logger()` (around line 282):
   - Lock log file during append
   - Consider using atomic single-line writes as alternative

### 3.2 Context Cache Thread-Safety

**Location:** `R/class-context.R`

**Steps:**
1. Option A (Simple): Disable cache in async mode
   ```r
   if (async_mode) private$.cache <- NULL
   ```
2. Option B (Later): Use database backend for shared cache

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
