# Tests for the synchronous Scheduler class
# Covers: onRejected cleanup, on_failure deadlock, start/abort lifecycle,
#         stop/restart, and critical suspend actions.

# -- onRejected cleanup -------------------------------------------------------
# When record_attachment() throws, execute_runtime() catches the unexpected
# error, marks state as "errored", and lets the stage finish.

test_that("Sync: onRejected cleanup when record_attachment fails", {
  agent_b_ran <- FALSE

  agent_a <- Agent(
    id = "agent_a",
    .data = function(runtime) "A result",
    description = "Agent whose record will be sabotaged"
  )
  agent_b <- Agent(
    id = "agent_b",
    .data = function(runtime) {
      agent_b_ran <<- TRUE
      "B completed"
    },
    description = "Downstream agent"
  )

  manifest <- Manifest(
    master = MasterPolicy(
      name = "sync_onrejected_test",
      version = "1.0.0",
      stages = "execution"
    ),
    states = list(
      StatePolicy(
        name = "state_a", stage = "execution", agent_id = "agent_a",
        priority = 200, max_retry = 0L, critical = FALSE
      ),
      StatePolicy(
        name = "state_b", stage = "execution", agent_id = "agent_b",
        priority = 100, critical = FALSE
      )
    )
  )

  scheduler <- Scheduler$new(manifest = manifest,
                             agents = list(agent_a, agent_b))

  tracker <- sabotage_record_attachment(scheduler, "state_a")

  errored_count <- 0L
  scheduler$on("runtime.errored", function(event) {
    errored_count <<- errored_count + 1L
  })

  scheduler$start()

  # Stage completed (not hung)
  expect_equal(scheduler$current_stage, "ready")

  # state_a in completed_map as "errored"
  expect_true(scheduler$completed_map$has("state_a"))
  expect_equal(scheduler$completed_map$get("state_a")$status, "errored")

  # Agent B still ran (state_a is non-critical)
  expect_true(agent_b_ran)

  # state_a cleaned from waiting_pool

  expect_false(scheduler$waiting_pool$has("state_a"))

  # runtime.errored event fired
  expect_gt(errored_count, 0L)

  # Sabotage actually triggered
  expect_gt(tracker$count, 0L)
})


# -- on_failure deadlock prevention --------------------------------------------
# State A fails with on_failure = "state_b". State B depends on state_a.
# The failed state must be marked "errored" BEFORE creating the redirect target.

test_that("Sync: on_failure redirect does not deadlock", {
  agent_b_ran <- FALSE

  agent_a <- Agent(
    id = "agent_a",
    .data = function(runtime) stop("Agent A deliberate failure"),
    description = "Always-failing agent"
  )
  agent_b <- Agent(
    id = "agent_b",
    .data = function(runtime) {
      agent_b_ran <<- TRUE
      "B completed after A failed"
    },
    description = "Redirect target agent"
  )

  manifest <- Manifest(
    master = MasterPolicy(
      name = "sync_onfailure_test",
      version = "1.0.0",
      stages = "execution"
    ),
    states = list(
      StatePolicy(
        name = "state_a", stage = "execution", agent_id = "agent_a",
        priority = 200, max_retry = 1L,
        on_failure = "state_b", critical = FALSE
      ),
      StatePolicy(
        name = "state_b", stage = "execution", agent_id = "agent_b",
        priority = 100, max_retry = 0L, critical = FALSE,
        depends_on = StateDeps(res = "state_a")
      )
    )
  )

  scheduler <- Scheduler$new(manifest = manifest,
                             agents = list(agent_a, agent_b))

  redirect_fired <- FALSE
  scheduler$on("runtime.redirect", function(event) {
    redirect_fired <<- TRUE
  })

  scheduler$start()

  expect_equal(scheduler$current_stage, "ready")
  expect_true(scheduler$completed_map$has("state_a"))
  expect_equal(scheduler$completed_map$get("state_a")$status, "errored")
  expect_true(agent_b_ran)
  expect_true(scheduler$completed_map$has("state_b"))
  expect_equal(scheduler$completed_map$get("state_b")$status, "finished")
  expect_true(redirect_fired)
  expect_equal(scheduler$waiting_pool$size(), 0L)
  expect_equal(scheduler$runtime_map$size(), 0L)
})


# -- start() lifecycle events --------------------------------------------------
# Part A: Happy path emits scheduler.completed.
# Part B: Critical failure + abort emits scheduler.aborted.

test_that("Sync: start() emits scheduler.completed on success", {
  completed <- NULL
  aborted <- NULL

  agent_ok <- Agent(id = "agent_ok",
                    .data = function(runtime) "OK")
  manifest <- Manifest(
    master = MasterPolicy(name = "sync_start_ok", version = "1.0.0",
                          stages = "run"),
    states = list(
      StatePolicy(name = "state_ok", stage = "run",
                  agent_id = "agent_ok", priority = 100)
    )
  )

  sched <- Scheduler$new(manifest = manifest, agents = list(agent_ok))
  sched$on("scheduler.completed", function(e) completed <<- e)
  sched$on("scheduler.aborted", function(e) aborted <<- e)
  sched$start()

  expect_false(is.null(completed))
  expect_true(is.null(aborted))
  expect_equal(sched$current_stage, "ready")
})

test_that("Sync: start() emits scheduler.aborted on critical abort", {
  completed <- NULL
  aborted <- NULL

  agent_fail <- Agent(id = "agent_fail",
                      .data = function(runtime) stop("Critical failure!"))
  manifest <- Manifest(
    master = MasterPolicy(name = "sync_start_abort", version = "1.0.0",
                          stages = "run"),
    states = list(
      StatePolicy(name = "state_critical", stage = "run",
                  agent_id = "agent_fail", priority = 100,
                  max_retry = 0L, critical = TRUE)
    )
  )

  sched <- Scheduler$new(manifest = manifest, agents = list(agent_fail))
  sched$on("suspend", function(event) "abort", id = "test_abort")
  sched$on("scheduler.completed", function(e) completed <<- e)
  sched$on("scheduler.aborted", function(e) aborted <<- e)

  expect_error(sched$start(), "[Aa]bort")

  expect_false(is.null(aborted))
  expect_true(is.null(completed))
})


# -- stop() cleanup + restart --------------------------------------------------

test_that("Sync: stop() clears all structures", {
  agent_fast <- Agent(id = "agent_fast",
                      .data = function(runtime) "fast done")
  agent_dep <- Agent(id = "agent_dep",
                     .data = function(runtime) "dep done")

  manifest <- Manifest(
    master = MasterPolicy(name = "sync_stop_test", version = "1.0.0",
                          stages = c("stage1", "stage2")),
    states = list(
      StatePolicy(name = "state_fast", stage = "stage1",
                  agent_id = "agent_fast", priority = 200),
      StatePolicy(name = "state_dep", stage = "stage2",
                  agent_id = "agent_dep", priority = 100)
    )
  )

  sched <- Scheduler$new(manifest = manifest,
                         agents = list(agent_fast, agent_dep))
  sched$start()
  sched$stop()

  expect_equal(sched$current_stage, "ready")
  expect_equal(sched$runtime_map$size(), 0L)
  expect_equal(sched$ready_queue$size(), 0L)
  expect_equal(sched$waiting_pool$size(), 0L)
  expect_equal(sched$completed_map$size(), 0L)
  expect_equal(sched$retry_map$size(), 0L)
  expect_false(isTRUE(sched$suspended))
  expect_true(is.null(sched$suspend_info))
  expect_false(isTRUE(sched$draining))
})

test_that("Sync: stop() then immediate restart succeeds", {
  run_count <- 0L

  agent_quick <- Agent(
    id = "agent_quick",
    .data = function(runtime) {
      run_count <<- run_count + 1L
      "quick done"
    }
  )

  manifest <- Manifest(
    master = MasterPolicy(name = "sync_restart_test", version = "1.0.0",
                          stages = "run"),
    states = list(
      StatePolicy(name = "state_quick", stage = "run",
                  agent_id = "agent_quick", priority = 100)
    )
  )

  sched <- Scheduler$new(manifest = manifest, agents = list(agent_quick))

  # Run 1
  sched$start()
  sched$stop()

  # Run 2
  completed <- NULL
  sched$on("scheduler.completed", function(e) completed <<- e)
  sched$start()

  expect_false(is.null(completed))
  expect_equal(sched$current_stage, "ready")
  expect_equal(run_count, 2L)
})


# -- Critical suspend: resume, skip, restart_stage, abort ----------------------

test_that("Sync: suspend 'resume' re-runs state with attempt reset", {
  resume_attempt <- 0L

  agent_fail_then_ok <- Agent(
    id = "agent_fail",
    .data = function(runtime) {
      resume_attempt <<- resume_attempt + 1L
      if (resume_attempt <= 1L) stop("Critical failure!")
      "Resumed successfully"
    }
  )

  manifest <- Manifest(
    master = MasterPolicy(name = "sync_resume_test", version = "1.0.0",
                          stages = "execution"),
    states = list(
      StatePolicy(name = "state_critical", stage = "execution",
                  agent_id = "agent_fail", priority = 200,
                  max_retry = 0L, critical = TRUE)
    )
  )

  sched <- Scheduler$new(manifest = manifest,
                         agents = list(agent_fail_then_ok))
  sched$on("suspend", function(event) "resume", id = "test_handler")

  completed <- FALSE
  sched$on("scheduler.completed", function(e) completed <<- TRUE)

  sched$start()

  expect_gte(resume_attempt, 2L)
  expect_true(completed)
  expect_true(sched$completed_map$has("state_critical"))
  expect_equal(sched$completed_map$get("state_critical")$status, "finished")
})

test_that("Sync: suspend 'skip' marks state skipped, skips dependents", {
  downstream_ran <- FALSE

  agent_fail <- Agent(id = "agent_fail",
                      .data = function(runtime) stop("Always fails!"))
  agent_downstream <- Agent(
    id = "agent_downstream",
    .data = function(runtime) {
      downstream_ran <<- TRUE
      "Downstream done"
    }
  )

  manifest <- Manifest(
    master = MasterPolicy(name = "sync_skip_test", version = "1.0.0",
                          stages = "execution"),
    states = list(
      StatePolicy(name = "state_critical", stage = "execution",
                  agent_id = "agent_fail", priority = 200,
                  max_retry = 0L, critical = TRUE),
      StatePolicy(name = "state_downstream", stage = "execution",
                  agent_id = "agent_downstream", priority = 100,
                  depends_on = StateDeps(res = "state_critical"))
    )
  )

  sched <- Scheduler$new(manifest = manifest,
                         agents = list(agent_fail, agent_downstream))
  sched$on("suspend", function(event) "skip", id = "test_handler")

  completed <- FALSE
  sched$on("scheduler.completed", function(e) completed <<- TRUE)

  sched$start()

  expect_true(completed)
  expect_true(sched$completed_map$has("state_critical"))
  expect_equal(sched$completed_map$get("state_critical")$status, "skipped")
  expect_false(downstream_ran)
})

test_that("Sync: suspend 'restart_stage' re-initializes stage", {
  restart_attempt <- 0L
  restart_count <- 0L

  agent_fail_then_ok <- Agent(
    id = "agent_fail",
    .data = function(runtime) {
      restart_attempt <<- restart_attempt + 1L
      if (restart_attempt <= 1L) stop("Stage failure!")
      "Stage restart successful"
    }
  )

  manifest <- Manifest(
    master = MasterPolicy(name = "sync_restart_stage_test",
                          version = "1.0.0", stages = "execution"),
    states = list(
      StatePolicy(name = "state_critical", stage = "execution",
                  agent_id = "agent_fail", priority = 200,
                  max_retry = 0L, critical = TRUE)
    )
  )

  sched <- Scheduler$new(manifest = manifest,
                         agents = list(agent_fail_then_ok))
  sched$on("suspend", function(event) {
    restart_count <<- restart_count + 1L
    if (restart_count <= 1L) return("restart_stage")
    return("abort")  # safety fallback
  }, id = "test_handler")

  completed <- FALSE
  sched$on("scheduler.completed", function(e) completed <<- TRUE)

  sched$start()

  expect_gte(restart_attempt, 2L)
  expect_true(completed)
  expect_true(sched$completed_map$has("state_critical"))
  expect_equal(sched$completed_map$get("state_critical")$status, "finished")
})

test_that("Sync: suspend 'abort' fires scheduler.aborted", {
  aborted <- NULL

  agent_fail <- Agent(id = "agent_fail",
                      .data = function(runtime) stop("Always fails!"))

  manifest <- Manifest(
    master = MasterPolicy(name = "sync_abort_test", version = "1.0.0",
                          stages = "execution"),
    states = list(
      StatePolicy(name = "state_critical", stage = "execution",
                  agent_id = "agent_fail", priority = 200,
                  max_retry = 0L, critical = TRUE)
    )
  )

  sched <- Scheduler$new(manifest = manifest, agents = list(agent_fail))
  sched$on("suspend", function(event) "abort", id = "test_handler")
  sched$on("scheduler.aborted", function(e) aborted <<- e)

  expect_error(sched$start(), "[Aa]bort")
  expect_false(is.null(aborted))
})
