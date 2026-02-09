# Tests for the asynchronous AsyncScheduler class
# Covers: onRejected cleanup, on_failure deadlock, start/abort lifecycle,
#         stop/restart + stale callbacks, and critical suspend actions.

# -- onRejected cleanup -------------------------------------------------------

test_that("Async: onRejected cleanup when record_attachment fails", {
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
      name = "async_onrejected_test",
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

  scheduler <- AsyncScheduler$new(manifest = manifest,
                                  agents = list(agent_a, agent_b))

  tracker <- sabotage_record_attachment(scheduler, "state_a")

  errored_count <- 0L
  scheduler$on("runtime.errored", function(event) {
    errored_count <<- errored_count + 1L
  })

  scheduler$start()
  drain_async()

  expect_equal(scheduler$current_stage, "ready")
  expect_true(scheduler$completed_map$has("state_a"))
  expect_equal(scheduler$completed_map$get("state_a")$status, "errored")
  expect_true(agent_b_ran)
  expect_false(scheduler$waiting_pool$has("state_a"))
  expect_gt(errored_count, 0L)
  expect_gt(tracker$count, 0L)
})


# -- on_failure deadlock prevention --------------------------------------------

test_that("Async: on_failure redirect does not deadlock", {
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
      name = "async_onfailure_test",
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

  scheduler <- AsyncScheduler$new(manifest = manifest,
                                  agents = list(agent_a, agent_b))

  redirect_fired <- FALSE
  scheduler$on("runtime.redirect", function(event) {
    redirect_fired <<- TRUE
  })

  scheduler$start()
  drain_async(timeout = 10)

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

test_that("Async: start() emits scheduler.completed on success", {
  completed <- NULL
  aborted <- NULL

  agent_ok <- Agent(id = "agent_ok",
                    .data = function(runtime) "OK")
  manifest <- Manifest(
    master = MasterPolicy(name = "async_start_ok", version = "1.0.0",
                          stages = "run"),
    states = list(
      StatePolicy(name = "state_ok", stage = "run",
                  agent_id = "agent_ok", priority = 100)
    )
  )

  sched <- AsyncScheduler$new(manifest = manifest, agents = list(agent_ok))
  sched$on("scheduler.completed", function(e) completed <<- e)
  sched$on("scheduler.aborted", function(e) aborted <<- e)

  sched$start()
  drain_async()

  expect_false(is.null(completed))
  expect_true(is.null(aborted))
  expect_equal(sched$current_stage, "ready")
})

test_that("Async: start() emits scheduler.aborted on critical abort", {
  completed <- NULL
  aborted <- NULL
  promise_rejected <- FALSE

  agent_fail <- Agent(id = "agent_fail",
                      .data = function(runtime) stop("Critical failure!"))
  manifest <- Manifest(
    master = MasterPolicy(name = "async_start_abort", version = "1.0.0",
                          stages = "run"),
    states = list(
      StatePolicy(name = "state_critical", stage = "run",
                  agent_id = "agent_fail", priority = 100,
                  max_retry = 0L, critical = TRUE)
    )
  )

  sched <- AsyncScheduler$new(manifest = manifest, agents = list(agent_fail))
  sched$on("suspend", function(event) "abort", id = "test_abort")
  sched$on("scheduler.completed", function(e) completed <<- e)
  sched$on("scheduler.aborted", function(e) aborted <<- e)

  p <- sched$start()
  p$then(
    onFulfilled = function(...) NULL,
    onRejected = function(e) {
      promise_rejected <<- TRUE
    }
  )

  drain_async()

  expect_false(is.null(aborted))
  expect_true(is.null(completed))
  expect_true(promise_rejected)
  expect_equal(sched$current_stage, "ready")
})


# -- stop() cleanup + restart + stale callbacks --------------------------------

test_that("Async: stop() clears all structures mid-execution", {
  stopped_event <- NULL

  agent_slow <- Agent(
    id = "agent_slow",
    .data = coro::async(function(runtime) {
      await(coro::async_sleep(5))
      "Should never complete"
    }),
    description = "Slow agent"
  )
  agent_fast <- Agent(
    id = "agent_fast",
    .data = function(runtime) "fast done",
    description = "Fast agent"
  )

  manifest <- Manifest(
    master = MasterPolicy(name = "async_stop_test", version = "1.0.0",
                          stages = "execution"),
    states = list(
      StatePolicy(name = "state_slow", stage = "execution",
                  agent_id = "agent_slow", priority = 200),
      StatePolicy(name = "state_fast", stage = "execution",
                  agent_id = "agent_fast", priority = 100,
                  depends_on = StateDeps(res = "state_slow"))
    )
  )

  sched <- AsyncScheduler$new(manifest = manifest,
                               agents = list(agent_slow, agent_fast))
  sched$on("scheduler.stopped", function(e) stopped_event <<- e)

  sched$start()
  later::run_now(timeoutSecs = 0.2)  # let slow agent dispatch
  sched$stop()
  later::run_now(timeoutSecs = 1)

  expect_equal(sched$current_stage, "ready")
  expect_equal(sched$runtime_map$size(), 0L)
  expect_equal(sched$ready_queue$size(), 0L)

  expect_equal(sched$waiting_pool$size(), 0L)
  expect_equal(sched$completed_map$size(), 0L)
  expect_equal(sched$retry_map$size(), 0L)
  expect_false(isTRUE(sched$suspended))
  expect_true(is.null(sched$suspend_info))
  expect_false(isTRUE(sched$draining))
  expect_false(is.null(stopped_event))
})

test_that("Async: stop() then immediate restart succeeds", {
  run_count <- 0L

  agent_quick <- Agent(
    id = "agent_quick",
    .data = function(runtime) {
      run_count <<- run_count + 1L
      "quick done"
    }
  )
  # Need agent_slow registered even if not used
  agent_slow <- Agent(
    id = "agent_slow",
    .data = coro::async(function(runtime) {
      await(coro::async_sleep(5))
      "slow"
    })
  )

  manifest <- Manifest(
    master = MasterPolicy(name = "async_restart_test", version = "1.0.0",
                          stages = "run"),
    states = list(
      StatePolicy(name = "state_quick", stage = "run",
                  agent_id = "agent_quick", priority = 100)
    )
  )

  sched <- AsyncScheduler$new(manifest = manifest,
                               agents = list(agent_quick, agent_slow))

  # Run 1: start then stop
  sched$start()
  sched$stop()
  later::run_now(timeoutSecs = 0.5)

  # Run 2: restart
  completed <- NULL
  sched$on("scheduler.completed", function(e) completed <<- e)
  sched$start()
  drain_async()

  expect_false(is.null(completed))
  expect_equal(sched$current_stage, "ready")
})

test_that("Async: stale callbacks do not corrupt new run", {
  stale_callback_fired <- FALSE
  new_run_completed <- FALSE

  agent_stale <- Agent(
    id = "agent_stale",
    .data = coro::async(function(runtime) {
      await(coro::async_sleep(1))
      stale_callback_fired <<- TRUE
      "Stale result"
    })
  )
  agent_new <- Agent(
    id = "agent_new",
    .data = function(runtime) {
      new_run_completed <<- TRUE
      "New result"
    }
  )

  manifest_old <- Manifest(
    master = MasterPolicy(name = "stale_callback_test", version = "1.0.0",
                          stages = "run"),
    states = list(
      StatePolicy(name = "state_stale", stage = "run",
                  agent_id = "agent_stale", priority = 100)
    )
  )
  manifest_new <- Manifest(
    master = MasterPolicy(name = "new_run_test", version = "1.0.0",
                          stages = "run"),
    states = list(
      StatePolicy(name = "state_new", stage = "run",
                  agent_id = "agent_new", priority = 100)
    )
  )

  sched_old <- AsyncScheduler$new(manifest = manifest_old,
                                   agents = list(agent_stale))
  stale_events <- 0L
  sched_old$on("runtime.resolved", function(event) {
    stale_events <<- stale_events + 1L
  })

  sched_old$start()
  later::run_now(timeoutSecs = 0.2)
  sched_old$stop()

  sched_new <- AsyncScheduler$new(manifest = manifest_new,
                                   agents = list(agent_new))
  new_completed <- NULL
  sched_new$on("scheduler.completed", function(e) new_completed <<- e)
  sched_new$start()

  drain_async(timeout = 5)

  expect_true(new_run_completed)
  expect_equal(sched_old$completed_map$size(), 0L)
  expect_equal(stale_events, 0L)
  expect_false(is.null(new_completed))
})


# -- Critical suspend: resume, skip, restart_stage, abort ----------------------

test_that("Async: suspend 'resume' re-runs state with attempt reset", {
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
    master = MasterPolicy(name = "async_resume_test", version = "1.0.0",
                          stages = "execution"),
    states = list(
      StatePolicy(name = "state_critical", stage = "execution",
                  agent_id = "agent_fail", priority = 200,
                  max_retry = 0L, critical = TRUE)
    )
  )

  sched <- AsyncScheduler$new(manifest = manifest,
                               agents = list(agent_fail_then_ok))
  sched$on("suspend", function(event) "resume", id = "test_handler")

  completed <- FALSE
  sched$on("scheduler.completed", function(e) completed <<- TRUE)

  sched$start()
  drain_async()

  expect_gte(resume_attempt, 2L)
  expect_true(completed)
  expect_true(sched$completed_map$has("state_critical"))
  expect_equal(sched$completed_map$get("state_critical")$status, "finished")
})

test_that("Async: suspend 'skip' marks state skipped, skips dependents", {
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
    master = MasterPolicy(name = "async_skip_test", version = "1.0.0",
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

  sched <- AsyncScheduler$new(manifest = manifest,
                               agents = list(agent_fail, agent_downstream))
  sched$on("suspend", function(event) "skip", id = "test_handler")

  completed <- FALSE
  sched$on("scheduler.completed", function(e) completed <<- TRUE)

  sched$start()
  drain_async()

  expect_true(completed)
  expect_true(sched$completed_map$has("state_critical"))
  expect_equal(sched$completed_map$get("state_critical")$status, "skipped")
  expect_false(downstream_ran)
})

test_that("Async: suspend 'restart_stage' re-initializes stage", {
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
    master = MasterPolicy(name = "async_restart_stage_test",
                          version = "1.0.0", stages = "execution"),
    states = list(
      StatePolicy(name = "state_critical", stage = "execution",
                  agent_id = "agent_fail", priority = 200,
                  max_retry = 0L, critical = TRUE)
    )
  )

  sched <- AsyncScheduler$new(manifest = manifest,
                               agents = list(agent_fail_then_ok))
  sched$on("suspend", function(event) {
    restart_count <<- restart_count + 1L
    if (restart_count <= 1L) return("restart_stage")
    return("abort")
  }, id = "test_handler")

  completed <- FALSE
  sched$on("scheduler.completed", function(e) completed <<- TRUE)

  sched$start()
  drain_async()

  expect_gte(restart_attempt, 2L)
  expect_true(completed)
  expect_true(sched$completed_map$has("state_critical"))
  expect_equal(sched$completed_map$get("state_critical")$status, "finished")
})

test_that("Async: suspend 'abort' rejects promise and fires scheduler.aborted", {
  aborted <- NULL
  promise_rejected <- FALSE

  agent_fail <- Agent(id = "agent_fail",
                      .data = function(runtime) stop("Always fails!"))

  manifest <- Manifest(
    master = MasterPolicy(name = "async_abort_test", version = "1.0.0",
                          stages = "execution"),
    states = list(
      StatePolicy(name = "state_critical", stage = "execution",
                  agent_id = "agent_fail", priority = 200,
                  max_retry = 0L, critical = TRUE)
    )
  )

  sched <- AsyncScheduler$new(manifest = manifest, agents = list(agent_fail))
  sched$on("suspend", function(event) "abort", id = "test_handler")
  sched$on("scheduler.aborted", function(e) aborted <<- e)

  p <- sched$start()
  p$then(
    onFulfilled = function(...) NULL,
    onRejected = function(e) {
      promise_rejected <<- TRUE
    }
  )

  drain_async()

  expect_false(is.null(aborted))
  expect_true(promise_rejected)
  expect_equal(sched$current_stage, "ready")
})
