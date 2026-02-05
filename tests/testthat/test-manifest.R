test_that("MasterPolicy validates stages correctly", {
  # Valid master policy
  mp <- MasterPolicy(
    name = "test",
    version = "1.0.0",
    stages = c("idle", "working"),
    parameters = list()
  )
  expect_true(S7::S7_inherits(mp, MasterPolicy))
  expect_equal(mp@stages, c("idle", "working"))

  # Stages should be lowercased
  expect_error(
    MasterPolicy(
      name = "test", version = "1.0.0", stages = character(0)
    ),
    "number of stages cannot be 0"
  )

  # Duplicates not allowed
  expect_error(
    MasterPolicy(
      name = "test", version = "1.0.0", stages = c("idle", "IDLE")
    ),
    "duplicated"
  )

  # Invalid characters
  expect_error(
    MasterPolicy(
      name = "test", version = "1.0.0", stages = c("idle!", "working")
    ),
    "can only contain letters"
  )
})

test_that("StatePolicy validates stage correctly", {
  sp <- StatePolicy(
    name = "state1",
    stage = "idle",
    description = "test state",
    agent_id = "agent1",
    parameters = list()
  )
  expect_true(S7::S7_inherits(sp, StatePolicy))
  expect_equal(sp@stage, "idle")

  # Stage cannot be blank
  expect_error(
    StatePolicy(
      name = "state1", stage = "",
      description = "test", agent_id = "agent1"
    ),
    "non-blank"
  )

  # Stage must be single string
  expect_error(
    StatePolicy(
      name = "state1", stage = c("idle", "working"),
      description = "test", agent_id = "agent1"
    ),
    "non-blank single character string"
  )
})

test_that("Manifest validates stage-state correspondence", {
  mp <- MasterPolicy(
    name = "test",
    version = "1.0.0",
    stages = c("idle", "working"),
    parameters = list()
  )

  sp1 <- StatePolicy(
    name = "init", stage = "idle",
    description = "init", agent_id = "agent_init"
  )
  sp2 <- StatePolicy(
    name = "work", stage = "working",
    description = "work", agent_id = "agent_work"
  )

  # Valid manifest
  mf <- Manifest(master = mp, states = list(sp1, sp2))
  expect_true(S7::S7_inherits(mf, Manifest))
  expect_equal(mf@name, "test")

  # Missing stage should fail
  expect_error(
    Manifest(master = mp, states = list(sp1)),
    "Missing stages.*working"
  )

  # Multiple states per stage is allowed
  sp3 <- StatePolicy(
    name = "work2", stage = "working",
    description = "work again", agent_id = "agent_work2"
  )
  mf2 <- Manifest(master = mp, states = list(sp1, sp2, sp3))
  expect_true(S7::S7_inherits(mf2, Manifest))
})

test_that("manifest_write creates valid YAML file", {
  mp <- MasterPolicy(
    name = "demo-workflow",
    version = "1.0.0",
    stages = c("idle", "working"),
    parameters = list(timeout = 300, retries = 3)
  )

  sp1 <- StatePolicy(
    name = "init",
    stage = "idle",
    description = "Initial idle state",
    agent_id = "agent_init",
    parameters = list(wait = TRUE)
  )

  sp2 <- StatePolicy(
    name = "process",
    stage = "working",
    description = "Processing state with execution",
    agent_id = "agent_process",
    parameters = list(max_duration = 600)
  )

  manifest <- Manifest(master = mp, states = list(sp1, sp2))

  tmp <- tempfile(fileext = ".yaml")
  on.exit(unlink(tmp), add = TRUE)

  result <- manifest_write(manifest, tmp)

  expect_true(file.exists(tmp))
  expect_gt(file.size(tmp), 0)

  # Check YAML content
  yaml_content <- yaml::read_yaml(tmp)
  expect_equal(yaml_content$master$name, "demo-workflow")
  expect_equal(yaml_content$master$version, "1.0.0")
  expect_equal(yaml_content$master$stages, c("idle", "working"))
  expect_equal(yaml_content$master$parameters$timeout, 300)
  expect_length(yaml_content$states, 2)
})

test_that("manifest_read reconstructs Manifest correctly", {
  mp <- MasterPolicy(
    name = "roundtrip-test",
    version = "2.1.0",
    stages = c("alpha", "beta", "gamma"),
    parameters = list(verbose = TRUE)
  )

  sp1 <- StatePolicy(
    name = "s1", stage = "alpha",
    description = "First state", agent_id = "agent_s1"
  )
  sp2 <- StatePolicy(
    name = "s2", stage = "beta",
    description = "Second state", agent_id = "agent_s2"
  )
  sp3 <- StatePolicy(
    name = "s3", stage = "gamma",
    description = "Third state", agent_id = "agent_s3"
  )

  original <- Manifest(master = mp, states = list(sp1, sp2, sp3))

  tmp <- tempfile(fileext = ".yaml")
  on.exit(unlink(tmp), add = TRUE)

  manifest_write(original, tmp)
  reconstructed <- manifest_read(tmp)

  # Verify master policy
  expect_equal(reconstructed@master@name, "roundtrip-test")
  expect_equal(reconstructed@master@version, "2.1.0")
  expect_equal(reconstructed@master@stages, c("alpha", "beta", "gamma"))
  expect_equal(reconstructed@master@parameters$verbose, TRUE)

  # Verify states
  expect_length(reconstructed@states, 3)
  expect_equal(reconstructed@states[[1]]@name, "s1")
  expect_equal(reconstructed@states[[1]]@stage, "alpha")
  expect_equal(reconstructed@states[[2]]@name, "s2")
  expect_equal(reconstructed@states[[3]]@stage, "gamma")
})

test_that("manifest_read validates on deserialization", {
  # Create a manifest with missing stage implementation
  tmp <- tempfile(fileext = ".yaml")
  on.exit(unlink(tmp), add = TRUE)

  invalid_yaml <- list(
    master = list(
      name = "invalid",
      description = "test",
      version = "1.0.0",
      stages = c("idle", "working", "done"),
      parameters = list()
    ),
    states = list(
      list(
        name = "s1", stage = "idle", description = "idle state",
        agent_id = "agent_s1", parameters = list()
      ),
      list(
        name = "s2", stage = "working", description = "work state",
        agent_id = "agent_s2", parameters = list()
      )
      # Missing "done" stage!
    )
  )

  yaml::write_yaml(invalid_yaml, tmp)

  expect_error(
    manifest_read(tmp),
    "Missing stages.*done"
  )
})

test_that("manifest roundtrip with complex descriptions", {
  mp <- MasterPolicy(
    name = "desc-test",
    version = "1.0.0",
    stages = c("stage1"),
    parameters = list()
  )

  # Test multiline description handling
  sp <- StatePolicy(
    name = "s1",
    stage = "stage1",
    description = c("Line 1", "Line 2", "Line 3"),
    agent_id = "agent_s1",
    parameters = list()
  )

  original <- Manifest(master = mp, states = list(sp))

  tmp <- tempfile(fileext = ".yaml")
  on.exit(unlink(tmp), add = TRUE)

  manifest_write(original, tmp)
  reconstructed <- manifest_read(tmp)

  # Description should be collapsed (YAML preserves newlines,
  # so it becomes multiline string)
  expect_type(reconstructed@states[[1]]@description, "character")
  expect_length(reconstructed@states[[1]]@description, 1)
  # YAML writes multiline strings with newlines preserved
  expect_match(reconstructed@states[[1]]@description, "Line")
})

test_that("manifest handles empty parameters", {
  mp <- MasterPolicy(
    name = "empty-params",
    version = "1.0.0",
    stages = c("s1"),
    parameters = list()
  )

  sp <- StatePolicy(
    name = "state",
    stage = "s1",
    description = "test",
    agent_id = "agent_state",
    parameters = list()
  )

  manifest <- Manifest(master = mp, states = list(sp))

  tmp <- tempfile(fileext = ".yaml")
  on.exit(unlink(tmp), add = TRUE)

  manifest_write(manifest, tmp)
  reconstructed <- manifest_read(tmp)

  expect_length(reconstructed@master@parameters, 0)
  expect_length(reconstructed@states[[1]]@parameters, 0)
})

test_that("StatePolicy priority validation works", {
  # Valid priorities
  sp1 <- StatePolicy(
    name = "s1", stage = "idle",
    agent_id = "agent_s1", priority = 0L
  )
  expect_equal(sp1@priority, 0L)

  sp2 <- StatePolicy(
    name = "s2", stage = "idle",
    agent_id = "agent_s2", priority = 999L
  )
  expect_equal(sp2@priority, 999L)

  # Default priority
  sp3 <- StatePolicy(name = "s3", stage = "idle", agent_id = "agent_s3")
  expect_equal(sp3@priority, 100L)

  # NA treated as default
  sp4 <- StatePolicy(
    name = "s4", stage = "idle",
    agent_id = "agent_s4", priority = NA
  )
  expect_equal(sp4@priority, 100L)

  # NULL treated as default (via setter)
  sp5 <- StatePolicy(
    name = "s5", stage = "idle",
    agent_id = "agent_s5", priority = NULL
  )
  expect_equal(sp5@priority, 100L)

  # Invalid priorities
  expect_error(
    StatePolicy(
      name = "s", stage = "idle",
      agent_id = "agent_s", priority = -1L
    ),
    "between 0 and 999"
  )

  expect_error(
    StatePolicy(
      name = "s", stage = "idle",
      agent_id = "agent_s", priority = 1000L
    ),
    "between 0 and 999"
  )
})

test_that("Critical states must have priority >= 1", {
  # Valid critical state
  sp <- StatePolicy(
    name = "s", stage = "idle",
    agent_id = "agent_s", priority = 1L, critical = TRUE
  )
  expect_true(sp@critical)
  expect_equal(sp@priority, 1L)

  # High priority critical
  sp2 <- StatePolicy(
    name = "s2", stage = "idle",
    agent_id = "agent_s2", priority = 900L, critical = TRUE
  )
  expect_true(sp2@critical)
  expect_equal(sp2@priority, 900L)

  # Invalid: critical with priority 0
  expect_error(
    StatePolicy(
      name = "s", stage = "idle",
      agent_id = "agent_s", priority = 0L, critical = TRUE
    ),
    "critical states must have priority >= 1"
  )
})

test_that("Manifest validates critical priority uniqueness", {
  mp <- MasterPolicy(
    name = "test",
    version = "1.0.0",
    stages = c("executing"),
    parameters = list()
  )

  # Valid: critical state with unique priority
  sp1 <- StatePolicy(
    name = "critical1", stage = "executing",
    agent_id = "agent_critical1",
    priority = 900L, critical = TRUE
  )
  sp2 <- StatePolicy(
    name = "normal1", stage = "executing",
    agent_id = "agent_normal1",
    priority = 100L, critical = FALSE
  )

  mf <- Manifest(master = mp, states = list(sp1, sp2))
  expect_true(S7::S7_inherits(mf, Manifest))

  # Invalid: another state in same stage with same priority
  # as critical
  sp3 <- StatePolicy(
    name = "conflict", stage = "executing",
    agent_id = "agent_conflict",
    priority = 900L, critical = FALSE
  )
  expect_error(
    Manifest(master = mp, states = list(sp1, sp3)),
    "cannot share its priority"
  )

  # Valid: critical states with different priorities in same stage
  sp4 <- StatePolicy(
    name = "critical2", stage = "executing",
    agent_id = "agent_critical2",
    priority = 800L, critical = TRUE
  )
  mf2 <- Manifest(master = mp, states = list(sp1, sp4))
  expect_true(S7::S7_inherits(mf2, Manifest))

  # Valid: non-critical states can share priorities
  # with each other
  sp5 <- StatePolicy(
    name = "normal2", stage = "executing",
    agent_id = "agent_normal2",
    priority = 100L, critical = FALSE
  )
  mf3 <- Manifest(master = mp, states = list(sp2, sp5))
  expect_true(S7::S7_inherits(mf3, Manifest))
})

test_that("Priority ordering makes sense for multi-state workflows", {
  mp <- MasterPolicy(
    name = "complex",
    version = "1.0.0",
    stages = c("executing"),
    parameters = list()
  )

  # Create states with different priorities
  sp_high <- StatePolicy(
    name = "high_priority", stage = "executing",
    agent_id = "agent_high",
    priority = 900L, critical = TRUE
  )
  sp_med <- StatePolicy(
    name = "medium_priority", stage = "executing",
    agent_id = "agent_med",
    priority = 500L
  )
  sp_low <- StatePolicy(
    name = "low_priority", stage = "executing",
    agent_id = "agent_low",
    priority = 100L
  )
  sp_lowest <- StatePolicy(
    name = "lowest_priority", stage = "executing",
    agent_id = "agent_lowest",
    priority = 0L
  )

  mf <- Manifest(master = mp, states = list(sp_high, sp_med, sp_low, sp_lowest))
  expect_true(S7::S7_inherits(mf, Manifest))

  # Verify priorities
  priorities <- vapply(mf@states, function(s) s@priority, integer(1))
  expect_equal(priorities, c(900L, 500L, 100L, 0L))

  # Verify only one is critical
  critical_flags <- vapply(mf@states, function(s) s@critical, logical(1))
  expect_equal(sum(critical_flags), 1)
  expect_true(mf@states[[1]]@critical)
})

# -----------------------------------------------------------------------------
# StateDeps Class Tests
# -----------------------------------------------------------------------------

test_that("StateDeps validates empty list correctly", {
  deps <- StateDeps()
  expect_true(S7::S7_inherits(deps, StateDeps))
  expect_length(deps@deps, 0)

  deps2 <- StateDeps()
  expect_true(S7::S7_inherits(deps2, StateDeps))
})

test_that("StateDeps validates well-formed entries", {
  # Minimal entry (only required 'state')
  deps <- StateDeps(
    my_param = list(state = "validator")
  )
  expect_true(S7::S7_inherits(deps, StateDeps))
  expect_equal(deps@deps$my_param$state, "validator")

  # Full entry with all fields
  deps2 <- StateDeps(
    result_data = list(state = "parser", field = "result", stage = "planning")
  )
  expect_equal(deps2@deps$result_data$field, "result")
  expect_equal(deps2@deps$result_data$stage, "planning")

  # Multiple dependencies
  deps3 <- StateDeps(
    validation_result = list(state = "validator", field = "result"),
    parsed_data = list(state = "parser", field = "description")
  )
  expect_length(deps3@deps, 2)
})

test_that("StateDeps rejects unnamed lists", {
  expect_error(
    StateDeps(list(state = "foo")),
    "named list"
  )

  # Partially named
  expect_error(
    StateDeps(.list = list(a = list(state = "foo"), list(state = "bar"))),
    "named list"
  )
})

test_that("StateDeps rejects missing required 'state' field", {
  expect_error(
    StateDeps(deps = list(my_param = list(field = "result"))),
    "missing required 'state' field"
  )

  expect_error(
    StateDeps(deps = list(my_param = list())),
    "missing required 'state' field"
  )
})

test_that("StateDeps rejects invalid 'state' values", {
  expect_error(
    StateDeps(.list = list(p = list(state = ""))),
    "non-blank"
  )

  expect_error(
    StateDeps(.list = list(p = list(state = NA_character_))),
    "non-blank"
  )

  expect_error(
    StateDeps(.list = list(p = list(state = c("a", "b")))),
    "single non-blank"
  )
})

test_that("StateDeps rejects invalid 'field' values", {
  expect_error(
    StateDeps(.list = list(p = list(state = "foo", field = "invalid"))),
    "'result' or 'description'"
  )

  expect_error(
    StateDeps(p = list(state = "foo", field = "")),
    "'result' or 'description'"
  )
})

test_that("StateDeps rejects invalid 'stage' values", {
  expect_error(
    StateDeps(.list = list(p = list(state = "foo", stage = ""))),
    "non-blank character string or NULL"
  )

  expect_error(
    StateDeps(.list = list(p = list(state = "foo", stage = NA_character_))),
    "non-blank character string or NULL"
  )
})

test_that("StateDeps rejects unknown fields", {
  expect_error(
    StateDeps(p = list(state = "foo", extra = "nope")),
    "unknown fields.*'extra'"
  )
})

test_that("StateDeps rejects duplicate parameter names", {
  # This is caught by R's named list behavior, but let's be explicit
  deps_list <- list(state = "foo")
  bad_list <- list(deps_list, deps_list)
  names(bad_list) <- c("a", "a")
  expect_error(
    StateDeps(.list = bad_list),
    "unique"
  )
})

# -----------------------------------------------------------------------------
# StatePolicy depends_on Property Tests
# -----------------------------------------------------------------------------

test_that("StatePolicy accepts depends_on property", {
  sp <- StatePolicy(
    name = "aggregator",
    stage = "executing",
    description = "Aggregates results",
    agent_id = "aggregator_agent",
    depends_on = StateDeps(
      validation_result = list(state = "validator", field = "result"),
      parsed_data = list(state = "parser")
    )
  )

  expect_true(S7::S7_inherits(sp, StatePolicy))
  expect_true(S7::S7_inherits(sp@depends_on, StateDeps))
  expect_length(sp@depends_on@deps, 2)
  expect_equal(sp@depends_on@deps$validation_result$state, "validator")
})

test_that("StatePolicy default depends_on is empty StateDeps", {
  sp <- StatePolicy(
    name = "simple",
    stage = "executing",
    description = "Simple state",
    agent_id = "agent1"
  )

  expect_true(S7::S7_inherits(sp@depends_on, StateDeps))
  expect_length(sp@depends_on@deps, 0)
})

test_that("StatePolicy accessibility accepts 'explicit' value", {
  sp <- StatePolicy(
    name = "explicit_state",
    stage = "executing",
    description = "Uses explicit dependencies",
    agent_id = "agent1",
    accessibility = "explicit",
    depends_on = StateDeps(
      input = list(state = "prior_state")
    )
  )

  expect_equal(sp@accessibility, "explicit")
})

test_that("StatePolicy accessibility rejects invalid values", {
  expect_error(
    StatePolicy(
      name = "bad", stage = "executing",
      agent_id = "agent1",
      accessibility = "invalid"
    ),
    "accessibility must be one of"
  )
})

# -----------------------------------------------------------------------------
# YAML Serialization with depends_on
# -----------------------------------------------------------------------------

test_that("manifest roundtrip preserves depends_on", {
  mp <- MasterPolicy(
    name = "async-workflow",
    version = "1.0.0",
    stages = c("planning", "executing"),
    parameters = list()
  )

  sp1 <- StatePolicy(
    name = "planner", stage = "planning",
    description = "Plan the task", agent_id = "planner_agent"
  )

  sp2 <- StatePolicy(
    name = "validator", stage = "executing",
    description = "Validate inputs", agent_id = "validator_agent",
    priority = 900L
  )

  sp3 <- StatePolicy(
    name = "aggregator", stage = "executing",
    description = "Aggregate results", agent_id = "aggregator_agent",
    priority = 50L,
    accessibility = "explicit",
    depends_on = StateDeps(
      validation_result = list(state = "validator", field = "result"),
      plan = list(state = "planner", field = "result", stage = "planning")
    )
  )

  original <- Manifest(master = mp, states = list(sp1, sp2, sp3))

  tmp <- tempfile(fileext = ".yaml")
  on.exit(unlink(tmp), add = TRUE)

  manifest_write(original, tmp)
  reconstructed <- manifest_read(tmp)

  # Verify depends_on preserved
  agg_state <- reconstructed@states[[3]]
  expect_equal(agg_state@name, "aggregator")
  expect_equal(agg_state@accessibility, "explicit")
  expect_true(S7::S7_inherits(agg_state@depends_on, StateDeps))
  expect_length(agg_state@depends_on@deps, 2)

  # Check dependency details
  expect_equal(
    agg_state@depends_on@deps$validation_result$state,
    "validator"
  )
  expect_equal(
    agg_state@depends_on@deps$plan$stage,
    "planning"
  )
})

test_that("manifest roundtrip handles empty depends_on", {
  mp <- MasterPolicy(
    name = "simple-workflow",
    version = "1.0.0",
    stages = c("working"),
    parameters = list()
  )

  sp <- StatePolicy(
    name = "worker", stage = "working",
    description = "Does work", agent_id = "worker_agent"
  )

  original <- Manifest(master = mp, states = list(sp))

  tmp <- tempfile(fileext = ".yaml")
  on.exit(unlink(tmp), add = TRUE)

  manifest_write(original, tmp)
  reconstructed <- manifest_read(tmp)

  # Empty depends_on should roundtrip correctly
  expect_true(S7::S7_inherits(reconstructed@states[[1]]@depends_on, StateDeps))
  expect_length(reconstructed@states[[1]]@depends_on@deps, 0)
})
