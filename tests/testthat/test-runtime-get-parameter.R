# Tests for AgentRuntime$get_parameter() and master_policy cascading

# Helper: create a properly initialized AgentContext
make_test_context <- function() {
  test_path <- file.path(tempdir(), "tricobbler", "context")
  dir.create(test_path, showWarnings = FALSE, recursive = TRUE)
  ctx <- AgentContext$new(path = test_path)
  ctx$init_resources()
  ctx
}

# Helper: build a minimal AgentRuntime with master and state-level params
make_test_runtime <- function(local_args = list(),
                              master_params = list()) {
  agent <- Agent(
    id = "test_agent",
    .data = function(runtime) "ok",
    description = "Test agent"
  )
  test_path <- file.path(tempdir(), "tricobbler", "context")
  dir.create(test_path, showWarnings = FALSE, recursive = TRUE)
  ctx <- AgentContext$new(path = test_path)
  ctx$init_resources()
  policy <- StatePolicy(
    name = "test_state",
    stage = "test_stage",
    agent_id = "test_agent",
    parameters = list(args = local_args)
  )
  master <- MasterPolicy(
    name = "test",
    version = "1.0.0",
    stages = "test_stage",
    parameters = master_params
  )
  AgentRuntime$new(
    agent = agent,
    context = ctx,
    policy = policy,
    master_policy = master
  )
}

# ---- get_parameter cascade tests --------------------------------------------

test_that("get_parameter returns local param when present", {
  rt <- make_test_runtime(
    local_args = list(location = "Houston"),
    master_params = list(location = "Austin")
  )
  # Local wins over global in cascade mode

  expect_equal(rt$get_parameter("location"), "Houston")
})

test_that("get_parameter falls back to global when local missing", {
  rt <- make_test_runtime(
    local_args = list(),
    master_params = list(location = "Austin")
  )
  expect_equal(rt$get_parameter("location"), "Austin")
})

test_that("get_parameter returns missing when key absent everywhere", {
  rt <- make_test_runtime()
  expect_null(rt$get_parameter("nothing"))
  expect_equal(rt$get_parameter("nothing", missing = "default"), "default")
})

test_that("get_parameter levels='local' ignores global", {
  rt <- make_test_runtime(
    local_args = list(),
    master_params = list(location = "Austin")
  )
  expect_null(rt$get_parameter("location", levels = "local"))
})

test_that("get_parameter levels='global' ignores local", {
  rt <- make_test_runtime(
    local_args = list(location = "Houston"),
    master_params = list(location = "Austin")
  )
  expect_equal(rt$get_parameter("location", levels = "global"), "Austin")
})

test_that("get_parameter levels='global' returns missing when no master", {
  agent <- Agent(
    id = "test_agent",
    .data = function(runtime) "ok",
    description = "Test agent"
  )
  ctx <- make_test_context()
  policy <- StatePolicy(
    name = "test_state",
    stage = "test_stage",
    agent_id = "test_agent",
    parameters = list(args = list(a = 1))
  )
  rt <- AgentRuntime$new(
    agent = agent,
    context = ctx,
    policy = policy,
    master_policy = NULL
  )
  expect_null(rt$get_parameter("a", levels = "global"))
  expect_equal(rt$get_parameter("a", levels = "cascade"), 1)
})

# ---- master_policy active binding -------------------------------------------

test_that("master_policy active binding returns the master policy", {
  rt <- make_test_runtime(master_params = list(x = 42))
  expect_true(S7::S7_inherits(rt$master_policy, MasterPolicy))
  expect_equal(rt$master_policy@parameters$x, 42)
})

test_that("master_policy is NULL when not provided", {
  agent <- Agent(
    id = "test_agent",
    .data = function(runtime) "ok",
    description = "Test agent"
  )
  ctx <- make_test_context()
  policy <- StatePolicy(
    name = "s", stage = "t", agent_id = "test_agent"
  )
  rt <- AgentRuntime$new(agent = agent, context = ctx, policy = policy)
  expect_null(rt$master_policy)
})

# ---- parameter cascading in as_agent (function agents) ----------------------

test_that("as_agent cascades master params to function agents", {
  received <- list()

  fn <- function(location, category) {
    received <<- list(location = location, category = category)
    "ok"
  }
  agent <- as_agent(fn, id = "test_fn")

  master <- MasterPolicy(
    name = "test",
    version = "1.0.0",
    stages = "s1",
    parameters = list(location = "Global City", category = "weather")
  )
  policy <- StatePolicy(
    name = "st", stage = "s1", agent_id = "test_fn",
    parameters = list()
  )
  ctx <- make_test_context()
  rt <- AgentRuntime$new(
    agent = agent, context = ctx, policy = policy,
    master_policy = master
  )

  rt$run()

  expect_equal(received$location, "Global City")
  expect_equal(received$category, "weather")
})

test_that("state-level args override global params for function agents", {
  received <- list()

  fn <- function(location) {
    received <<- list(location = location)
    "ok"
  }
  agent <- as_agent(fn, id = "override_fn")

  master <- MasterPolicy(
    name = "test",
    version = "1.0.0",
    stages = "s1",
    parameters = list(location = "Global City")
  )
  # State-level overrides global
  policy <- StatePolicy(
    name = "st", stage = "s1", agent_id = "override_fn",
    parameters = list(args = list(location = "Local Town"))
  )
  ctx <- make_test_context()
  rt <- AgentRuntime$new(
    agent = agent, context = ctx, policy = policy,
    master_policy = master
  )

  rt$run()

  expect_equal(received$location, "Local Town")
})

test_that("dependencies override both global and local params", {
  # This test verifies the precedence: dependency > local > global
  received <- list()

  fn <- function(data_value) {
    received <<- list(data_value = data_value)
    "ok"
  }

  # Agent A produces a result
  agent_a <- Agent(
    id = "producer",
    .data = function(runtime) "from dependency",
    description = "Produces data"
  )

  # Agent B receives data via dependency
  agent_b <- as_agent(fn, id = "consumer")

  master <- MasterPolicy(
    name = "test",
    version = "1.0.0",
    stages = "s1",
    # Global param tries to set data_value
    parameters = list(data_value = "from global")
  )

  policy_a <- StatePolicy(
    name = "producer_state", stage = "s1", agent_id = "producer"
  )
  policy_b <- StatePolicy(
    name = "consumer_state", stage = "s1", agent_id = "consumer",
    # Local param also tries to set data_value
    parameters = list(args = list(data_value = "from local")),
    depends_on = StateDeps(.list = list(
      data_value = list(state = "producer_state")
    ))
  )

  manifest <- Manifest(
    master = master,
    states = list(policy_a, policy_b)
  )

  scheduler <- Scheduler$new(
    manifest = manifest,
    agents = list(agent_a, agent_b)
  )
  scheduler$start()

  # Dependency wins over both local and global
  expect_equal(received$data_value, "from dependency")
})
