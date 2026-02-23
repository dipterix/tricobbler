# ====================================================================
# Tests for workflow-io.R
# ====================================================================
# Tests cover: workflow_save / workflow_load / workflow_list
# plus internal helpers: manifest_to_workflow_list,
# workflow_list_to_manifest, provider_name_to_suffix

# --- provider_name_to_suffix ---
test_that("provider_name_to_suffix maps known providers", {
  provider_name_to_suffix <- tricobbler:::provider_name_to_suffix
  expect_equal(provider_name_to_suffix("Ollama"), "ollama")
  expect_equal(provider_name_to_suffix("Anthropic"), "anthropic")
  expect_equal(provider_name_to_suffix("OpenAI"), "openai")
  expect_equal(provider_name_to_suffix("Azure/OpenAI"), "azure_openai")
  expect_equal(provider_name_to_suffix("Google/Gemini"), "google_gemini")
})

test_that("provider_name_to_suffix falls back gracefully for unknown", {
  provider_name_to_suffix <- tricobbler:::provider_name_to_suffix
  # Unknown provider -- should normalize to lowercase with underscores
  result <- provider_name_to_suffix("Custom/Provider")
  expect_type(result, "character")
  expect_equal(result, "custom_provider")
})


# --- manifest_to_workflow_list / workflow_list_to_manifest roundtrip ---
test_that("manifest roundtrips through workflow list", {
  mp <- MasterPolicy(
    name = "roundtrip-test",
    description = "A test manifest",
    version = "1.0.0",
    stages = c("triage", "executing"),
    parameters = list(timeout = 300)
  )
  sp1 <- StatePolicy(
    name = "extract",
    stage = "triage",
    description = "Extract paths",
    agent_id = "extractor",
    priority = 200L,
    critical = TRUE
  )
  sp2 <- StatePolicy(
    name = "execute",
    stage = "executing",
    description = "Run the executor",
    agent_id = "runner",
    priority = 100L,
    depends_on = StateDeps(
      extracted = list(state = "extract", field = "result", stage = "triage")
    )
  )
  m <- Manifest(master = mp, states = list(sp1, sp2))

  wf_list <- manifest_to_workflow_list(m)

  # Structure checks
  expect_true(is.list(wf_list))
  expect_true(all(c("name", "version", "stages", "parameters") %in% names(wf_list)))
  expect_named(wf_list$stages, c("triage", "executing"))
  expect_length(wf_list$stages$triage, 1)
  expect_length(wf_list$stages$executing, 1)

  # The stage field should be removed from individual states
  expect_null(wf_list$stages$triage[[1]]$stage)
  expect_null(wf_list$stages$executing[[1]]$stage)

  # Reconstruct
  m2 <- workflow_list_to_manifest(wf_list)
  expect_true(S7::S7_inherits(m2, Manifest))
  expect_equal(m2@master@name, "roundtrip-test")
  expect_equal(m2@master@version, "1.0.0")
  expect_length(m2@states, 2)
  expect_equal(m2@states[[1]]@stage, "triage")
  expect_equal(m2@states[[2]]@stage, "executing")
  expect_equal(m2@states[[2]]@depends_on@deps$extracted$state, "extract")
  expect_equal(m2@states[[2]]@depends_on@deps$extracted$stage, "triage")
})


test_that("manifest roundtrip preserves depends_on and return_type in parameters", {
  # Build a manifest with rich depends_on and return_type in parameters
  mp <- MasterPolicy(
    name = "deps-return-type-test",
    description = "Test depends_on and return_type roundtrip",
    version = "2.0.0",
    stages = c("planning", "executing", "review"),
    parameters = list(global_timeout = 600)
  )

  sp_plan <- StatePolicy(
    name = "planner",
    stage = "planning",
    description = "Generate a plan",
    agent_id = "llm_planner",
    priority = 100L,
    parameters = list(
      system_prompt = "You are a planner.",
      user_prompt = "Create plan for the task.",
      return_type = list(
        type = "object",
        properties = list(
          steps = list(
            type = "array",
            description = "Ordered steps",
            items = list(type = "string")
          ),
          summary = list(type = "string", description = "Plan summary")
        )
      )
    )
  )

  sp_validate <- StatePolicy(
    name = "validator",
    stage = "executing",
    description = "Validate the plan",
    agent_id = "validator_agent",
    priority = 900L,
    critical = TRUE,
    accessibility = "explicit",
    depends_on = StateDeps(
      plan_output = list(
        state = "planner",
        field = "result",
        stage = "planning"
      )
    )
  )

  sp_execute <- StatePolicy(
    name = "executor",
    stage = "executing",
    description = "Execute the plan",
    agent_id = "exec_agent",
    priority = 100L,
    accessibility = "explicit",
    max_retry = 2L,
    on_failure = "validator",
    depends_on = StateDeps(
      plan_output = list(
        state = "planner",
        field = "result",
        stage = "planning"
      ),
      validation = list(
        state = "validator",
        field = "description"
      )
    ),
    parameters = list(
      return_type = list(
        type = "object",
        properties = list(
          status = list(
            type = "enum",
            enum = list("success", "partial", "failed"),
            description = "Execution status"
          ),
          output = list(type = "string", description = "Result text")
        )
      )
    )
  )

  sp_review <- StatePolicy(
    name = "reviewer",
    stage = "review",
    description = "Review the results",
    agent_id = "review_agent",
    priority = 100L,
    final = TRUE,
    accessibility = "explicit",
    depends_on = StateDeps(
      exec_result = list(
        state = "executor",
        field = "result",
        stage = "executing"
      )
    )
  )

  m <- Manifest(
    master = mp,
    states = list(sp_plan, sp_validate, sp_execute, sp_review)
  )

  # --- Round-trip through workflow list ---
  wf_list <- manifest_to_workflow_list(m)
  m2 <- workflow_list_to_manifest(wf_list)

  # Master policy preserved
  expect_equal(m2@master@name, "deps-return-type-test")
  expect_equal(m2@master@version, "2.0.0")
  expect_equal(m2@master@stages, c("planning", "executing", "review"))
  expect_equal(m2@master@parameters$global_timeout, 600)

  # Correct number of states

  expect_length(m2@states, 4)

  # --- Planner: return_type in parameters ---
  planner <- Filter(function(s) s@name == "planner", m2@states)[[1]]
  expect_equal(planner@stage, "planning")
  expect_equal(planner@parameters$system_prompt, "You are a planner.")
  expect_equal(planner@parameters$return_type$type, "object")
  expect_equal(
    planner@parameters$return_type$properties$steps$type, "array"
  )
  expect_equal(
    planner@parameters$return_type$properties$steps$items$type, "string"
  )
  expect_equal(
    planner@parameters$return_type$properties$summary$type, "string"
  )

  # --- Validator: cross-stage depends_on + critical ---
  validator <- Filter(function(s) s@name == "validator", m2@states)[[1]]
  expect_true(validator@critical)
  expect_equal(validator@priority, 900L)
  expect_equal(validator@accessibility, "explicit")
  expect_length(validator@depends_on@deps, 1)
  expect_equal(validator@depends_on@deps$plan_output$state, "planner")
  expect_equal(validator@depends_on@deps$plan_output$field, "result")
  expect_equal(validator@depends_on@deps$plan_output$stage, "planning")

  # --- Executor: multiple depends_on + return_type + on_failure ---
  executor <- Filter(function(s) s@name == "executor", m2@states)[[1]]
  expect_equal(executor@max_retry, 2L)
  expect_equal(executor@on_failure, "validator")
  expect_length(executor@depends_on@deps, 2)
  expect_equal(executor@depends_on@deps$plan_output$state, "planner")
  expect_equal(executor@depends_on@deps$plan_output$stage, "planning")
  expect_equal(executor@depends_on@deps$validation$state, "validator")
  expect_equal(executor@depends_on@deps$validation$field, "description")
  # Same-stage dep has no stage field (NULL after roundtrip)
  expect_null(executor@depends_on@deps$validation$stage)
  # return_type with enum
  expect_equal(
    executor@parameters$return_type$properties$status$type, "enum"
  )
  expect_equal(
    executor@parameters$return_type$properties$status$enum,
    list("success", "partial", "failed")
  )

  # --- Reviewer: cross-stage dep + final ---
  reviewer <- Filter(function(s) s@name == "reviewer", m2@states)[[1]]
  expect_true(reviewer@final)
  expect_equal(reviewer@depends_on@deps$exec_result$state, "executor")
  expect_equal(reviewer@depends_on@deps$exec_result$stage, "executing")
  expect_equal(reviewer@depends_on@deps$exec_result$field, "result")

  # --- Strict structural comparisons ---
  # Parameters must round-trip without spurious class attributes
  expect_identical(
    planner@parameters,
    sp_plan@parameters
  )
  expect_identical(
    executor@parameters,
    sp_execute@parameters
  )
  # depends_on must round-trip identically
  expect_identical(
    as.list(executor@depends_on),
    as.list(sp_execute@depends_on)
  )
  expect_identical(
    as.list(validator@depends_on),
    as.list(sp_validate@depends_on)
  )
})


test_that("manifest roundtrip through YAML preserves depends_on and return_type", {
  tmp <- tempfile(fileext = ".yaml")
  on.exit(unlink(tmp), add = TRUE)

  mp <- MasterPolicy(
    name = "yaml-deps-rt",
    version = "1.0.0",
    stages = c("planning", "executing"),
    parameters = list()
  )

  sp1 <- StatePolicy(
    name = "planner",
    stage = "planning",
    description = "Plan task",
    agent_id = "agent_plan",
    parameters = list(
      return_type = list(
        type = "object",
        properties = list(
          plan = list(type = "string", description = "The plan"),
          confidence = list(type = "number", description = "Confidence score")
        )
      )
    )
  )

  sp2 <- StatePolicy(
    name = "executor",
    stage = "executing",
    description = "Execute task",
    agent_id = "agent_exec",
    accessibility = "explicit",
    depends_on = StateDeps(
      plan_data = list(
        state = "planner", field = "result", stage = "planning"
      )
    ),
    parameters = list(
      return_type = list(
        type = "array",
        items = list(type = "string"),
        description = "List of outputs"
      )
    )
  )

  m <- Manifest(master = mp, states = list(sp1, sp2))

  # Write to YAML and read back
  agents <- list(
    as_agent("base::identity", id = "agent_plan"),
    as_agent("base::identity", id = "agent_exec")
  )

  workflow_save(tmp, manifest = m, agents = agents, append = FALSE)
  wf <- workflow_load(tmp, name = "yaml-deps-rt", scheduler_class = NULL)
  m2 <- wf$manifest

  # return_type on planner survived YAML
  planner <- Filter(function(s) s@name == "planner", m2@states)[[1]]
  expect_equal(planner@parameters$return_type$type, "object")
  expect_equal(
    planner@parameters$return_type$properties$plan$type, "string"
  )
  expect_equal(
    planner@parameters$return_type$properties$confidence$type, "number"
  )

  # depends_on on executor survived YAML
  executor <- Filter(function(s) s@name == "executor", m2@states)[[1]]
  expect_equal(executor@accessibility, "explicit")
  expect_equal(executor@depends_on@deps$plan_data$state, "planner")
  expect_equal(executor@depends_on@deps$plan_data$stage, "planning")
  expect_equal(executor@depends_on@deps$plan_data$field, "result")

  # return_type array on executor survived YAML
  expect_equal(executor@parameters$return_type$type, "array")
  expect_equal(executor@parameters$return_type$items$type, "string")

  # --- Strict structural comparisons ---
  expect_identical(
    planner@parameters,
    sp1@parameters
  )
  expect_identical(
    executor@parameters,
    sp2@parameters
  )
  expect_identical(
    as.list(executor@depends_on),
    as.list(sp2@depends_on)
  )
})


# --- workflow_save / workflow_load / workflow_list ---
test_that("workflow_save creates valid YAML that workflow_load reads back", {
  tmp <- tempfile(fileext = ".yaml")
  on.exit(unlink(tmp), add = TRUE)

  mp <- MasterPolicy(
    name = "save-test",
    version = "1.0.0",
    stages = c("idle", "working"),
    parameters = list(limit = 10)
  )
  sp_idle <- StatePolicy(
    name = "wait",
    stage = "idle",
    description = "Wait for input",
    agent_id = "worker"
  )
  sp <- StatePolicy(
    name = "work",
    stage = "working",
    description = "Do work",
    agent_id = "worker"
  )
  m <- Manifest(master = mp, states = list(sp_idle, sp))

  agents <- list(
    as_agent("base::identity", id = "worker")
  )

  workflow_save(tmp, manifest = m, agents = agents)
  expect_true(file.exists(tmp))

  # List mode
  names <- suppressMessages(workflow_list(tmp))
  expect_equal(names, "save-test")

  # Load mode
  wf <- workflow_load(tmp, name = "save-test", scheduler_class = NULL)
  expect_true(S7::S7_inherits(wf$manifest, Manifest))
  expect_equal(wf$manifest@master@name, "save-test")
  expect_length(wf$agents, 1)
  # Agent should be reconstructed (package_function type)
  expect_true(S7::S7_inherits(wf$agents[[1]], Agent))
})

test_that("workflow_load returns Scheduler instance by default", {
  tmp <- tempfile(fileext = ".yaml")
  on.exit(unlink(tmp), add = TRUE)

  mp <- MasterPolicy(
    name = "scheduler-test", version = "1.0.0",
    stages = "idle"
  )
  sp <- StatePolicy(
    name = "s1", stage = "idle",
    description = "State", agent_id = "a1"
  )
  m <- Manifest(master = mp, states = list(sp))
  agents <- list(
    as_agent("base::identity", id = "a1")
  )
  workflow_save(tmp, manifest = m, agents = agents)

  sched <- workflow_load(tmp, name = "scheduler-test")
  expect_true(inherits(sched, "TricobblerScheduler"))
  expect_true(S7::S7_inherits(sched$manifest, Manifest))
  expect_equal(sched$manifest@master@name, "scheduler-test")
  expect_equal(sched$agents$size(), 1L)
})

test_that("workflow_load returns error for missing workflow", {
  tmp <- tempfile(fileext = ".yaml")
  on.exit(unlink(tmp), add = TRUE)

  mp <- MasterPolicy(
    name = "only-one", version = "1.0.0",
    stages = "idle"
  )
  sp <- StatePolicy(
    name = "s1", stage = "idle",
    description = "State", agent_id = "a1"
  )
  m <- Manifest(master = mp, states = list(sp))
  agents <- list(
    as_agent("base::identity", id = "a1")
  )
  workflow_save(tmp, manifest = m, agents = agents, append = FALSE)

  expect_error(
    workflow_load(tmp, name = "nonexistent"),
    "not found"
  )
})

test_that("workflow_load errors on missing file", {
  expect_error(
    workflow_load("/no/such/file.yaml"),
    "not found"
  )
})


# --- Append / replace semantics ---
test_that("workflow_save append=TRUE merges by name and id", {
  tmp <- tempfile(fileext = ".yaml")
  on.exit(unlink(tmp), add = TRUE)

  # First save
  mp1 <- MasterPolicy(
    name = "wf-alpha", version = "1.0.0",
    stages = "idle"
  )
  sp1 <- StatePolicy(
    name = "s1", stage = "idle",
    description = "Alpha state", agent_id = "a1"
  )
  m1 <- Manifest(master = mp1, states = list(sp1))
  agents1 <- list(
    as_agent("base::cat", id = "a1")
  )
  workflow_save(tmp, manifest = m1, agents = agents1, append = FALSE)

  # Second save -- add new workflow + update existing agent
  mp2 <- MasterPolicy(
    name = "wf-beta", version = "2.0.0",
    stages = "idle"
  )
  sp2 <- StatePolicy(
    name = "s2", stage = "idle",
    description = "Beta state", agent_id = "a1"
  )
  m2 <- Manifest(master = mp2, states = list(sp2))
  agents2 <- list(
    as_agent("base::print", id = "a1")
  )
  workflow_save(tmp, manifest = m2, agents = agents2, append = TRUE)

  # Should have both workflows
  names <- suppressMessages(workflow_list(tmp))
  expect_length(names, 2)
  expect_true("wf-alpha" %in% names)
  expect_true("wf-beta" %in% names)

  # Agent should have been updated (print, not cat)
  raw <- yaml::read_yaml(tmp)
  expect_length(raw$agents, 1)
  expect_equal(raw$agents[[1]]$package_function, "base::print")
})

test_that("workflow_save append=FALSE overwrites", {
  tmp <- tempfile(fileext = ".yaml")
  on.exit(unlink(tmp), add = TRUE)

  # First save
  mp1 <- MasterPolicy(
    name = "old", version = "1.0.0", stages = "idle"
  )
  sp1 <- StatePolicy(
    name = "s", stage = "idle",
    description = "Old", agent_id = "a"
  )
  m1 <- Manifest(master = mp1, states = list(sp1))
  workflow_save(
    tmp, manifest = m1,
    agents = list(
      as_agent("base::identity", id = "a")
    ),
    append = FALSE
  )

  # Second save -- overwrite
  mp2 <- MasterPolicy(
    name = "new", version = "2.0.0", stages = "idle"
  )
  sp2 <- StatePolicy(
    name = "s", stage = "idle",
    description = "New", agent_id = "b"
  )
  m2 <- Manifest(master = mp2, states = list(sp2))
  workflow_save(
    tmp, manifest = m2,
    agents = list(
      as_agent("base::cat", id = "b")
    ),
    append = FALSE
  )

  names <- suppressMessages(workflow_list(tmp))
  expect_length(names, 1)
  expect_equal(names, "new")
})


# --- Script agent reconstruction ---
test_that("reconstruct_agent handles script type", {
  # Create a temp R file with a function
  tmp_r <- tempfile(fileext = ".R")
  on.exit(unlink(tmp_r), add = TRUE)

  writeLines(c(
    "my_agent_fn <- function(x) {",
    "  paste('hello', x)",
    "}"
  ), tmp_r)

  config <- list(
    id = "test_script",
    type = "script",
    source = basename(tmp_r),
    function_name = "my_agent_fn"
  )

  agent <- reconstruct_agent(config, base_dir = dirname(tmp_r))
  expect_true(S7::S7_inherits(agent, Agent))
  expect_equal(agent@id, "test_script")
  expect_equal(agent@config$type, "script")
})

test_that("reconstruct_agent handles package_function type", {
  config <- list(
    id = "pkg_fn",
    type = "package_function",
    package_function = "base::identity"
  )

  agent <- reconstruct_agent(config, base_dir = tempdir())
  expect_true(S7::S7_inherits(agent, Agent))
})


# --- Full YAML roundtrip with stage grouping ---
test_that("full YAML roundtrip preserves stage structure", {
  tmp <- tempfile(fileext = ".yaml")
  on.exit(unlink(tmp), add = TRUE)

  mp <- MasterPolicy(
    name = "full-roundtrip",
    version = "1.0.0",
    stages = c("setup", "run", "cleanup"),
    parameters = list(x = 1, y = "two")
  )
  sp1 <- StatePolicy(
    name = "init", stage = "setup",
    description = "Initialize", agent_id = "a1",
    priority = 100L
  )
  sp2 <- StatePolicy(
    name = "process", stage = "run",
    description = "Process data", agent_id = "a2",
    priority = 200L,
    critical = TRUE,
    depends_on = StateDeps(
      init_result = list(state = "init", field = "result", stage = "setup")
    )
  )
  sp3 <- StatePolicy(
    name = "finalize", stage = "cleanup",
    description = "Clean up", agent_id = "a1",
    priority = 50L
  )
  m <- Manifest(master = mp, states = list(sp1, sp2, sp3))

  agents <- list(
    as_agent("base::identity", id = "a1"),
    as_agent("base::print", id = "a2")
  )

  workflow_save(tmp, manifest = m, agents = agents, append = FALSE)

  # Verify YAML structure has stage grouping
  raw <- yaml::read_yaml(tmp)
  expect_named(raw$workflows[[1]]$stages, c("setup", "run", "cleanup"))

  # Load back
  wf <- workflow_load(tmp, name = "full-roundtrip", scheduler_class = NULL)
  expect_equal(wf$manifest@master@name, "full-roundtrip")
  expect_equal(wf$manifest@master@stages, c("setup", "run", "cleanup"))
  expect_length(wf$manifest@states, 3)

  # Verify dependencies survived
  run_state <- Filter(
    function(s) s@name == "process",
    wf$manifest@states
  )[[1]]
  expect_equal(run_state@depends_on@deps$init_result$state, "init")
  expect_equal(run_state@depends_on@deps$init_result$stage, "setup")
})


# --- Circular read → write → read stability ---
test_that("load → save → load produces identical YAML (circular stability)", {
  tmp1 <- tempfile(fileext = ".yaml")
  tmp2 <- tempfile(fileext = ".yaml")
  on.exit({ unlink(tmp1); unlink(tmp2) }, add = TRUE)

  # --- build and save a workflow ---
  mp <- MasterPolicy(
    name = "circular-test",
    version = "0.9.0",
    stages = c("alpha", "beta"),
    parameters = list(k = 42, label = "test")
  )
  sp1 <- StatePolicy(
    name = "step-a", stage = "alpha",
    description = "First step", agent_id = "agent1",
    priority = 100L, critical = TRUE
  )
  sp2 <- StatePolicy(
    name = "step-b", stage = "beta",
    description = "Second step", agent_id = "agent2",
    priority = 50L,
    depends_on = StateDeps(
      prior = list(state = "step-a", field = "result", stage = "alpha")
    )
  )
  m <- Manifest(master = mp, states = list(sp1, sp2))
  agents <- list(
    as_agent("base::identity", id = "agent1"),
    as_agent("base::print", id = "agent2")
  )

  workflow_save(tmp1, manifest = m, agents = agents, append = FALSE)

  # --- first load ---
  wf1 <- workflow_load(tmp1, name = "circular-test", scheduler_class = NULL)

  # --- re-save using the loaded manifest + agents ---
  workflow_save(tmp2, manifest = wf1$manifest, agents = wf1$agents, append = FALSE)

  # --- second load ---
  wf2 <- workflow_load(tmp2, name = "circular-test", scheduler_class = NULL)

  # YAML files should be identical when parsed
  raw1 <- yaml::read_yaml(tmp1)
  raw2 <- yaml::read_yaml(tmp2)
  expect_equal(raw1, raw2)

  # Manifest fields must match
  expect_equal(wf2$manifest@master@name,    wf1$manifest@master@name)
  expect_equal(wf2$manifest@master@version, wf1$manifest@master@version)
  expect_equal(wf2$manifest@master@stages,  wf1$manifest@master@stages)
  expect_equal(wf2$manifest@master@parameters, wf1$manifest@master@parameters)
  expect_length(wf2$manifest@states, length(wf1$manifest@states))

  # State details
  for (i in seq_along(wf1$manifest@states)) {
    s1 <- wf1$manifest@states[[i]]
    s2 <- wf2$manifest@states[[i]]
    expect_equal(s2@name,        s1@name)
    expect_equal(s2@stage,       s1@stage)
    expect_equal(s2@description, s1@description)
    expect_equal(s2@agent_id,    s1@agent_id)
    expect_equal(s2@priority,    s1@priority)
    expect_equal(s2@critical,    s1@critical)
  }

  # Dependency on step-b must survive
  sb2 <- Filter(function(s) s@name == "step-b", wf2$manifest@states)[[1]]
  expect_equal(sb2@depends_on@deps$prior$state, "step-a")
  expect_equal(sb2@depends_on@deps$prior$stage, "alpha")
  expect_equal(sb2@depends_on@deps$prior$field, "result")

  # Agents must round-trip
  expect_length(wf2$agents, length(wf1$agents))
  ids1 <- vapply(wf1$agents, function(a) a@id, character(1))
  ids2 <- vapply(wf2$agents, function(a) a@id, character(1))
  expect_equal(sort(ids2), sort(ids1))
})

test_that("manifest roundtrip preserves enum return_type with values key", {
  mp <- MasterPolicy(
    name = "enum-rt", version = "1.0.0",
    stages = c("classify"),
    parameters = list()
  )
  sp <- StatePolicy(
    name = "classifier", stage = "classify",
    description = "Classify input", agent_id = "c1",
    parameters = list(
      return_type = list(
        type = "object",
        properties = list(
          label = list(
            type = "enum",
            values = list("positive", "negative", "neutral"),
            description = "Sentiment label"
          ),
          confidence = list(type = "number", description = "Score")
        )
      )
    )
  )
  m <- Manifest(master = mp, states = list(sp))

  wf_list <- manifest_to_workflow_list(m)
  m2 <- workflow_list_to_manifest(wf_list)
  cl <- Filter(function(s) s@name == "classifier", m2@states)[[1]]

  # Strict roundtrip: parameters must be identical
  expect_identical(cl@parameters, sp@parameters)

  # Also verify map_type_to_ellmer can convert the roundtripped return_type
  etype <- map_type_to_ellmer(cl@parameters$return_type)
  expect_s3_class(etype, "ellmer::TypeObject")
})

# --- workflow_load config templating ---
test_that("workflow_load interpolates ${ } templates with config", {
  yaml_text <- '
workflows:
- name: tmpl-test
  version: "1.0.0"
  stages:
    main:
    - name: step1
      description: A step
      agent_id: a1
      resources: {tools: [], skills: []}
      accessibility: all
      parameters: []
      max_retry: 0
      priority: 100
      critical: no
      final: no
      on_failure: .na.character
      append_errors_to_chat: no
      clear_chat_on_pass: no
      depends_on: []
agents:
- id: a1
  type: chat
  provider: ollama
  model: test-model
  base_url: "${ config$ollama$base_url }"
  description: templated agent
'
  tmp <- tempfile(fileext = ".yaml")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(yaml_text, tmp)

  custom_cfg <- list(paths = list(
    workdir = tempdir(),
    ollama = "http://myhost:9999"
  ))

  wf <- workflow_load(tmp, name = "tmpl-test", scheduler_class = NULL,
                      config = custom_cfg)
  expect_type(wf, "list")
  expect_s7_class(wf$manifest, Manifest)
  expect_equal(wf$config, custom_cfg)
})

test_that("workflow_load forwards config to Scheduler constructor", {
  yaml_text <- '
workflows:
- name: cfg-fwd
  version: "1.0.0"
  stages:
    main:
    - name: s1
      description: step
      agent_id: a1
      resources: {tools: [], skills: []}
      accessibility: all
      parameters: []
      max_retry: 0
      priority: 100
      critical: no
      final: no
      on_failure: .na.character
      append_errors_to_chat: no
      clear_chat_on_pass: no
      depends_on: []
agents:
- id: a1
  type: script
  description: dummy
  source: nonexistent.R
  function_name: fn
'
  tmp <- tempfile(fileext = ".yaml")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(yaml_text, tmp)

  custom_cfg <- list(paths = list(
    workdir = tempdir(),
    ollama = "http://custom:1234"
  ))

  sched <- workflow_load(tmp, name = "cfg-fwd",
                         scheduler_class = Scheduler,
                         config = custom_cfg)
  expect_r6_class(sched, "TricobblerScheduler")
  expect_equal(sched$config, custom_cfg)
})

test_that("workflow_load errors on bad template expressions (fail hard)", {
  yaml_text <- '
workflows:
- name: bad-tmpl
  version: "1.0.0"
  stages:
    main:
    - name: s1
      description: step
      agent_id: a1
      resources: {tools: [], skills: []}
      accessibility: all
      parameters: []
      max_retry: 0
      priority: 100
      critical: no
      final: no
      on_failure: .na.character
      append_errors_to_chat: no
      clear_chat_on_pass: no
      depends_on: []
agents:
- id: a1
  type: chat
  provider: ollama
  model: m
  base_url: "${ config$nonexistent_key$deeply$nested }"
  description: bad
'
  tmp <- tempfile(fileext = ".yaml")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(yaml_text, tmp)

  # Should error - no tryCatch, fail hard
  expect_error(
    workflow_load(tmp, name = "bad-tmpl", scheduler_class = NULL,
                  config = list(paths = list(ollama = "http://localhost")))
  )
})

test_that("workflow_load with no templates is a no-op for glue", {
  # Existing YAML without ${ } should load identically
  yaml_text <- '
workflows:
- name: plain
  version: "1.0.0"
  stages:
    main:
    - name: s1
      description: plain step
      agent_id: a1
      resources: {tools: [], skills: []}
      accessibility: all
      parameters: []
      max_retry: 0
      priority: 100
      critical: no
      final: no
      on_failure: .na.character
      append_errors_to_chat: no
      clear_chat_on_pass: no
      depends_on: []
agents:
- id: a1
  type: script
  description: dummy
  source: dummy.R
  function_name: fn
'
  tmp <- tempfile(fileext = ".yaml")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(yaml_text, tmp)

  wf <- workflow_load(tmp, name = "plain", scheduler_class = NULL)
  expect_s7_class(wf$manifest, Manifest)
  expect_equal(wf$manifest@master@name, "plain")
})
