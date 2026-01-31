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
    MasterPolicy(name = "test", version = "1.0.0", stages = character(0)),
    "number of stages cannot be 0"
  )
  
  # Duplicates not allowed
  expect_error(
    MasterPolicy(name = "test", version = "1.0.0", stages = c("idle", "IDLE")),
    "duplicated"
  )
  
  # Invalid characters
  expect_error(
    MasterPolicy(name = "test", version = "1.0.0", stages = c("idle!", "working")),
    "can only contain letters"
  )
})

test_that("StatePolicy validates stage correctly", {
  sp <- StatePolicy(
    name = "state1",
    stage = "idle",
    description = "test state",
    parameters = list()
  )
  expect_true(S7::S7_inherits(sp, StatePolicy))
  expect_equal(sp@stage, "idle")
  
  # Stage cannot be blank
  expect_error(
    StatePolicy(name = "state1", stage = "", description = "test"),
    "non-blank"
  )
  
  # Stage must be single string
  expect_error(
    StatePolicy(name = "state1", stage = c("idle", "working"), description = "test"),
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
  
  sp1 <- StatePolicy(name = "init", stage = "idle", description = "init")
  sp2 <- StatePolicy(name = "work", stage = "working", description = "work")
  
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
  sp3 <- StatePolicy(name = "work2", stage = "working", description = "work again")
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
    parameters = list(wait = TRUE)
  )
  
  sp2 <- StatePolicy(
    name = "process",
    stage = "working",
    description = "Processing state with execution",
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
  
  sp1 <- StatePolicy(name = "s1", stage = "alpha", description = "First state")
  sp2 <- StatePolicy(name = "s2", stage = "beta", description = "Second state")
  sp3 <- StatePolicy(name = "s3", stage = "gamma", description = "Third state")
  
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
      list(name = "s1", stage = "idle", description = "idle state", parameters = list()),
      list(name = "s2", stage = "working", description = "work state", parameters = list())
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
    parameters = list()
  )
  
  original <- Manifest(master = mp, states = list(sp))
  
  tmp <- tempfile(fileext = ".yaml")
  on.exit(unlink(tmp), add = TRUE)
  
  manifest_write(original, tmp)
  reconstructed <- manifest_read(tmp)
  
  # Description should be collapsed (YAML preserves newlines, so it becomes multiline string)
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
  sp1 <- StatePolicy(name = "s1", stage = "idle", priority = 0L)
  expect_equal(sp1@priority, 0L)
  
  sp2 <- StatePolicy(name = "s2", stage = "idle", priority = 999L)
  expect_equal(sp2@priority, 999L)
  
  # Default priority
  sp3 <- StatePolicy(name = "s3", stage = "idle")
  expect_equal(sp3@priority, 100L)
  
  # NA treated as default
  sp4 <- StatePolicy(name = "s4", stage = "idle", priority = NA)
  expect_equal(sp4@priority, 100L)
  
  # NULL treated as default (via setter)
  sp5 <- StatePolicy(name = "s5", stage = "idle", priority = NULL)
  expect_equal(sp5@priority, 100L)
  
  # Invalid priorities
  expect_error(
    StatePolicy(name = "s", stage = "idle", priority = -1L),
    "between 0 and 999"
  )
  
  expect_error(
    StatePolicy(name = "s", stage = "idle", priority = 1000L),
    "between 0 and 999"
  )
})

test_that("Critical states must have priority >= 1", {
  # Valid critical state
  sp <- StatePolicy(name = "s", stage = "idle", priority = 1L, critical = TRUE)
  expect_true(sp@critical)
  expect_equal(sp@priority, 1L)
  
  # High priority critical
  sp2 <- StatePolicy(name = "s2", stage = "idle", priority = 900L, critical = TRUE)
  expect_true(sp2@critical)
  expect_equal(sp2@priority, 900L)
  
  # Invalid: critical with priority 0
  expect_error(
    StatePolicy(name = "s", stage = "idle", priority = 0L, critical = TRUE),
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
  sp1 <- StatePolicy(name = "critical1", stage = "executing", 
                     priority = 900L, critical = TRUE)
  sp2 <- StatePolicy(name = "normal1", stage = "executing", 
                     priority = 100L, critical = FALSE)
  
  mf <- Manifest(master = mp, states = list(sp1, sp2))
  expect_true(S7::S7_inherits(mf, Manifest))
  
  # Invalid: another state in same stage with same priority as critical
  sp3 <- StatePolicy(name = "conflict", stage = "executing", 
                     priority = 900L, critical = FALSE)
  expect_error(
    Manifest(master = mp, states = list(sp1, sp3)),
    "cannot share its priority"
  )
  
  # Valid: critical states with different priorities in same stage
  sp4 <- StatePolicy(name = "critical2", stage = "executing", 
                     priority = 800L, critical = TRUE)
  mf2 <- Manifest(master = mp, states = list(sp1, sp4))
  expect_true(S7::S7_inherits(mf2, Manifest))
  
  # Valid: non-critical states can share priorities with each other
  sp5 <- StatePolicy(name = "normal2", stage = "executing", 
                     priority = 100L, critical = FALSE)
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
  sp_high <- StatePolicy(name = "high_priority", stage = "executing", 
                         priority = 900L, critical = TRUE)
  sp_med <- StatePolicy(name = "medium_priority", stage = "executing", 
                        priority = 500L)
  sp_low <- StatePolicy(name = "low_priority", stage = "executing", 
                        priority = 100L)
  sp_lowest <- StatePolicy(name = "lowest_priority", stage = "executing", 
                           priority = 0L)
  
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
