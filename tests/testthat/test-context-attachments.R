# Test AgentContext attachment methods: has_attachment, get_attachment

# Helper to create a context with its directory
create_test_context <- function() {

  test_path <- file.path(tempdir(), "tricobbler", "context")
  dir.create(test_path, showWarnings = FALSE, recursive = TRUE)
  ctx <- AgentContext$new(path = test_path)
  ctx$init_resources()
  ctx
}

test_that("has_attachment returns FALSE for invalid inputs", {
  ctx <- create_test_context()


  # NULL, NA, empty string

  expect_false(ctx$has_attachment(NULL))
  expect_false(ctx$has_attachment(NA_character_))
  expect_false(ctx$has_attachment(""))

  # Wrong types
  expect_false(ctx$has_attachment(123))
  expect_false(ctx$has_attachment(TRUE))

  # Multiple values

  expect_false(ctx$has_attachment(c("a", "b")))
})

test_that("has_attachment returns FALSE for malformed identifiers", {
  ctx <- create_test_context()

  # Random strings
  expect_false(ctx$has_attachment("random_string"))
  expect_false(ctx$has_attachment("not_valid_format"))

  # Missing brackets
  expect_false(ctx$has_attachment("stage_state_agent_123456T123456_1"))

  # Missing agent bracket
  expect_false(ctx$has_attachment("[stage][state]_123456T123456_1"))

  # Wrong date format (5 digits instead of 6)
  expect_false(ctx$has_attachment("[stage][state][agent]_12345T123456_1"))

  # Wrong time format
  expect_false(ctx$has_attachment("[stage][state][agent]_123456T12345_1"))

  # Missing attempt number
  expect_false(ctx$has_attachment("[stage][state][agent]_123456T123456"))
})

test_that("has_attachment returns FALSE for valid format but non-existent", {
  ctx <- create_test_context()

  valid_but_missing <- "[test][state][agent123]_260101T120000_1"
  expect_false(ctx$has_attachment(valid_but_missing))
})

test_that("has_attachment returns TRUE for existing attachments", {
  ctx <- create_test_context()

  # Create a real attachment
  record_dummy_result(ctx,
    result = list(value = 42),
    stage = "teststage",
    state = "teststate",
    agent_id = "testagent",
    current_attempt = 1L,
    description = "Test result"
  )

  # Get the attachment ID
  attachments <- ctx$list_attachments()
  expect_true(is.data.frame(attachments))
  expect_gt(nrow(attachments), 0)

  attachment_id <- attachments$filename[1]
  expect_true(ctx$has_attachment(attachment_id))
})

test_that("get_attachment errors on missing argument", {
  ctx <- create_test_context()

  expect_error(
    ctx$get_attachment(),
    "`attachment_id` is required"
  )
})

test_that("get_attachment errors on invalid inputs", {
  ctx <- create_test_context()

  expect_error(
    ctx$get_attachment(NULL),
    "must be a single non-empty character string"
  )
  expect_error(
    ctx$get_attachment(NA_character_),
    "must be a single non-empty character string"
  )
  expect_error(
    ctx$get_attachment(""),
    "must be a single non-empty character string"
  )
  expect_error(
    ctx$get_attachment(123),
    "must be a single non-empty character string"
  )
  expect_error(
    ctx$get_attachment(c("a", "b")),
    "must be a single non-empty character string"
  )
})

test_that("get_attachment errors on malformed identifiers", {
  ctx <- create_test_context()

  expect_error(
    ctx$get_attachment("random_string"),
    "Invalid identifier format"
  )
  expect_error(
    ctx$get_attachment("[stage][state]_123456T123456_1"),
    "Invalid identifier format"
  )
})

test_that("get_attachment errors on non-existent but valid format", {
  ctx <- create_test_context()

  valid_but_missing <- "[test][state][agent123]_260101T120000_1"
  expect_error(
    ctx$get_attachment(valid_but_missing),
    "is not registered"
  )
})

test_that("get_attachment retrieves existing attachments", {
  ctx <- create_test_context()

  # Create a real attachment
  test_result <- list(value = 42, name = "test")
  record_dummy_result(ctx,
    result = test_result,
    stage = "mystage",
    state = "mystate",
    agent_id = "myagent",
    current_attempt = 1L,
    description = "My test result"
  )

  # Get the attachment ID
  attachments <- ctx$list_attachments()
  attachment_id <- attachments$filename[1]

  # Retrieve and verify
  retrieved <- ctx$get_attachment(attachment_id)
  expect_type(retrieved, "list")
  expect_equal(retrieved$result, test_result)
  expect_equal(retrieved$stage, "mystage")
  expect_equal(retrieved$state, "mystate")
  expect_equal(retrieved$agent_id, "myagent")
  expect_equal(retrieved$current_attempt, 1L)
  expect_equal(retrieved$description, "My test result")
})

test_that("attachment regex accepts valid identifier formats", {
  ctx <- create_test_context()

  # Create attachments with various valid characters
  record_dummy_result(ctx,
    result = "test1",
    stage = "stage-with-dash",
    state = "state_with_underscore",
    agent_id = "agent_with_underscore",
    current_attempt = 1L,
    description = "Test 1"
  )

  record_dummy_result(ctx,
    result = "test2",
    stage = "UPPERCASE",
    state = "MixedCase",
    agent_id = "agent123",
    current_attempt = 99L,
    description = "Test 2"
  )

  attachments <- ctx$list_attachments()
  expect_equal(nrow(attachments), 2)

  # Both should be retrievable
  for (i in seq_len(nrow(attachments))) {
    expect_true(ctx$has_attachment(attachments$filename[i]))
    retrieved <- ctx$get_attachment(attachments$filename[i])
    expect_type(retrieved, "list")
  }
})
