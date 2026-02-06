# Test MCP context attachment tools against YAML specifications

# Helper function to validate R value against YAML type spec
validate_type_against_spec <- function(value, type_spec, path = "root") {
  spec_type <- type_spec$type %||% "unknown"

  result <- list(valid = TRUE, errors = character())

  # Handle NULL values - acceptable for optional fields
  if (is.null(value)) {
    return(result)
  }

  switch(spec_type,
    "boolean" = {
      if (!is.logical(value) || length(value) != 1) {
        result$valid <- FALSE
        result$errors <- c(result$errors,
          sprintf("%s: expected boolean, got %s", path, class(value)[1]))
      }
    },
    "integer" = {
      if (!is.numeric(value) || length(value) != 1) {
        result$valid <- FALSE
        result$errors <- c(result$errors,
          sprintf("%s: expected integer, got %s", path, class(value)[1]))
      }
    },
    "string" = {
      if (!is.character(value) || length(value) != 1) {
        result$valid <- FALSE
        result$errors <- c(result$errors,
          sprintf("%s: expected string, got %s", path, class(value)[1]))
      }
    },
    "array" = {
      if (!is.list(value) && !is.vector(value) && !is.data.frame(value)) {
        result$valid <- FALSE
        result$errors <- c(result$errors,
          sprintf("%s: expected array, got %s", path, class(value)[1]))
      }
    },
    "object" = {
      if (!is.list(value) && !is.data.frame(value)) {
        result$valid <- FALSE
        result$errors <- c(result$errors,
          sprintf("%s: expected object, got %s", path, class(value)[1]))
      }
    }
  )

  result
}

# Validate response against YAML returns spec - checks types of known properties
validate_response_against_spec <- function(parsed, yaml_spec) {
  errors <- character()

  returns_spec <- yaml_spec$returns
  if (is.null(returns_spec)) {
    return(list(valid = TRUE, errors = errors))
  }

  # Check top-level type
  if (returns_spec$type == "object" && !is.list(parsed)) {
    errors <- c(errors, "Response should be an object (list)")
  }

  # Check each property in the spec
  properties <- returns_spec$properties
  if (!is.null(properties)) {
    for (prop_name in names(properties)) {
      prop_spec <- properties[[prop_name]]
      prop_value <- parsed[[prop_name]]

      # Validate if property exists and has value
      if (!is.null(prop_value)) {
        validation <- validate_type_against_spec(
          prop_value, prop_spec, path = prop_name
        )
        if (!validation$valid) {
          errors <- c(errors, validation$errors)
        }
      }
    }
  }

  list(valid = length(errors) == 0, errors = errors)
}

# Check for extra properties in response that are NOT in the YAML spec
# This catches undocumented properties like "note"
find_extra_properties <- function(parsed, yaml_spec) {
  returns_spec <- yaml_spec$returns
  if (is.null(returns_spec) || is.null(returns_spec$properties)) {
    return(character())
  }

  spec_props <- names(returns_spec$properties)
  response_props <- names(parsed)

  # Find properties in response that are not in spec
  extra_props <- setdiff(response_props, spec_props)
  extra_props
}

# Helper to create a test context with attachments
create_test_context_with_attachment <- function() {
  test_path <- file.path(tempdir(), "tricobbler", "test-mcp-attachments")
  dir.create(test_path, showWarnings = FALSE, recursive = TRUE)
  ctx <- AgentContext$new(path = test_path)
  ctx$init_resources()

  # Record an attachment (no with_activated needed)
  record_dummy_result(ctx,
    result = list(value = 42, name = "test"),
    stage = "teststage",
    state = "teststate",
    agent_id = "testagent",
    current_attempt = 1L,
    description = "Test attachment for MCP tool testing"
  )

  ctx
}

# Helper to create a test runtime for MCP tool testing
# Creates a minimal runtime with a mock agent and policy
create_test_runtime <- function(context, accessibility = "all") {
  # Create a minimal test agent
  test_agent <- Agent(
    function(runtime) "test",
    id = "test_agent",
    description = "Test agent for unit tests"
  )
  # Create a minimal test policy with specified accessibility
  test_policy <- StatePolicy(
    name = "test_policy",
    stage = "test",
    agent_id = "test_agent",
    accessibility = accessibility
  )
  # Create and return runtime
  AgentRuntime$new(
    agent = test_agent,
    context = context,
    policy = test_policy
  )
}


# ===========================================================================
# Tests for mcp_tool_context_attachment_list
# ===========================================================================

test_that(
  "mcp_tool_context_attachment_list returns valid JSON matching YAML spec",
  {
    yaml_spec <- mcptool_read("tricobbler-mcp_tool_context_attachment_list")

    context <- create_test_context_with_attachment()
    on.exit(unlink(context$store_path, recursive = TRUE), add = TRUE)

    # Execute tool with runtime (default accessibility = "all")
    runtime <- create_test_runtime(context)
    result <- mcp_tool_context_attachment_list(.runtime = runtime)

    # Verify result is JSON
    expect_s3_class(result, "json")

    # Parse JSON result
    parsed <- jsonlite::fromJSON(as.character(result))

    # Validate response structure against YAML spec
    validation <- validate_response_against_spec(parsed, yaml_spec)
    expect_true(
      validation$valid,
      info = paste(validation$errors, collapse = "\n")
    )

    # Verify required properties exist
    expect_true(parsed$success)
    expect_type(parsed$context_id, "character")
    expect_type(parsed$count, "integer")
  }
)

test_that(
  "mcp_tool_context_attachment_list includes note when accessibility != 'all'",
  {
    context <- create_test_context_with_attachment()
    on.exit(unlink(context$store_path, recursive = TRUE), add = TRUE)

    # Execute with runtime that has "logs" accessibility
    runtime <- create_test_runtime(context, accessibility = "logs")
    result <- mcp_tool_context_attachment_list(.runtime = runtime)

    parsed <- jsonlite::fromJSON(as.character(result))

    # When accessibility is "logs", should include a note
    expect_true("note" %in% names(parsed))
    expect_type(parsed$note, "character")
    expect_true(grepl("privilege", parsed$note, ignore.case = TRUE))
  }
)

test_that(
  paste(
    "mcp_tool_context_attachment_list note property detected",
    "as extra when not in YAML"
  ),
  {
    yaml_spec <- mcptool_read("tricobbler-mcp_tool_context_attachment_list")

    context <- create_test_context_with_attachment()
    on.exit(unlink(context$store_path, recursive = TRUE), add = TRUE)

    # Execute with runtime that has "logs" accessibility
    runtime <- create_test_runtime(context, accessibility = "logs")
    result <- mcp_tool_context_attachment_list(.runtime = runtime)

    parsed <- jsonlite::fromJSON(as.character(result))

    # Check for extra properties not in YAML spec
    extra_props <- find_extra_properties(parsed, yaml_spec)

    # This test documents the expected mismatch:
    # - If YAML doesn't have "note", this will be non-empty
    # - After running mcptool_build(), this should be empty
    if ("note" %in% names(yaml_spec$returns$properties)) {
      # YAML has been updated - note should not be in extra
      expect_false("note" %in% extra_props)
    } else {
      # YAML hasn't been updated - note IS extra (documents the mismatch)
      expect_true(
        "note" %in% extra_props,
        info = paste(
          "Expected 'note' to be detected as extra property.",
          "Run mcptool_build() to update YAML specs."
        )
      )
    }
  }
)

test_that(
  "mcp_tool_context_attachment_list no note when accessibility is 'all'",
  {
    context <- create_test_context_with_attachment()
    on.exit(unlink(context$store_path, recursive = TRUE), add = TRUE)

    # Execute with runtime that has "all" accessibility
    runtime <- create_test_runtime(context, accessibility = "all")
    result <- mcp_tool_context_attachment_list(.runtime = runtime)

    parsed <- jsonlite::fromJSON(as.character(result))

    # When accessibility is "all", should NOT include a note
    expect_false("note" %in% names(parsed))
    expect_true(parsed$success)
  }
)


# ===========================================================================
# Tests for mcp_tool_context_attachment_exists
# ===========================================================================

test_that(
  "mcp_tool_context_attachment_exists returns valid JSON matching YAML spec",
  {
    yaml_spec <- mcptool_read("tricobbler-mcp_tool_context_attachment_exists")

    context <- create_test_context_with_attachment()
    on.exit(unlink(context$store_path, recursive = TRUE), add = TRUE)

    # Get an attachment ID to test with
    attachments <- context$list_attachments()
    attachment_id <- attachments$attachment_id[1]

    # Execute tool with runtime
    runtime <- create_test_runtime(context)
    result <- mcp_tool_context_attachment_exists(attachment_id = attachment_id, .runtime = runtime)

    # Verify result is JSON
    expect_s3_class(result, "json")

    # Parse JSON result
    parsed <- jsonlite::fromJSON(as.character(result))

    # Validate response structure against YAML spec
    validation <- validate_response_against_spec(parsed, yaml_spec)
    expect_true(
      validation$valid,
      info = paste(validation$errors, collapse = "\n")
    )

    # Verify required properties
    expect_true(parsed$success)
    expect_true(parsed$exists)
    expect_equal(parsed$attachment_id, attachment_id)
  }
)

test_that(
  paste(
    "mcp_tool_context_attachment_exists includes note",
    "when accessibility != 'all'"
  ),
  {
    context <- create_test_context_with_attachment()
    on.exit(unlink(context$store_path, recursive = TRUE), add = TRUE)

    attachments <- context$list_attachments()
    attachment_id <- attachments$attachment_id[1]

    # Execute with runtime that has "logs" accessibility
    runtime <- create_test_runtime(context, accessibility = "logs")
    result <- mcp_tool_context_attachment_exists(attachment_id = attachment_id, .runtime = runtime)

    parsed <- jsonlite::fromJSON(as.character(result))

    # When accessibility is "logs", should include a note
    expect_true("note" %in% names(parsed))
    expect_type(parsed$note, "character")
  }
)

test_that(
  "mcp_tool_context_attachment_exists note detected as extra when not in YAML",
  {
    yaml_spec <- mcptool_read("tricobbler-mcp_tool_context_attachment_exists")

    context <- create_test_context_with_attachment()
    on.exit(unlink(context$store_path, recursive = TRUE), add = TRUE)

    attachments <- context$list_attachments()
    attachment_id <- attachments$attachment_id[1]

    # Execute with runtime that has "logs" accessibility
    runtime <- create_test_runtime(context, accessibility = "logs")
    result <- mcp_tool_context_attachment_exists(attachment_id = attachment_id, .runtime = runtime)

    parsed <- jsonlite::fromJSON(as.character(result))

    # Check for extra properties not in YAML spec
    extra_props <- find_extra_properties(parsed, yaml_spec)

    if ("note" %in% names(yaml_spec$returns$properties)) {
      expect_false("note" %in% extra_props)
    } else {
      expect_true(
        "note" %in% extra_props,
        info = paste(
          "Expected 'note' to be detected as extra property.",
          "Run mcptool_build() to update YAML specs."
        )
      )
    }
  }
)

test_that(
  "mcp_tool_context_attachment_exists no note when accessibility is 'all'",
  {
    context <- create_test_context_with_attachment()
    on.exit(unlink(context$store_path, recursive = TRUE), add = TRUE)

    attachments <- context$list_attachments()
    attachment_id <- attachments$attachment_id[1]

    # Execute with runtime that has "all" accessibility
    runtime <- create_test_runtime(context, accessibility = "all")
    result <- mcp_tool_context_attachment_exists(attachment_id = attachment_id, .runtime = runtime)

    parsed <- jsonlite::fromJSON(as.character(result))

    expect_false("note" %in% names(parsed))
    expect_true(parsed$success)
    expect_true(parsed$exists)
  }
)


# ===========================================================================
# Tests for mcp_tool_context_attachment_get
# ===========================================================================

test_that(
  "mcp_tool_context_attachment_get returns error when accessibility != 'all'",
  {
    context <- create_test_context_with_attachment()
    on.exit(unlink(context$store_path, recursive = TRUE), add = TRUE)

    attachments <- context$list_attachments()
    attachment_id <- attachments$attachment_id[1]

    # Execute with runtime that has "logs" accessibility
    runtime <- create_test_runtime(context, accessibility = "logs")
    result <- mcp_tool_context_attachment_get(attachment_id = attachment_id, .runtime = runtime)

    parsed <- jsonlite::fromJSON(as.character(result))

    # Should fail because accessibility is "logs", not "all"
    expect_false(parsed$success)
    expect_true("error" %in% names(parsed))
    expect_true(grepl("accessibility", parsed$error, ignore.case = TRUE))
  }
)

test_that(
  "mcp_tool_context_attachment_get succeeds with 'all' accessibility",
  {
    yaml_spec <- mcptool_read("tricobbler-mcp_tool_context_attachment_get")

    context <- create_test_context_with_attachment()
    on.exit(unlink(context$store_path, recursive = TRUE), add = TRUE)

    attachments <- context$list_attachments()
    attachment_id <- attachments$attachment_id[1]

    # Execute with runtime that has "all" accessibility
    runtime <- create_test_runtime(context, accessibility = "all")
    result <- mcp_tool_context_attachment_get(attachment_id = attachment_id, .runtime = runtime)

    expect_s3_class(result, "json")

    parsed <- jsonlite::fromJSON(as.character(result))

    # Validate response structure against YAML spec
    validation <- validate_response_against_spec(parsed, yaml_spec)
    expect_true(
      validation$valid,
      info = paste(validation$errors, collapse = "\n")
    )

    expect_true(parsed$success)
    expect_equal(parsed$attachment_id, attachment_id)
    expect_true("attachment" %in% names(parsed))
  }
)

test_that(
  "mcp_tool_context_attachment_get returns error with 'none' accessibility",
  {
    context <- create_test_context_with_attachment()
    on.exit(unlink(context$store_path, recursive = TRUE), add = TRUE)

    attachments <- context$list_attachments()
    attachment_id <- attachments$attachment_id[1]

    # Execute with runtime that has "none" accessibility
    runtime <- create_test_runtime(context, accessibility = "none")
    result <- mcp_tool_context_attachment_get(attachment_id = attachment_id, .runtime = runtime)

    parsed <- jsonlite::fromJSON(as.character(result))

    expect_false(parsed$success)
    expect_true("error" %in% names(parsed))
    expect_true(grepl("none", parsed$error, ignore.case = TRUE))
  }
)


# ===========================================================================
# Tests for accessibility on log tools (they should also check accessibility)
# ===========================================================================

test_that(
  "mcp_tool_context_logs_head returns error with 'none' accessibility",
  {
    context <- create_test_context_with_attachment()
    on.exit(unlink(context$store_path, recursive = TRUE), add = TRUE)

    # Add a log entry (no with_activated needed)
    context$logger("Test log entry", caller = context, level = "INFO",
                   verbose = "none")

    # Execute with runtime that has "none" accessibility
    runtime <- create_test_runtime(context, accessibility = "none")
    result <- mcp_tool_context_logs_head(.runtime = runtime)

    parsed <- jsonlite::fromJSON(as.character(result))

    expect_false(parsed$success)
    expect_true("error" %in% names(parsed))
    expect_true(grepl("none", parsed$error, ignore.case = TRUE))
  }
)

test_that(
  "mcp_tool_context_logs_tail returns error with 'none' accessibility",
  {
    context <- create_test_context_with_attachment()
    on.exit(unlink(context$store_path, recursive = TRUE), add = TRUE)

    # Add a log entry (no with_activated needed)
    context$logger("Test log entry", caller = context, level = "INFO",
                   verbose = "none")

    # Execute with runtime that has "none" accessibility
    runtime <- create_test_runtime(context, accessibility = "none")
    result <- mcp_tool_context_logs_tail(.runtime = runtime)

    parsed <- jsonlite::fromJSON(as.character(result))

    expect_false(parsed$success)
    expect_true(grepl("none", parsed$error, ignore.case = TRUE))
  }
)

test_that(
  "mcp_tool_context_logs_search returns error with 'none' accessibility",
  {
    context <- create_test_context_with_attachment()
    on.exit(unlink(context$store_path, recursive = TRUE), add = TRUE)

    # Add a log entry (no with_activated needed)
    context$logger("Test log entry", caller = context, level = "INFO",
                   verbose = "none")

    # Execute with runtime that has "none" accessibility
    runtime <- create_test_runtime(context, accessibility = "none")
    result <- mcp_tool_context_logs_search(pattern = "test", .runtime = runtime)

    parsed <- jsonlite::fromJSON(as.character(result))

    expect_false(parsed$success)
    expect_true(grepl("none", parsed$error, ignore.case = TRUE))
  }
)


# ===========================================================================
# Explicit tests for YAML spec completeness (will fail until mcptool_build)
# ===========================================================================

test_that(
  "YAML spec for attachment_list should document 'note' property",
  {
    yaml_spec <- mcptool_read("tricobbler-mcp_tool_context_attachment_list")

    # This test explicitly checks if YAML has the note property
    has_note_in_spec <- "note" %in% names(yaml_spec$returns$properties)

    # After mcptool_build() this should pass
    # Before mcptool_build() this will fail, documenting the gap
    expect_true(
      has_note_in_spec,
      info = paste(
        "YAML spec is missing 'note' property.",
        "Run mcptool_build() to regenerate YAML from roxygen2 documentation."
      )
    )
  }
)

test_that(
  "YAML spec for attachment_exists should document 'note' property",
  {
    yaml_spec <- mcptool_read("tricobbler-mcp_tool_context_attachment_exists")

    has_note_in_spec <- "note" %in% names(yaml_spec$returns$properties)

    expect_true(
      has_note_in_spec,
      info = paste(
        "YAML spec is missing 'note' property.",
        "Run mcptool_build() to regenerate YAML from roxygen2 documentation."
      )
    )
  }
)
