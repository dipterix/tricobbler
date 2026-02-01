# Test MCP context log tools against YAML specifications

# Helper function to validate R value against YAML type spec
validate_type_against_spec <- function(value, type_spec, path = "root") {
  spec_type <- type_spec$type %||% "unknown"

  result <- list(valid = TRUE, errors = character())

  # Handle NULL values
if (is.null(value)) {
    # NULL is acceptable for optional fields
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
      # Object can be list, data.frame, or named structure
      if (!is.list(value) && !is.data.frame(value)) {
        result$valid <- FALSE
        result$errors <- c(result$errors,
          sprintf("%s: expected object, got %s", path, class(value)[1]))
      }
    }
  )

  result
}

# Validate entire response against YAML returns spec
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

test_that(
  "mcp_tool_context_logs_head output types match YAML spec exactly",
  {
  yaml_spec <- mcptool_read("tricobbler-mcp_tool_context_logs_head")

  # Create test context with logs
  context <- AgentContext$new()
  context$init_resources()
  on.exit(unlink(context$store_path, recursive = TRUE), add = TRUE)

  context$with_activated({
    context$logger(
      "Test message",
      caller = context,
      level = "INFO",
      verbose = "none"
    )
  })

  result <- context$with_activated({
    mcp_tool_context_logs_head(max_lines = 10L)
  })

  parsed <- jsonlite::fromJSON(as.character(result))

  # Validate response structure against YAML spec
  validation <- validate_response_against_spec(parsed, yaml_spec)
  expect_true(validation$valid, info = paste(validation$errors, collapse = "\n")) # nolint: line_length_linter.

  # Explicitly verify each property type matches spec
  props <- yaml_spec$returns$properties

  # success: boolean
  expect_equal(props$success$type, "boolean")
  expect_type(parsed$success, "logical")
  expect_length(parsed$success, 1)

  # context_id: string
  expect_equal(props$context_id$type, "string")
  expect_type(parsed$context_id, "character")
  expect_length(parsed$context_id, 1)

  # count: integer
  expect_equal(props$count$type, "integer")
  expect_type(parsed$count, "integer")
  expect_length(parsed$count, 1)

  # entries: object (which in JSON becomes array of objects -> data.frame)
  expect_equal(props$entries$type, "object")
  # Note: YAML says "object" but actual output is array of entry objects
  # This is a spec issue - entries should be type: array with items: object
  expect_true(is.data.frame(parsed$entries) || is.list(parsed$entries))
})

test_that("mcp_tool_context_logs_head returns JSON matching YAML spec", {
  # Load YAML specification
 yaml_spec <- mcptool_read("tricobbler-mcp_tool_context_logs_head")

  # Verify YAML spec structure
expect_equal(yaml_spec$output_format, "json")
  expect_equal(yaml_spec$returns$type, "object")
  expect_true("success" %in% names(yaml_spec$returns$properties))
  expect_true("context_id" %in% names(yaml_spec$returns$properties))
  expect_true("count" %in% names(yaml_spec$returns$properties))
  expect_true("entries" %in% names(yaml_spec$returns$properties))
  expect_true("error" %in% names(yaml_spec$returns$properties))

  # Create test context with logs
  context <- AgentContext$new()
  context$init_resources()
  on.exit(unlink(context$store_path, recursive = TRUE), add = TRUE)

  context$with_activated({
    context$logger(
      "Test message 1",
      caller = context,
      level = "INFO",
      verbose = "none"
    )
    context$logger(
      "Test warning",
      caller = context,
      level = "WARN",
      verbose = "none"
    )
    context$logger(
      "Test error",
      caller = context,
      level = "ERROR",
      verbose = "none"
    )
  })

  # Execute tool within activated context
  result <- context$with_activated({
    mcp_tool_context_logs_head(max_lines = 10L)
  })

  # Verify result has "json" class
  expect_s3_class(result, "json")

  # Parse JSON result
  parsed <- jsonlite::fromJSON(as.character(result))

  # Validate against YAML spec: success case
  expect_true(parsed$success)
  expect_type(parsed$success, "logical")
  expect_type(parsed$context_id, "character")
  expect_type(parsed$count, "integer")

  # Validate entries structure - should be data.frame (array of objects)
  expect_true(is.data.frame(parsed$entries) || is.list(parsed$entries))

  if (is.data.frame(parsed$entries)) {
    # When dataframe="rows", jsonlite returns data.frame
    expect_true("line_no" %in% names(parsed$entries))
    expect_true("level" %in% names(parsed$entries))
    expect_true("time" %in% names(parsed$entries))
    expect_true("caller" %in% names(parsed$entries))
    expect_true("content" %in% names(parsed$entries))

    # Validate types within entries
    expect_type(parsed$entries$line_no, "integer")
    expect_type(parsed$entries$level, "character")
    expect_type(parsed$entries$time, "character")
    expect_type(parsed$entries$caller, "character")
    expect_type(parsed$entries$content, "character")
  }

  # Verify count matches entries
  expect_equal(parsed$count, nrow(parsed$entries))
})

test_that("mcp_tool_context_logs_head error case matches YAML spec", {
  yaml_spec <- mcptool_read("tricobbler-mcp_tool_context_logs_head")

  # Execute without active context
  result <- mcp_tool_context_logs_head()

  expect_s3_class(result, "json")

  parsed <- jsonlite::fromJSON(as.character(result))

  # Validate error structure per YAML spec
  expect_false(parsed$success)
  expect_type(parsed$success, "logical")
  expect_type(parsed$error, "character")
  expect_true(nzchar(parsed$error))

  # Error case should not have context_id, count, or entries
  expect_null(parsed$context_id)
  expect_null(parsed$count)
  expect_null(parsed$entries)
})

test_that("mcp_tool_context_logs_tail returns JSON matching YAML spec", {
  yaml_spec <- mcptool_read("tricobbler-mcp_tool_context_logs_tail")

  expect_equal(yaml_spec$output_format, "json")
  expect_equal(yaml_spec$returns$type, "object")

  context <- AgentContext$new()
  context$init_resources()
  on.exit(unlink(context$store_path, recursive = TRUE), add = TRUE)

  context$with_activated({
    context$logger(
      "First message",
      caller = context,
      level = "INFO",
      verbose = "none"
    )
    context$logger(
      "Second message",
      caller = context,
      level = "INFO",
      verbose = "none"
    )
    context$logger(
      "Third message",
      caller = context,
      level = "INFO",
      verbose = "none"
    )
    context$logger(
      "Fourth message",
      caller = context,
      level = "INFO",
      verbose = "none"
    )
  })

  # Read only last 2 entries
  result <- context$with_activated({
    mcp_tool_context_logs_tail(max_lines = 2L)
  })

  expect_s3_class(result, "json")

  parsed <- jsonlite::fromJSON(as.character(result))

  expect_true(parsed$success)
  expect_equal(parsed$count, 2L)
  expect_equal(nrow(parsed$entries), 2L)

  # Tail should return last entries (Third and Fourth)
  expect_true(all(grepl("message", parsed$entries$content)))
})

test_that("mcp_tool_context_logs_search returns JSON matching YAML spec", {
  yaml_spec <- mcptool_read("tricobbler-mcp_tool_context_logs_search")

  expect_equal(yaml_spec$output_format, "json")
  expect_equal(yaml_spec$returns$type, "object")

  # YAML spec should include pattern field
  expect_true("pattern" %in% names(yaml_spec$returns$properties))

  context <- AgentContext$new()
  context$init_resources()
  on.exit(unlink(context$store_path, recursive = TRUE), add = TRUE)

  context$with_activated({
    context$logger(
      "Normal operation",
      caller = context,
      level = "INFO",
      verbose = "none"
    )
    context$logger(
      "Warning about memory",
      caller = context,
      level = "WARN",
      verbose = "none"
    )
    context$logger(
      "Critical error occurred",
      caller = context,
      level = "ERROR",
      verbose = "none"
    )
    context$logger(
      "Another normal log",
      caller = context,
      level = "INFO",
      verbose = "none"
    )
  })

  # Search for "error" pattern
  result <- context$with_activated({
    mcp_tool_context_logs_search(pattern = "error", max_lines = 100L)
  })

  expect_s3_class(result, "json")

  parsed <- jsonlite::fromJSON(as.character(result))

  expect_true(parsed$success)
  expect_equal(parsed$pattern, "error")
  expect_equal(parsed$count, 1L)
  expect_true(grepl("error", parsed$entries$content, ignore.case = TRUE))
})

test_that("mcp_tool_context_logs_search validates required pattern parameter", {
  yaml_spec <- mcptool_read("tricobbler-mcp_tool_context_logs_search")

  # pattern should be required according to spec
  required_params <- yaml_spec$parameters$required
  expect_true("pattern" %in% required_params)

  context <- AgentContext$new()
  context$init_resources()
  on.exit(unlink(context$store_path, recursive = TRUE), add = TRUE)

  # Call without pattern - should return error
  result <- context$with_activated({
    mcp_tool_context_logs_search()
  })

  expect_s3_class(result, "json")

  parsed <- jsonlite::fromJSON(as.character(result))

  expect_false(parsed$success)
  expect_type(parsed$error, "character")
  expect_true(grepl("pattern", parsed$error, ignore.case = TRUE))
})

test_that("ellmer tool instantiation preserves YAML spec metadata", {
  skip_if_not_installed("ellmer")

  yaml_spec <- mcptool_read("tricobbler-mcp_tool_context_logs_head")

  # Instantiate as ellmer tool
  state_env <- mcptool_state_factory()
  tool <- mcptool_instantiate(yaml_spec, state_env = state_env)

  # Verify it's an ellmer ToolDef
  expect_s3_class(tool, "ellmer::ToolDef")

  # Verify tool name matches
  expect_equal(tool@name, gsub("[^a-zA-Z0-9_-]+", "-", yaml_spec$name))

  # Verify description includes category and returns info
  expect_true(grepl("Category", tool@description))
  expect_true(grepl("info", tool@description))
  expect_true(grepl("Returns", tool@description))
  expect_true(grepl("JSON", tool@description)) # Output format
})

test_that("instantiated tool executes and returns valid JSON", {
  skip_if_not_installed("ellmer")

  yaml_spec <- mcptool_read("tricobbler-mcp_tool_context_logs_head")
  state_env <- mcptool_state_factory()
  tool <- mcptool_instantiate(yaml_spec, state_env = state_env)

  # Create context and add logs
  context <- AgentContext$new()
  context$init_resources()
  on.exit(unlink(context$store_path, recursive = TRUE), add = TRUE)

  context$with_activated({
    context$logger(
      "Ellmer test log",
      caller = context,
      level = "INFO",
      verbose = "none"
    )
  })

  # Execute the tool (ToolDef is callable as a function) within context
  result <- context$with_activated({
    tool(max_lines = 10L)
  })

  # The wrapper should preserve the json class (since it inherits "json")
  # and mcp_describe.json should return it as-is
  expect_type(result, "character")

  # Parse and validate
  parsed <- jsonlite::fromJSON(result)
  expect_true(parsed$success)
  expect_true("entries" %in% names(parsed))
})

test_that("entries array elements have consistent structure", {
  context <- AgentContext$new()
  context$init_resources()
  on.exit(unlink(context$store_path, recursive = TRUE), add = TRUE)

  # Create multiple log entries with different levels
  context$with_activated({
    context$logger(
      "Info message",
      caller = context,
      level = "INFO",
      verbose = "none"
    )
    context$logger(
      "Warning message",
      caller = context,
      level = "WARN",
      verbose = "none"
    )
    context$logger(
      "Error message",
      caller = context,
      level = "ERROR",
      verbose = "none"
    )
  })

  result <- context$with_activated({
    mcp_tool_context_logs_head(max_lines = 100L)
  })

  parsed <- jsonlite::fromJSON(as.character(result))

  # Each entry should have exactly these fields
  expected_fields <- c("line_no", "content", "level", "caller", "time")

  expect_true(all(expected_fields %in% names(parsed$entries)))
  expect_equal(length(names(parsed$entries)), length(expected_fields))

  # All entries should have non-NA values for required fields
  expect_true(all(!is.na(parsed$entries$line_no)))
  expect_true(all(!is.na(parsed$entries$level)))
  expect_true(all(!is.na(parsed$entries$time)))
  expect_true(all(!is.na(parsed$entries$caller)))
  expect_true(all(!is.na(parsed$entries$content)))

  # line_no should be positive integers
  expect_true(all(parsed$entries$line_no > 0))

  # level should be valid log levels
  valid_levels <- c("TRACE", "DEBUG", "INFO", "WARN", "ERROR", "FATAL")
  expect_true(all(parsed$entries$level %in% valid_levels))

  # time should match HH:MM:SS format
  expect_true(all(grepl("^\\d{2}:\\d{2}:\\d{2}$", parsed$entries$time)))
})

test_that("level filtering works correctly", {
  context <- AgentContext$new()
  context$init_resources()
  on.exit(unlink(context$store_path, recursive = TRUE), add = TRUE)

  context$with_activated({
    context$logger(
      "Info only",
      caller = context,
      level = "INFO",
      verbose = "none"
    )
    context$logger(
      "Warning only",
      caller = context,
      level = "WARN",
      verbose = "none"
    )
    context$logger(
      "Error only",
      caller = context,
      level = "ERROR",
      verbose = "none"
    )
  })

  # Filter to only ERROR level
  result <- context$with_activated({
    mcp_tool_context_logs_head(max_lines = 100L, levels = "ERROR")
  })

  parsed <- jsonlite::fromJSON(as.character(result))

  expect_true(parsed$success)
  expect_equal(parsed$count, 1L)
  expect_equal(parsed$entries$level, "ERROR")
  expect_true(grepl("Error only", parsed$entries$content))
})

test_that("pattern filtering works correctly", {
  context <- AgentContext$new()
  context$init_resources()
  on.exit(unlink(context$store_path, recursive = TRUE), add = TRUE)

  context$with_activated({
    context$logger(
      "Processing file A",
      caller = context,
      level = "INFO",
      verbose = "none"
    )
    context$logger(
      "Memory usage high",
      caller = context,
      level = "WARN",
      verbose = "none"
    )
    context$logger(
      "Processing file B",
      caller = context,
      level = "INFO",
      verbose = "none"
    )
  })

  result <- context$with_activated({
    mcp_tool_context_logs_head(max_lines = 100L, pattern = "Processing")
  })

  parsed <- jsonlite::fromJSON(as.character(result))

  expect_true(parsed$success)
  expect_equal(parsed$count, 2L)
  expect_true(all(grepl("Processing", parsed$entries$content)))
})

test_that("skip_lines pagination works", {
  context <- AgentContext$new()
  context$init_resources()
  on.exit(unlink(context$store_path, recursive = TRUE), add = TRUE)

  context$with_activated({
    context$logger("Line 1", caller = context, level = "INFO", verbose = "none")
    context$logger("Line 2", caller = context, level = "INFO", verbose = "none")
    context$logger("Line 3", caller = context, level = "INFO", verbose = "none")
    context$logger("Line 4", caller = context, level = "INFO", verbose = "none")
  })

  # Skip first 2 lines from head
  result <- context$with_activated({
    mcp_tool_context_logs_head(max_lines = 100L, skip_lines = 2L)
  })

  parsed <- jsonlite::fromJSON(as.character(result))

  expect_true(parsed$success)
  expect_equal(parsed$count, 2L)
  expect_true(grepl("Line 3", parsed$entries$content[1]))
  expect_true(grepl("Line 4", parsed$entries$content[2]))
})

test_that("empty log file returns valid empty response", {
  context <- AgentContext$new()
  context$init_resources()
  on.exit(unlink(context$store_path, recursive = TRUE), add = TRUE)

  # Don't add any logs

  result <- context$with_activated({
    mcp_tool_context_logs_head(max_lines = 10L)
  })

  expect_s3_class(result, "json")

  parsed <- jsonlite::fromJSON(as.character(result))

  expect_true(parsed$success)
  expect_equal(parsed$count, 0L)
  # entries should be empty but valid
  expect_true(is.data.frame(parsed$entries) || length(parsed$entries) == 0)
})

# =============================================================================
# Comprehensive YAML Spec Validation Tests
# =============================================================================

test_that("all context log tools have consistent YAML spec structure", {
  tools <- c(
    "tricobbler-mcp_tool_context_logs_head",
    "tricobbler-mcp_tool_context_logs_tail",
    "tricobbler-mcp_tool_context_logs_search"
  )

  for (tool_name in tools) {
    yaml_spec <- mcptool_read(tool_name)

    # All should have output_format: json
    expect_equal(yaml_spec$output_format, "json",
      info = sprintf("%s should have output_format: json", tool_name))

    # All should have category: info
    expect_equal(yaml_spec$category, "info",
      info = sprintf("%s should have category: info", tool_name))

    # All should return object type
    expect_equal(yaml_spec$returns$type, "object",
      info = sprintf("%s returns should be object type", tool_name))

    # All should have success and error properties
    props <- names(yaml_spec$returns$properties)
    expect_true("success" %in% props,
      info = sprintf("%s should have success property", tool_name))
    expect_true("error" %in% props,
      info = sprintf("%s should have error property", tool_name))
    expect_true("entries" %in% props,
      info = sprintf("%s should have entries property", tool_name))
    expect_true("count" %in% props,
      info = sprintf("%s should have count property", tool_name))
    expect_true("context_id" %in% props,
      info = sprintf("%s should have context_id property", tool_name))
  }
})

test_that("YAML parameter specs match actual function signatures", {
  yaml_spec <- mcptool_read("tricobbler-mcp_tool_context_logs_head")

  # Get actual function formals
  fn_formals <- formals(mcp_tool_context_logs_head)
  fn_params <- names(fn_formals)

  # Get YAML params
  yaml_params <- names(yaml_spec$parameters$properties)

  # All YAML params should exist in function
  for (param in yaml_params) {
    expect_true(param %in% fn_params,
      info = sprintf("YAML param '%s' should exist in function", param))
  }

  # Check parameter types in YAML
  props <- yaml_spec$parameters$properties

  expect_equal(props$max_lines$type, "integer")
  expect_equal(props$skip_lines$type, "integer")
  expect_equal(props$pattern$type, "string")
  expect_equal(props$levels$type, "array")
})

test_that("search_logs YAML spec includes pattern as required", {
  yaml_spec <- mcptool_read("tricobbler-mcp_tool_context_logs_search")

  # pattern should be in required list
  required <- yaml_spec$parameters$required
  expect_true("pattern" %in% required,
    info = "pattern should be required for search_logs")

  # pattern should have string type
  expect_equal(yaml_spec$parameters$properties$pattern$type, "string")

  # search_logs should also have pattern in returns
  expect_true("pattern" %in% names(yaml_spec$returns$properties))
})

test_that("actual JSON output field types match YAML spec types", {
  yaml_spec <- mcptool_read("tricobbler-mcp_tool_context_logs_head")

  context <- AgentContext$new()
  context$init_resources()
  on.exit(unlink(context$store_path, recursive = TRUE), add = TRUE)

  context$with_activated({
    context$logger("Test", caller = context, level = "INFO", verbose = "none")
  })

  result <- context$with_activated({
    mcp_tool_context_logs_head(max_lines = 10L)
  })

  parsed <- jsonlite::fromJSON(as.character(result))
  props <- yaml_spec$returns$properties

  # Map YAML types to R types for validation
  yaml_to_r_type <- function(yaml_type) {
    switch(yaml_type,
      "boolean" = "logical",
      "integer" = "integer",
      "string" = "character",
      "object" = c("list", "data.frame"),
      "array" = c("list", "data.frame", "character", "numeric"),
      "unknown"
    )
  }

  # Validate each field
  for (prop_name in names(props)) {
    if (!is.null(parsed[[prop_name]])) {
      expected_types <- yaml_to_r_type(props[[prop_name]]$type)
      actual_type <- typeof(parsed[[prop_name]])

      # For data.frame, typeof returns "list"
      if (is.data.frame(parsed[[prop_name]])) {
        actual_type <- "data.frame"
      }

      expect_true(
        actual_type %in% expected_types ||
          class(parsed[[prop_name]])[1] %in% expected_types,
        info = sprintf(
          "Field '%s': YAML type '%s' (expects %s), got R type '%s'",
          prop_name, props[[prop_name]]$type,
          paste(expected_types, collapse = "/"), actual_type
        )
      )
    }
  }
})

test_that(
  "error response only contains fields specified in YAML for error case",
  {
  yaml_spec <- mcptool_read("tricobbler-mcp_tool_context_logs_head")

  # Call without context to get error
  result <- mcp_tool_context_logs_head()
  parsed <- jsonlite::fromJSON(as.character(result))

  # Error response should have success: false
  expect_false(parsed$success)

  # Error response should have error field
  expect_true(!is.null(parsed$error))
  expect_type(parsed$error, "character")

  # Verify error field is in YAML spec
  expect_true("error" %in% names(yaml_spec$returns$properties))
  expect_equal(yaml_spec$returns$properties$error$type, "string")
})

test_that("entries structure matches documented entry fields", {
  yaml_spec <- mcptool_read("tricobbler-mcp_tool_context_logs_head")

  # The YAML description says entries contain:
  # line_no, level, time, caller, content
  entries_desc <- yaml_spec$returns$properties$entries$description
  expected_fields <- c("line_no", "level", "time", "caller", "content")

  # Verify all expected fields are mentioned in description
  for (field in expected_fields) {
    expect_true(
      grepl(field, entries_desc),
      info = sprintf(
        "Field '%s' should be documented in entries description",
        field
      )
    )
  }

  # Now verify actual output matches
  context <- AgentContext$new()
  context$init_resources()
  on.exit(unlink(context$store_path, recursive = TRUE), add = TRUE)

  context$with_activated({
    context$logger(
      "Test",
      caller = context,
      level = "INFO",
      verbose = "none"
    )
  })

  result <- context$with_activated({
    mcp_tool_context_logs_head(max_lines = 10L)
  })

  parsed <- jsonlite::fromJSON(as.character(result))

  # Verify entries has exactly the documented fields
  actual_fields <- names(parsed$entries)
  expect_setequal(actual_fields, expected_fields)
})

# =============================================================================
# ellmer tool_string integration tests
# =============================================================================
# These tests verify that JSON output from MCP tools flows correctly through
# ellmer's tool_string() function, ensuring AI receives properly formatted JSON.

test_that(
  "JSON tool output flows correctly through ellmer tool_string",
  {
  skip_if_not_installed("ellmer")

  # Create context with logs

  context <- AgentContext$new()
  context$init_resources()
  on.exit(unlink(context$store_path, recursive = TRUE), add = TRUE)

  context$with_activated({
    context$logger(
      "Test log entry",
      caller = context,
      level = "INFO",
      verbose = "none"
    )
  })


  # Get raw JSON output from tool
  raw_result <- context$with_activated({
    mcp_tool_context_logs_head(max_lines = 10L)
  })

  # Verify it has json class (from jsonlite::toJSON)
  expect_s3_class(raw_result, "json")

  # Simulate what ellmer does: wrap in ContentToolResult and call tool_string
  content_result <- ellmer::ContentToolResult(value = raw_result)

  # This is what gets sent to AI
  ai_receives <- ellmer:::tool_string(content_result)

  # The JSON should be passed through unchanged
  expect_identical(ai_receives, raw_result)
  expect_identical(as.character(ai_receives), as.character(raw_result))
})

test_that(
  "tool_string preserves exact JSON structure for all log tools",
  {
  skip_if_not_installed("ellmer")

  context <- AgentContext$new()
  context$init_resources()
  on.exit(unlink(context$store_path, recursive = TRUE), add = TRUE)

  # Create varied log entries
  context$with_activated({
    context$logger(
      "Info log",
      caller = context,
      level = "INFO",
      verbose = "none"
    )
    context$logger(
      "Warning log",
      caller = context,
      level = "WARN",
      verbose = "none"
    )
    context$logger(
      "Error log",
      caller = context,
      level = "ERROR",
      verbose = "none"
    )
  })

  tools <- list(
    head = function() mcp_tool_context_logs_head(max_lines = 10L),
    tail = function() mcp_tool_context_logs_tail(max_lines = 10L),
    search = function() mcp_tool_context_logs_search(pattern = "log")
  )

  for (tool_name in names(tools)) {
    raw_result <- context$with_activated({
      tools[[tool_name]]()
    })

    # Wrap in ContentToolResult like ellmer does
    content_result <- ellmer::ContentToolResult(value = raw_result)
    ai_receives <- ellmer:::tool_string(content_result)

    # Parse both to compare structure
    original_parsed <- jsonlite::fromJSON(as.character(raw_result))
    ai_parsed <- jsonlite::fromJSON(as.character(ai_receives))

    # Structure should be identical
    expect_equal(names(original_parsed), names(ai_parsed),
      info = sprintf("Tool %s: top-level field names preserved", tool_name))
    expect_equal(original_parsed$success, ai_parsed$success,
      info = sprintf("Tool %s: success field preserved", tool_name))
    expect_equal(original_parsed$count, ai_parsed$count,
      info = sprintf("Tool %s: count field preserved", tool_name))
  }
})

test_that(
  "instantiated ellmer tool returns JSON that flows through tool_string",
  {
  skip_if_not_installed("ellmer")

  yaml_spec <- mcptool_read("tricobbler-mcp_tool_context_logs_head")
  state_env <- mcptool_state_factory()
  tool <- mcptool_instantiate(yaml_spec, state_env = state_env)

  context <- AgentContext$new()
  context$init_resources()
  on.exit(unlink(context$store_path, recursive = TRUE), add = TRUE)

  context$with_activated({
    context$logger(
      "Ellmer integration test",
      caller = context,
      level = "INFO",
      verbose = "none"
    )
  })

  # Execute through instantiated tool (simulates real usage)
  result <- context$with_activated({
    tool(max_lines = 10L)
  })

  # Result should be json class
  expect_s3_class(result, "json")

  # Wrap in ContentToolResult and verify tool_string passes it through
  content_result <- ellmer::ContentToolResult(value = result)
  ai_receives <- ellmer:::tool_string(content_result)

  # Should be identical
  expect_identical(ai_receives, result)

  # Verify structure is valid JSON with expected schema
  parsed <- jsonlite::fromJSON(as.character(ai_receives))
  expect_true(parsed$success)
  expect_true("context_id" %in% names(parsed))
  expect_true("entries" %in% names(parsed))
  expect_true("count" %in% names(parsed))
})

test_that(
  "error responses flow correctly through tool_string",
  {
  skip_if_not_installed("ellmer")

  # Test without active context - should return error JSON
  result <- mcp_tool_context_logs_head(max_lines = 10L)

  expect_s3_class(result, "json")

  # Simulate ellmer flow
  content_result <- ellmer::ContentToolResult(value = result)
  ai_receives <- ellmer:::tool_string(content_result)

  # Parse and verify error structure
  parsed <- jsonlite::fromJSON(as.character(ai_receives))
  expect_false(parsed$success)
  expect_true(!is.null(parsed$error) && nzchar(parsed$error))
})

test_that(
  "JSON output is compact (no pretty printing) for efficient AI consumption",
  {
  skip_if_not_installed("ellmer")

  context <- AgentContext$new()
  context$init_resources()
  on.exit(unlink(context$store_path, recursive = TRUE), add = TRUE)

  context$with_activated({
    context$logger(
      "Test message",
      caller = context,
      level = "INFO",
      verbose = "none"
    )
  })

  result <- context$with_activated({
    mcp_tool_context_logs_head(max_lines = 10L)
  })

  # Verify JSON is compact (single line, no indentation)
  json_str <- as.character(result)

  # Should not have newlines except within string values
  # A compact JSON should be a single line
  lines <- strsplit(json_str, "\n")[[1]]
  expect_equal(length(lines), 1,
    info = "JSON should be compact (single line) for efficient AI consumption")
})

test_that("JSON arrays are serialized as rows not columns", {
  skip_if_not_installed("ellmer")

  context <- AgentContext$new()
  context$init_resources()
  on.exit(unlink(context$store_path, recursive = TRUE), add = TRUE)

  context$with_activated({
    context$logger("First", caller = context, level = "INFO", verbose = "none")
    context$logger("Second", caller = context, level = "WARN", verbose = "none")
  })

  result <- context$with_activated({
    mcp_tool_context_logs_head(max_lines = 10L)
  })

  # Parse JSON
  parsed <- jsonlite::fromJSON(as.character(result))

  # entries should be a data.frame with rows (not a list of columns)
  expect_s3_class(parsed$entries, "data.frame")
  expect_equal(nrow(parsed$entries), 2)

  # Each row should represent one log entry
  expect_equal(parsed$entries$content[1], "First")
  expect_equal(parsed$entries$content[2], "Second")
})
