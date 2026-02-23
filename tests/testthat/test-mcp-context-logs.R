# Test MCP context log tools - plain text output format

# Helper to parse a single line from log output format:
# "  1: [HH:MM:SS][caller][LEVEL] content"
parse_log_line <- function(line) {
  m <- regmatches(
    line,
    regexec(
      "^\\s*(\\d+):\\s*\\[([^]]+)\\]\\[([^]]+)\\]\\[([^]]+)\\]\\s*(.*)$",
      line
    )
  )[[1]]
  if (length(m) == 0) {
    return(NULL)
  }
  list(
    line_no = as.integer(m[2]),
    time = m[3],
    caller = m[4],
    level = m[5],
    content = m[6]
  )
}

# Helper to parse all lines from log output into a data.frame
parse_log_output <- function(text) {
  empty <- data.frame(
    line_no = integer(), time = character(),
    caller = character(), level = character(),
    content = character(), stringsAsFactors = FALSE
  )
  if (!nzchar(trimws(text))) {
    return(empty)
  }
  lines <- strsplit(text, "\n")[[1]]
  parsed <- lapply(lines, parse_log_line)
  parsed <- parsed[!vapply(parsed, is.null, logical(1))]
  if (length(parsed) == 0) {
    return(empty)
  }
  data.frame(
    line_no = vapply(parsed, `[[`, integer(1), "line_no"),
    time = vapply(parsed, `[[`, character(1), "time"),
    caller = vapply(parsed, `[[`, character(1), "caller"),
    level = vapply(parsed, `[[`, character(1), "level"),
    content = vapply(parsed, `[[`, character(1), "content"),
    stringsAsFactors = FALSE
  )
}

# Helper to create a test context with its directory
create_test_context <- function() {
  ctx <- AgentContext$new()
  ctx$init_resources()
  ctx
}

# Helper to create a test runtime for MCP tool testing
# Creates a minimal runtime with a mock agent and policy that allows full access
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

test_that(
  "mcp_tool_context_logs_head returns formatted log lines",
  {
  yaml_spec <- mcptool_read("tricobbler-mcp_tool_context_logs_head")

  # Create test context with logs
  context <- create_test_context()
  on.exit(unlink(context$store_path, recursive = TRUE), add = TRUE)

  # Log a test message (no with_activated needed, caller is explicit)
  context$logger(
    "Test message",
    caller = context,
    level = "INFO",
    verbose = "none"
  )

  # Create runtime and call tool with .runtime parameter
  runtime <- create_test_runtime(context)
  result <- mcp_tool_context_logs_head(max_lines = 10L, .runtime = runtime)

  # Result should be a plain character string
  expect_type(result, "character")
  expect_true(nzchar(result))

  # Parse the formatted output
  entries <- parse_log_output(result)
  expect_true(nrow(entries) > 0)

  # Each entry should have the expected fields
  expect_true(all(!is.na(entries$line_no)))
  expect_true(all(!is.na(entries$level)))
  expect_true(all(!is.na(entries$time)))
  expect_true(all(!is.na(entries$caller)))
  expect_true(all(!is.na(entries$content)))

  # line_no should be positive integers
  expect_true(all(entries$line_no > 0))

  # level should be valid log levels
  valid_levels <- c("TRACE", "DEBUG", "INFO", "WARN", "ERROR", "FATAL")
  expect_true(all(entries$level %in% valid_levels))

  # time should match HH:MM:SS format
  expect_true(all(grepl("^\\d{2}:\\d{2}:\\d{2}$", entries$time)))
})

test_that("mcp_tool_context_logs_head returns string matching YAML spec", {
  # Load YAML specification
  yaml_spec <- mcptool_read("tricobbler-mcp_tool_context_logs_head")

  # Verify YAML spec structure
  expect_equal(yaml_spec$output_format, "string")

  # Create test context with logs
  context <- create_test_context()
  on.exit(unlink(context$store_path, recursive = TRUE), add = TRUE)

  # Log test messages (no with_activated needed)
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

  # Execute tool with runtime
  runtime <- create_test_runtime(context)
  result <- mcp_tool_context_logs_head(max_lines = 10L, .runtime = runtime)

  # Result is a plain character string
  expect_type(result, "character")

  # Parse formatted output
  entries <- parse_log_output(result)

  # Should have entries
  expect_true(nrow(entries) >= 3)

  # Validate entries structure
  expect_true(all(c("line_no", "level", "time", "caller", "content") %in%
    names(entries)))

  # Validate types within entries
  expect_type(entries$line_no, "integer")
  expect_type(entries$level, "character")
  expect_type(entries$time, "character")
  expect_type(entries$caller, "character")
  expect_type(entries$content, "character")
})

test_that("mcp_tool_context_logs_search error case returns plain error", {
  # Execute with missing required pattern parameter
  result <- mcp_tool_context_logs_search(pattern = "")

  expect_type(result, "character")

  # Error response should contain a meaningful message
  expect_true(grepl("pattern", result, ignore.case = TRUE))
})

test_that("mcp_tool_context_logs_tail returns string matching YAML spec", {
  yaml_spec <- mcptool_read("tricobbler-mcp_tool_context_logs_tail")

  expect_equal(yaml_spec$output_format, "string")

  context <- create_test_context()
  on.exit(unlink(context$store_path, recursive = TRUE), add = TRUE)

  # Log test messages (no with_activated needed)
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

  # Create runtime and read only last 2 entries
  runtime <- create_test_runtime(context)

  # Log additional messages AFTER runtime creation so they are the "tail"
  # (Runtime initialization adds a log line which would otherwise be last)
  context$logger(
    "Third message",
    caller = context, level = "INFO", verbose = "none"
  )
  context$logger(
    "Fourth message",
    caller = context, level = "INFO", verbose = "none"
  )

  result <- mcp_tool_context_logs_tail(max_lines = 2L, .runtime = runtime)

  expect_type(result, "character")

  entries <- parse_log_output(result)

  expect_equal(nrow(entries), 2L)

  # Tail should return last entries (Third and Fourth)
  expect_true(all(grepl("message", entries$content)))
})

test_that("mcp_tool_context_logs_search returns string matching YAML spec", {
  yaml_spec <- mcptool_read("tricobbler-mcp_tool_context_logs_search")

  expect_equal(yaml_spec$output_format, "string")

  context <- create_test_context()
  on.exit(unlink(context$store_path, recursive = TRUE), add = TRUE)

  # Log test messages (no with_activated needed)
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

  # Create runtime and search for "error" pattern
  runtime <- create_test_runtime(context)
  result <- mcp_tool_context_logs_search(
    pattern = "error",
    max_lines = 100L,
    .runtime = runtime
  )

  expect_type(result, "character")

  entries <- parse_log_output(result)

  expect_equal(nrow(entries), 1L)
  expect_true(grepl("error", entries$content, ignore.case = TRUE))
})

test_that("mcp_tool_context_logs_search validates required pattern parameter", {
  yaml_spec <- mcptool_read("tricobbler-mcp_tool_context_logs_search")

  # pattern should be required according to spec
  required_params <- yaml_spec$parameters$required
  expect_true("pattern" %in% required_params)

  context <- create_test_context()
  on.exit(unlink(context$store_path, recursive = TRUE), add = TRUE)

  # Create runtime and call without pattern - should return error string
  runtime <- create_test_runtime(context)
  result <- mcp_tool_context_logs_search(.runtime = runtime)

  expect_type(result, "character")
  expect_true(grepl("pattern", result, ignore.case = TRUE))
})

test_that("ellmer tool instantiation preserves YAML spec metadata", {
  skip_if_not_installed("ellmer")

  yaml_spec <- mcptool_read("tricobbler-mcp_tool_context_logs_head")

  # Instantiate as ellmer tool
  tool <- mcptool_instantiate(yaml_spec)

  # Verify it's an ellmer ToolDef
  expect_s3_class(tool, "ellmer::ToolDef")

  # Verify tool name matches
  expect_equal(tool@name, gsub("[^a-zA-Z0-9_-]+", "-", yaml_spec$name))

  # Verify description includes category and returns info
  expect_true(grepl("Category", tool@description))
  expect_true(grepl("info", tool@description))
  expect_true(grepl("Returns", tool@description))
})

test_that("instantiated tool executes and returns valid output", {
  skip_if_not_installed("ellmer")

  # Create context and add logs first (needed to create runtime)
  context <- create_test_context()
  on.exit(unlink(context$store_path, recursive = TRUE), add = TRUE)

  # Log test message (no with_activated needed)
  context$logger(
    "Ellmer test log",
    caller = context,
    level = "INFO",
    verbose = "none"
  )

  # Create runtime and instantiate tool with runtime
  runtime <- create_test_runtime(context)
  yaml_spec <- mcptool_read("tricobbler-mcp_tool_context_logs_head")
  tool <- mcptool_instantiate(yaml_spec, runtime = runtime)

  # Execute the tool (ToolDef is callable as a function)
  # Result goes through mcp_describe() which may transform short strings
  result <- tool(max_lines = 10L)

  expect_type(result, "character")
  expect_true(nzchar(result))
})

test_that("entries have consistent structure", {
  context <- create_test_context()
  on.exit(unlink(context$store_path, recursive = TRUE), add = TRUE)

  # Create multiple log entries with different levels (no with_activated needed)
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

  # Create runtime and call tool
  runtime <- create_test_runtime(context)
  result <- mcp_tool_context_logs_head(max_lines = 100L, .runtime = runtime)

  entries <- parse_log_output(result)

  # Each entry should have exactly these fields
  expected_fields <- c("line_no", "content", "level", "caller", "time")

  expect_true(all(expected_fields %in% names(entries)))

  # All entries should have non-NA values for required fields
  expect_true(all(!is.na(entries$line_no)))
  expect_true(all(!is.na(entries$level)))
  expect_true(all(!is.na(entries$time)))
  expect_true(all(!is.na(entries$caller)))
  expect_true(all(!is.na(entries$content)))

  # line_no should be positive integers
  expect_true(all(entries$line_no > 0))

  # level should be valid log levels
  valid_levels <- c("TRACE", "DEBUG", "INFO", "WARN", "ERROR", "FATAL")
  expect_true(all(entries$level %in% valid_levels))

  # time should match HH:MM:SS format
  expect_true(all(grepl("^\\d{2}:\\d{2}:\\d{2}$", entries$time)))
})

test_that("level filtering works correctly", {
  context <- create_test_context()
  on.exit(unlink(context$store_path, recursive = TRUE), add = TRUE)

  # Log test messages (no with_activated needed)
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

  # Create runtime and filter to only ERROR level
  runtime <- create_test_runtime(context)
  result <- mcp_tool_context_logs_head(
    max_lines = 100L, levels = "ERROR",
    .runtime = runtime
  )

  entries <- parse_log_output(result)

  expect_equal(nrow(entries), 1L)
  expect_equal(entries$level, "ERROR")
  expect_true(grepl("Error only", entries$content))
})

test_that("pattern filtering works correctly", {
  context <- create_test_context()
  on.exit(unlink(context$store_path, recursive = TRUE), add = TRUE)

  # Log test messages (no with_activated needed)
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

  # Create runtime and call tool with pattern filter
  runtime <- create_test_runtime(context)
  result <- mcp_tool_context_logs_head(
    max_lines = 100L, pattern = "Processing",
    .runtime = runtime
  )

  entries <- parse_log_output(result)

  expect_equal(nrow(entries), 2L)
  expect_true(all(grepl("Processing", entries$content)))
})

test_that("skip_lines pagination works", {
  context <- create_test_context()
  on.exit(unlink(context$store_path, recursive = TRUE), add = TRUE)

  # Log test messages (no with_activated needed)
  context$logger("Line 1", caller = context, level = "INFO", verbose = "none")
  context$logger("Line 2", caller = context, level = "INFO", verbose = "none")
  context$logger("Line 3", caller = context, level = "INFO", verbose = "none")
  context$logger("Line 4", caller = context, level = "INFO", verbose = "none")

  # Create runtime and skip first 2 lines from head
  runtime <- create_test_runtime(context)
  
  # Runtime initialization adds a log line. We want to
  # test logic on KNOWN lines.
  # So we clear logs, then add our lines.
  if (file.exists(context$logger_path)) unlink(context$logger_path)
  
  context$logger(
    "Line 2",
    caller = context, level = "INFO", verbose = "none"
  )
  context$logger(
    "Line 3",
    caller = context, level = "INFO", verbose = "none"
  )
  context$logger(
    "Line 4",
    caller = context, level = "INFO", verbose = "none"
  )

  result <- mcp_tool_context_logs_head(
    max_lines = 100L, skip_lines = 0L,
    .runtime = runtime
  )
  # ORIGINAL TEST LOGIC: skip_lines=2 was
  # skipping "Line 1" and (?)
  # Original code had:
  # context$logger("Line 2"...)
  # context$logger("Line 3"...)
  # context$logger("Line 4"...)
  # skip_lines=2.
  # Expected: Line 3, Line 4.
  
  # If I cleared logs, I have Line 2, 3, 4.
  # skip_lines=1 -> Line 3, 4.
  
  # Or I can just replicate original state:
  if (file.exists(context$logger_path)) unlink(context$logger_path)
  context$logger(
    "Line 1 (hidden)",
    caller = context, level = "INFO", verbose = "none"
  )
  context$logger(
    "Line 2 (hidden)",
    caller = context, level = "INFO", verbose = "none"
  )
  context$logger(
    "Line 3",
    caller = context, level = "INFO", verbose = "none"
  )
  context$logger(
    "Line 4",
    caller = context, level = "INFO", verbose = "none"
  )
  
  # skip_lines=2 should give Line 3, 4
  result <- mcp_tool_context_logs_head(
    max_lines = 100L, skip_lines = 2L,
    .runtime = runtime
  )

  entries <- parse_log_output(result)

  expect_equal(nrow(entries), 2L)
  expect_true(grepl("Line 3", entries$content[1]))
  expect_true(grepl("Line 4", entries$content[2]))
})

test_that("empty log file returns valid empty response", {
  context <- create_test_context()
  on.exit(unlink(context$store_path, recursive = TRUE), add = TRUE)

  # Don't add any logs

  # Create runtime and call tool on empty log
  runtime <- create_test_runtime(context)
  # Runtime initialization log breaks "empty" assumption. Clear it.
  if (file.exists(context$logger_path)) unlink(context$logger_path)
  
  result <- mcp_tool_context_logs_head(max_lines = 10L, .runtime = runtime)

  expect_type(result, "character")

  # Empty log should produce empty or whitespace-only string
  entries <- parse_log_output(result)
  expect_equal(nrow(entries), 0L)
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

    # All should have output_format: string
    expect_equal(yaml_spec$output_format, "string",
      info = sprintf("%s should have output_format: string", tool_name))

    # All should have category: info
    expect_equal(yaml_spec$category, "info",
      info = sprintf("%s should have category: info", tool_name))
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
})

test_that("actual output matches plain text format", {
  context <- create_test_context()
  on.exit(unlink(context$store_path, recursive = TRUE), add = TRUE)

  # Log test message (no with_activated needed)
  context$logger("Test", caller = context, level = "INFO", verbose = "none")

  # Create runtime and call tool
  runtime <- create_test_runtime(context)
  result <- mcp_tool_context_logs_head(max_lines = 10L, .runtime = runtime)

  expect_type(result, "character")

  # Parse and verify line format
  entries <- parse_log_output(result)
  expect_true(nrow(entries) > 0)
  expect_true(all(entries$line_no > 0))
  expect_true(all(entries$level %in%
    c("TRACE", "DEBUG", "INFO", "WARN", "ERROR", "FATAL")))
})

test_that(
  "error response returns a plain error string",
  {
  # Call with empty pattern to get error (search requires a pattern)
  result <- mcp_tool_context_logs_search(pattern = "")

  # Error response should be a plain string
  expect_type(result, "character")
  expect_true(nzchar(result))
  expect_true(grepl("pattern", result, ignore.case = TRUE))
})

test_that("log output contains all documented entry fields", {
  # Now verify actual output matches
  context <- create_test_context()
  on.exit(unlink(context$store_path, recursive = TRUE), add = TRUE)

  # Log test message (no with_activated needed)
  context$logger(
    "Test",
    caller = context,
    level = "INFO",
    verbose = "none"
  )

  # Create runtime and call tool
  runtime <- create_test_runtime(context)
  result <- mcp_tool_context_logs_head(max_lines = 10L, .runtime = runtime)

  # Verify entries have the documented fields
  entries <- parse_log_output(result)
  expected_fields <- c("line_no", "level", "time", "caller", "content")
  expect_true(all(expected_fields %in% names(entries)))
})

# =============================================================================
# ellmer tool_string integration tests
# =============================================================================
# These tests verify that plain text output from MCP tools flows correctly
# through ellmer's tool_string() function.

test_that(
  "tool output flows correctly through ellmer tool_string",
  {
  skip_if_not_installed("ellmer")

  # Create context with logs
  context <- create_test_context()
  on.exit(unlink(context$store_path, recursive = TRUE), add = TRUE)

  # Log test message (no with_activated needed)
  context$logger(
    "Test log entry",
    caller = context,
    level = "INFO",
    verbose = "none"
  )

  # Create runtime and get raw output from tool
  runtime <- create_test_runtime(context)
  raw_result <- mcp_tool_context_logs_head(max_lines = 10L, .runtime = runtime)

  # Verify it is a plain character string
  expect_type(raw_result, "character")

  # Simulate what ellmer does: wrap in ContentToolResult and call tool_string
  content_result <- ellmer::ContentToolResult(value = raw_result)

  # This is what gets sent to AI
  ai_receives <- ellmer:::tool_string(content_result)

  # The string should be passed through unchanged
  expect_identical(as.character(ai_receives), as.character(raw_result))
})

test_that(
  "tool_string preserves exact output for all log tools",
  {
  skip_if_not_installed("ellmer")

  context <- create_test_context()
  on.exit(unlink(context$store_path, recursive = TRUE), add = TRUE)

  # Create varied log entries (no with_activated needed)
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

  # Create runtime for tool calls
  runtime <- create_test_runtime(context)

  tools <- list(
    head = function() {
      mcp_tool_context_logs_head(
        max_lines = 10L, .runtime = runtime
      )
    },
    tail = function() {
      mcp_tool_context_logs_tail(
        max_lines = 10L, .runtime = runtime
      )
    },
    search = function() {
      mcp_tool_context_logs_search(
        pattern = "log", .runtime = runtime
      )
    }
  )

  for (tool_name in names(tools)) {
    raw_result <- tools[[tool_name]]()

    # All results should be plain character strings
    expect_type(raw_result, "character")

    # Wrap in ContentToolResult like ellmer does
    content_result <- ellmer::ContentToolResult(value = raw_result)
    ai_receives <- ellmer:::tool_string(content_result)

    # String should be preserved through the flow
    expect_identical(as.character(ai_receives), as.character(raw_result),
      info = sprintf("Tool %s: output preserved through tool_string",
        tool_name))
  }
})

test_that(
  "instantiated ellmer tool returns output that flows through tool_string",
  {
  skip_if_not_installed("ellmer")

  # Create context first
  context <- create_test_context()
  on.exit(unlink(context$store_path, recursive = TRUE), add = TRUE)

  # Log test message (no with_activated needed)
  context$logger(
    "Ellmer integration test",
    caller = context,
    level = "INFO",
    verbose = "none"
  )

  # Create runtime and instantiate tool with runtime
  runtime <- create_test_runtime(context)
  yaml_spec <- mcptool_read("tricobbler-mcp_tool_context_logs_head")
  tool <- mcptool_instantiate(yaml_spec, runtime = runtime)

  # Execute through instantiated tool (simulates real usage)
  result <- tool(max_lines = 10L)

  # Result should be a plain character string
  expect_type(result, "character")
  expect_true(nzchar(result))

  # Wrap in ContentToolResult and verify tool_string passes it through
  content_result <- ellmer::ContentToolResult(value = result)
  ai_receives <- ellmer:::tool_string(content_result)

  # Verify the output came through
  expect_type(as.character(ai_receives), "character")
  expect_true(nzchar(as.character(ai_receives)))
})

test_that(
  "error responses flow correctly through tool_string",
  {
  skip_if_not_installed("ellmer")

  # Test with empty pattern - should return error string
  result <- mcp_tool_context_logs_search(pattern = "")

  expect_type(result, "character")

  # Simulate ellmer flow
  content_result <- ellmer::ContentToolResult(value = result)
  ai_receives <- ellmer:::tool_string(content_result)

  # Verify error message comes through
  expect_true(grepl("pattern", as.character(ai_receives), ignore.case = TRUE))
})

test_that(
  "plain text output is human-readable with consistent line format",
  {
  context <- create_test_context()
  on.exit(unlink(context$store_path, recursive = TRUE), add = TRUE)

  # Log test message (no with_activated needed)
  context$logger(
    "Test message",
    caller = context,
    level = "INFO",
    verbose = "none"
  )

  # Create runtime and call tool
  runtime <- create_test_runtime(context)
  result <- mcp_tool_context_logs_head(max_lines = 10L, .runtime = runtime)

  # Each line should match the format: "  N: [time][caller][LEVEL] content"
  lines <- strsplit(result, "\n")[[1]]
  for (line_text in lines) {
    expect_true(
      grepl("^\\s*\\d+:\\s*\\[", line_text),
      info = sprintf(
        "Line should match format 'N: [time][caller][LEVEL] content': %s",
        line_text
      )
    )
  }
})

test_that("multiple log entries are separated by newlines", {
  context <- create_test_context()
  on.exit(unlink(context$store_path, recursive = TRUE), add = TRUE)

  # Create runtime and call tool
  runtime <- create_test_runtime(context)
  # Clear runtime init log to keep test clean
  if (file.exists(context$logger_path)) unlink(context$logger_path)

  # Log test messages (no with_activated needed)
  context$logger("First", caller = context, level = "INFO", verbose = "none")
  context$logger("Second", caller = context, level = "WARN", verbose = "none")

  result <- mcp_tool_context_logs_head(max_lines = 10L, .runtime = runtime)

  # Should have 2 lines
  entries <- parse_log_output(result)
  expect_equal(nrow(entries), 2)

  # Each row should represent one log entry
  expect_equal(entries$content[1], "First")
  expect_equal(entries$content[2], "Second")
})
