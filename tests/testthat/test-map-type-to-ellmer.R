# Tests for map_type_to_ellmer()

test_that("map_type_to_ellmer handles character shorthand", {
  result <- map_type_to_ellmer("string")
  expect_s3_class(result, "ellmer::TypeBasic")

  result <- map_type_to_ellmer("integer")
  expect_s3_class(result, "ellmer::TypeBasic")

  result <- map_type_to_ellmer("number")
  expect_s3_class(result, "ellmer::TypeBasic")

  result <- map_type_to_ellmer("boolean")
  expect_s3_class(result, "ellmer::TypeBasic")
})

test_that("map_type_to_ellmer handles list with type field", {
  result <- map_type_to_ellmer(list(type = "string", description = "A name"))
  expect_s3_class(result, "ellmer::TypeBasic")

  result <- map_type_to_ellmer(list(type = "integer", description = "Count"))
  expect_s3_class(result, "ellmer::TypeBasic")
})

test_that("map_type_to_ellmer handles enum types", {
  result <- map_type_to_ellmer(list(
    type = "string",
    enum = c("red", "green", "blue"),
    description = "Color choice"
  ))
  expect_s3_class(result, "ellmer::TypeEnum")
})

test_that("map_type_to_ellmer handles array types", {
  # Array with string items

  result <- map_type_to_ellmer(list(
    type = "array",
    items = list(type = "string"),
    description = "List of names"
  ))

  expect_s3_class(result, "ellmer::TypeArray")

  # Array with integer items
  result <- map_type_to_ellmer(list(
    type = "array",
    items = list(type = "integer", description = "A number"),
    description = "List of counts"
  ))
  expect_s3_class(result, "ellmer::TypeArray")

  # Array without explicit items (defaults to string)
  result <- map_type_to_ellmer(list(type = "array"))
  expect_s3_class(result, "ellmer::TypeArray")
})

test_that("map_type_to_ellmer handles object types", {
  result <- map_type_to_ellmer(list(
    type = "object",
    description = "User info",
    properties = list(
      name = list(type = "string", description = "User name"),
      age = list(type = "integer", description = "User age")
    )
  ))
  expect_s3_class(result, "ellmer::TypeObject")
})

test_that("map_type_to_ellmer handles nested object types", {
  result <- map_type_to_ellmer(list(
    type = "object",
    description = "Complex structure",
    properties = list(
      user = list(
        type = "object",
        description = "Nested user",
        properties = list(
          name = list(type = "string"),
          scores = list(
            type = "array",
            items = list(type = "number")
          )
        )
      )
    )
  ))
  expect_s3_class(result, "ellmer::TypeObject")
})

test_that("map_type_to_ellmer handles required field", {
  # Default is required = TRUE
  result <- map_type_to_ellmer(list(type = "string"))
  expect_true(result@required)

  # Explicit required = FALSE
  result <- map_type_to_ellmer(list(type = "string", required = FALSE))
  expect_false(result@required)
})

test_that("map_type_to_ellmer falls back to string for unknown types", {
  result <- map_type_to_ellmer(list(type = "unknown_type"))
  expect_s3_class(result, "ellmer::TypeBasic")
})

test_that("map_type_to_ellmer errors on invalid input", {
  expect_error(map_type_to_ellmer(123), "`type_def` must be a list or character")
  expect_error(map_type_to_ellmer(NULL), "`type_def` must be a list or character")
})

test_that("map_type_to_ellmer handles empty properties object", {
  result <- map_type_to_ellmer(list(
    type = "object",
    description = "Empty object"
  ))
  expect_s3_class(result, "ellmer::TypeObject")
})
