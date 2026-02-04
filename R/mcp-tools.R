#' \verb{MCP} tool extraction and type inference
#' @keywords internal
#' @name mcp-tools
NULL

#' Clean LaTeX/Rd Markup from Text
#'
#' @param text Character. Text with potential LaTeX/Rd markup
#' @return Character. Cleaned text
#' @keywords internal
#' @noRd
clean_rd_markup <- function(text) {
  if (is.null(text) || !nzchar(text)) {
    return(text)
  }

  # Remove \code{...}
  text <- gsub("\\\\code\\{([^}]+)\\}", "\\1", text)

  # Remove \link{...}
  text <- gsub("\\\\link\\{([^}]+)\\}", "\\1", text)

  # Remove \link[pkg]{...}
  text <- gsub("\\\\link\\[[^]]+\\]\\{([^}]+)\\}", "\\1", text)

  # Remove \describe{...}
  text <- gsub("\\\\describe\\{", "", text)

  # Remove \item{...}
  text <- gsub("\\\\item\\{([^}]+)\\}", "\\1", text)

  # Remove other common Rd tags
  text <- gsub("\\\\(emph|strong|bold|italic)\\{([^}]+)\\}", "\\2", text)

  # Clean up extra whitespace
  text <- gsub("\\s+", " ", text)
  text <- trimws(text)

  text
}

#' Strip R Type Prefixes from Descriptions
#'
#' @param text Character. Description text that may start with R type prefix
#' @return Character. Description with type prefix removed
#' @keywords internal
#' @noRd
strip_r_type_prefix <- function(text) {
  if (is.null(text) || !nzchar(text)) {
    return(text)
  }

  # Remove common R type prefixes at start of description
  # Pattern: "Type[specifier], rest of description" or
  # "Type[], rest of description"
  # Examples: "Character string, ...", "Logical, ...",
  # "Character[], ..."
  text <- sub("^(Character string|Character vector|Character\\[\\]|Character|Logical|Integer|Numeric|Double|List|Data frame|Matrix|Array\\[\\]|Array|Vector|Function|Environment)([,:]?\\s*)", "", text, ignore.case = TRUE) # nolint: line_length_linter.

  # Capitalize first letter after stripping
  if (nzchar(text)) {
    substr(text, 1, 1) <- toupper(substr(text, 1, 1))
  }

  text
}

#' Map R Type Names to JSON Schema Types
#'
#' @param r_type Character. R type name (e.g., "character", "numeric")
#' @return Character. JSON schema type ("string", "number", etc.)
#' @keywords internal
#' @noRd
map_r_type_to_json <- function(r_type) {
  r_type_lower <- tolower(trimws(r_type))

  switch(
    r_type_lower,
    "character" = "string",
    "string" = "string",
    "text" = "string",
    "numeric" = "number",
    "double" = "number",
    "float" = "number",
    "integer" = "integer",
    "int" = "integer",
    "logical" = "boolean",
    "boolean" = "boolean",
    "bool" = "boolean",
    "list" = "object",
    "object" = "object",
    "environment" = "object",
    "vector" = "array",
    "array" = "array",
    "string" # default fallback
  )
}

#' Parse Type String to JSON Schema Type
#'
#' @param type_str Character. Type string (e.g., \code{"character"},
#'   \code{"numeric[]"})
#' @return List with type and optional items field
#' @keywords internal
#' @noRd
parse_type_string <- function(type_str) {
  type_str <- trimws(type_str)

  # Handle array types: character[], numeric[]
  if (grepl("\\[\\]$", type_str)) {
    base_type <- sub("\\[\\]$", "", type_str)
    return(list(
      type = "array",
      items = list(type = map_r_type_to_json(base_type))
    ))
  }

  # Simple type
  list(type = map_r_type_to_json(type_str))
}

#' Infer Parameter Type from Description and Function Formals
#'
#' @param param_name Character. Parameter name
#' @param description Character. Parameter description from roxygen
#' @param func_value Function. The actual R function
#' @return List with type (and optional items for arrays)
#' @keywords internal
#' @noRd
infer_param_type <- function(param_name, description, func_value = NULL) {
  # Convert description to lowercase for case-insensitive matching
  desc_lower <- tolower(description)

  # Strategy 0A: Check for Type[] pattern at the start of description
  # (e.g., "Character[], array of...")
  # This should be checked before other patterns
  array_prefix_match <- regexec(
    "^\\s*([A-Za-z]+)\\[\\]",
    description,
    ignore.case = TRUE
  )
  array_matches <- regmatches(description, array_prefix_match)[[1]]

  if (length(array_matches) >= 2) {
    base_type <- array_matches[2]
    return(list(
      type = "array",
      items = list(type = map_r_type_to_json(base_type))
    ))
  }

  # Strategy 0B: Extract type from Rd markup
  # (e.g., \\code{character[]})
  # Common patterns: \code{type}, \code{type[]}, \link[pkg]{type}

  # Match \code{type} or \code{type[]} (case-insensitive)
  code_match <- regexec(
    "\\\\code\\{([A-Za-z]+(?:\\[\\])?)\\}",
    description,
    ignore.case = TRUE
  )
  matches <- regmatches(description, code_match)[[1]]

  if (length(matches) >= 2) {
    type_str <- matches[2]
    return(parse_type_string(type_str))
  }

  # Match \link{type} or \link[pkg]{type} (case-insensitive)
  link_match <- regexec(
    "\\\\link(?:\\[[^]]+\\])?\\{([A-Za-z]+(?:\\[\\])?)\\}",
    description,
    ignore.case = TRUE
  )
  matches <- regmatches(description, link_match)[[1]]

  if (length(matches) >= 2) {
    type_str <- matches[2]
    return(parse_type_string(type_str))
  }

  # Strategy 1: Look for explicit type prefix in description
  # e.g., "Character. The text to score"
  # e.g., "Numeric. Number of items"
  # e.g., "Character[]. List of tags"
  # e.g., "Character string, ..." or "Character vector, ..."

  # First try matching multi-word type patterns like
  # "Character string", "Numeric vector"
  multiword_type_match <- regexec(
    paste0(
      "^\\s*(Character string|Character vector|Numeric vector|",
      "Integer vector|Logical vector|Character\\[\\]|",
      "Numeric\\[\\]|Integer\\[\\])[,:]?\\s*"
    ),
    description,
    ignore.case = TRUE
  )
  multiword_matches <- regmatches(description, multiword_type_match)[[1]]

  if (length(multiword_matches) >= 2) {
    type_str <- multiword_matches[2]
    # Map multi-word types
    type_str_lower <- tolower(trimws(type_str))
    if (type_str_lower == "character string") {
      return(list(type = "string"))
    } else if (grepl("vector$", type_str_lower)) {
      # Extract base type from "X vector"
      base_type <- sub("\\s+vector$", "", type_str_lower, ignore.case = TRUE)
      return(list(
        type = "array",
        items = list(type = map_r_type_to_json(base_type))
      ))
    }
    return(parse_type_string(type_str))
  }

  # Extract first word/phrase before period or colon
  # (case-insensitive)
  type_match <- regexec(
    "^\\s*([A-Za-z]+(?:\\[\\])?)[.:]\\s*",
    description,
    ignore.case = TRUE
  )
  matches <- regmatches(description, type_match)[[1]]

  if (length(matches) >= 2) {
    type_str <- matches[2]
    return(parse_type_string(type_str))
  }

  # Strategy 2: Look for type keywords anywhere in description
  # Check for vector patterns first
  # (e.g., "character vector", "numeric vector")
  vector_patterns <- list(
    string = c(
      "\\bcharacter\\s+vector\\b",
      "\\bcharacter\\s+array\\b",
      "\\bstring\\s+vector\\b"
    ),
    number = c(
      "\\bnumeric\\s+vector\\b",
      "\\bnumeric\\s+array\\b",
      "\\bdouble\\s+vector\\b"
    ),
    integer = c(
      "\\binteger\\s+vector\\b",
      "\\binteger\\s+array\\b",
      "\\bint\\s+vector\\b"
    ),
    boolean = c("\\blogical\\s+vector\\b", "\\bboolean\\s+vector\\b")
  )

  for (item_type in names(vector_patterns)) {
    for (pattern in vector_patterns[[item_type]]) {
      if (grepl(pattern, desc_lower)) {
        return(list(
          type = "array",
          items = list(type = item_type)
        ))
      }
    }
  }

  type_patterns <- list(
    # Check object first - more specific patterns should come first
    object = c(
      "^\\s*object\\b", "\\blist\\b", "\\benvironment\\b",
      "\\bdata\\s*frame\\b", "\\bdata\\.frame\\b", "\\btibble\\b"
    ),
    string = c("\\bcharacter\\b", "\\bstring\\b", "\\btext\\b"),
    number = c("\\bnumeric\\b", "\\bdouble\\b", "\\bfloat\\b"),
    integer = c("\\binteger\\b", "\\bint\\b"),
    boolean = c("\\blogical\\b", "\\bboolean\\b", "\\bbool\\b"),
    array = c("\\bvector\\b", "\\barray\\b", "\\blist of\\b")
  )

  for (json_type in names(type_patterns)) {
    for (pattern in type_patterns[[json_type]]) {
      if (grepl(pattern, desc_lower)) {
        return(list(type = json_type))
      }
    }
  }

  # Strategy 3: Infer from default value in function formals
  if (!is.null(func_value) && is.function(func_value)) {
    formals_list <- formals(func_value)
    if (param_name %in% names(formals_list)) {
      default <- formals_list[[param_name]]

      # Check if default value exists
      if (!missing(default) && !identical(default, quote(expr = ))) {
        inferred <- infer_type_from_value(default)
        if (!is.null(inferred)) {
          return(inferred)
        }
      }
    }
  }

  # Default to string
  list(type = "string")
}

#' Infer Type from R Value
#'
#' @param value An R value
#' @return List with type or NULL
#' @keywords internal
#' @noRd
infer_type_from_value <- function(value) {
  # Handle quoted expressions (can't infer)
  if (is.symbol(value) || is.language(value)) {
    return(NULL)
  }

  if (is.character(value)) {
    return(list(type = "string"))
  } else if (is.integer(value)) {
    return(list(type = "integer"))
  } else if (is.numeric(value)) {
    return(list(type = "number"))
  } else if (is.logical(value)) {
    return(list(type = "boolean"))
  } else if (is.list(value)) {
    return(list(type = "object"))
  } else if (is.vector(value) && length(value) > 1) {
    # Vector of primitives -> array
    return(list(type = "array"))
  }

  NULL
}

#' Check if Parameter Has Default Value
#'
#' @param param_name Character. Parameter name
#' @param func Function. The R function
#' @return Logical
#' @keywords internal
#' @noRd
has_default_value <- function(param_name, func) {
  if (is.null(func) || !is.function(func)) {
    return(FALSE)
  }

  formals_list <- formals(func)
  if (param_name %in% names(formals_list)) {
    default <- formals_list[[param_name]]
    return(!missing(default) && !identical(default, quote(expr = )))
  }

  FALSE
}

#' Extract Enum and Default from Function Formals
#'
#' @param param_name Character. Parameter name
#' @param func Function. The R function
#' @param description Character. Parameter description to check if
#'   it's singular/plural
#' @return List with enum, default, and is_enum flag, or NULL
#' @keywords internal
#' @noRd
extract_enum_from_formals <- function(param_name, func, description) {
  if (is.null(func) || !is.function(func)) {
    return(NULL)
  }

  formals_list <- formals(func)
  if (!param_name %in% names(formals_list)) {
    return(NULL)
  }

  default_val <- formals_list[[param_name]]

  # Check if default is missing or empty
  if (missing(default_val) || identical(default_val, quote(expr = ))) {
    return(NULL)
  }

  # Try to evaluate the default to get the actual vector
  # For c("a", "b"), this will give us a character vector
  enum_values <- tryCatch({
    eval(default_val)
  }, error = function(e) {
    NULL
  })

  # Check if it's a vector with multiple values
  if (is.null(enum_values) || length(enum_values) <= 1) {
    return(NULL)
  }

  # Check if this is truly an enum (single value selection)
  # vs an array parameter
  # Look for clues in description: "string" suggests single,
  # "array"/"vector" suggests multiple
  desc_lower <- tolower(description)
  is_singular <- grepl("\\b(string|character string|single)\\b", desc_lower) &&
                 !grepl("\\b(array|vector|list|multiple)\\b", desc_lower)

  # Only treat as enum if it's singular type
  if (!is_singular) {
    return(NULL)
  }

  # Return enum values and default
  list(
    enum = as.list(enum_values),
    default = enum_values[1],
    is_enum = TRUE
  )
}

#' Serialize Default Value for JSON Schema
#'
#' @param default_val Default value from formals
#' @return Serialized value suitable for JSON, or NULL if can't serialize
#' @keywords internal
#' @noRd
serialize_default_value <- function(default_val) {
  # Handle missing/empty defaults
  if (missing(default_val) || identical(default_val, quote(expr = ))) {
    return(NULL)
  }

  # Try to evaluate the default
  value <- tryCatch({
    eval(default_val)
  }, error = function(e) {
    return(NULL)
  })

  if (is.null(value)) {
    # For NULL defaults, return explicit null
    if (identical(default_val, quote(NULL))) {
      return(NULL)  # JSON null
    }
    return(NULL)  # Can't serialize
  }

  # Convert R types to JSON-compatible types
  if (is.logical(value)) {
    # TRUE/FALSE -> true/false
    return(as.logical(value[1]))
  } else if (is.numeric(value) || is.integer(value)) {
    return(as.numeric(value[1]))
  } else if (is.character(value)) {
    return(as.character(value[1]))
  } else if (is.vector(value) && length(value) > 1) {
    # For arrays, return as list
    return(as.list(value))
  }

  # Can't serialize complex types
  NULL
}

#' Extract Examples from Parameter Description
#'
#' @param description Character. Parameter description that may
#'   contain example values
#' @return Character vector of examples, or NULL if none found
#' @keywords internal
#' @noRd
extract_examples_from_description <- function(description) {
  if (is.null(description) || !nzchar(description)) {
    return(NULL)
  }

  # Look for keywords that introduce examples
  # Pattern: "example values", "options are", "valid values",
  # "e.g.", "for example"
  pattern <- paste0(
    "(?i)(example values?|options? (?:are|include)|",
    "valid values?|e\\.g\\.|for example|such as)[:\\s]*"
  )

  # Check if description contains example keywords
  if (!grepl(pattern, description, perl = TRUE)) {
    return(NULL)
  }

  # Extract everything after the keyword
  parts <- strsplit(description, pattern, perl = TRUE)[[1]]
  if (length(parts) < 2) {
    return(NULL)
  }

  # Get the part after the keyword
  example_text <- parts[2]

  # Extract \code{"value"} or \code{'value'} or \code{value} patterns
  # Use a more robust pattern that handles nested braces in JSON
  # Split by \code{ and then find matching }
  code_starts <- gregexpr("\\\\code\\{", example_text)
  if (code_starts[[1]][1] == -1) {
    return(NULL)
  }

  start_positions <- code_starts[[1]]
  examples <- character()

  for (start_pos in start_positions) {
    # Find the matching closing brace
    # Start after \code{
    pos <- start_pos + 6  # length of "\\code{"
    depth <- 1
    chars <- strsplit(example_text, "")[[1]]
    in_string <- FALSE
    string_char <- NULL

    while (pos <= length(chars) && depth > 0) {
      ch <- chars[pos]

      # Handle string literals
      if (!in_string && (ch == '"' || ch == "'")) {
        in_string <- TRUE
        string_char <- ch
      } else if (in_string && ch == string_char &&
                 (pos == 1 || chars[pos - 1] != "\\")) {
        in_string <- FALSE
        string_char <- NULL
      }

      # Count braces only outside strings
      if (!in_string) {
        if (ch == "{") {
          depth <- depth + 1
        } else if (ch == "}") {
          depth <- depth - 1
        }
      }

      pos <- pos + 1
    }

    if (depth == 0) {
      # Extract content between \code{ and }
      content <- substr(example_text, start_pos + 6, pos - 2)
      # Remove surrounding quotes if present
      content <- gsub("^['\"]|['\"]$", "", content)
      if (nzchar(content)) {
        examples <- c(examples, content)
      }
    }
  }

  if (length(examples) == 0) {
    return(NULL)
  }

  as.list(examples)
}

#' Extract Tool Definition from Roxygen Block Lines
#'
#' @param block_lines Character vector of roxygen comment lines
#' @param func_name Character. Function name
#' @param pkg_name Character. Package name
#' @param source_file Character. Source file path
#' @param func_formals List. Function formals (optional)
#' @return List with tool definition or NULL
#' @keywords internal
#' @noRd
extract_tool_from_roxygen_block <- function(
    block_lines,
    func_name,
    pkg_name,
    source_file,
    func_formals = NULL
) {
  # Remove leading #' from lines
  clean_lines <- sub("^\\s*#'\\s?", "", block_lines)

  # Extract description
  description <- NULL
  desc_start <- which(grepl("^@description\\s*", clean_lines))
  if (length(desc_start) > 0) {
    desc_start <- desc_start[[1]]
    # Find next @ tag or end of block
    desc_end <- desc_start[1] + 1
    while (
      desc_end <= length(clean_lines) && !grepl("^@", clean_lines[desc_end])
    ) {
      desc_end <- desc_end + 1
    }
    desc_lines <- clean_lines[seq(desc_start, desc_end - 1)]
    description <- paste(trimws(desc_lines), collapse = " ")
    # Normalize whitespace
    description <- gsub("(^@description\\s+|\\s+)", " ", description)
    description <- trimws(description)
    description <- clean_rd_markup(description)
  }

  # Extract keyword metadata
  category <- NULL
  dangerous <- FALSE
  requires_approval <- FALSE
  output_format <- NULL

  keyword_line_idx <- grep("^@keywords\\s+", clean_lines)
  if (length(keyword_line_idx) > 0) {
    keyword_line <- clean_lines[keyword_line_idx[1]]
    keyword_content <- sub("^@keywords\\s+", "", keyword_line)
    # Split on whitespace and commas - use proper regex character class
    keywords <- unlist(strsplit(keyword_content, "[[:space:],]+"))
    keywords <- keywords[nzchar(keywords)]

    for (kw in keywords) {
      # Extract category from mcp-category-xxx
      if (grepl("^mcp-category-", kw)) {
        category <- sub("^mcp-category-", "", kw)
      }
      # Check for dangerous flag
      if (kw == "mcp-dangerous") {
        dangerous <- TRUE
      }
      # Check for requires approval flag
      if (kw == "mcp-requires-approval") {
        requires_approval <- TRUE
      }
      # Check for output format (json, text, etc.)
      if (grepl("^mcp-output-", kw)) {
        output_format <- sub("^mcp-output-", "", kw)
      }
    }
  }

  # Extract parameters (handle multi-line descriptions)
  param_starts <- grep("^@param\\s+", clean_lines)
  properties <- fastmap::fastmap()
  required <- fastmap::fastqueue()

  lapply(param_starts, function(param_idx) {
    # Find the end of this parameter (next @ tag or end of block)
    param_end <- param_idx + 1
    while (param_end <= length(clean_lines) &&
           !grepl("^@", clean_lines[param_end])) {
      param_end <- param_end + 1
    }

    # Collect all lines for this parameter
    param_lines_full <- clean_lines[param_idx:(param_end - 1)]
    param_text <- paste(param_lines_full, collapse = " ")

    # Parse: @param name Description
    parts <- sub("^@param\\s+", "", param_text)
    param_match <- regexec("^([a-zA-Z_][a-zA-Z0-9._]*)\\s+(.+)$", parts)
    matches <- regmatches(parts, param_match)[[1]]

    if (length(matches) >= 3) {
      param_name <- matches[2]

      # Skip internal parameters (starting with '.')
      # These are used for passing context like state environments that
      # should not be exposed to AI agents in MCP tool specifications
      if (startsWith(param_name, ".")) {
        return(NULL)
      }

      param_desc_raw <- matches[3]
      # Normalize whitespace
      param_desc_raw <- gsub("\\s+", " ", param_desc_raw)
      param_desc_raw <- trimws(param_desc_raw)

      # Extract examples from raw description (needs \code{} patterns)
      param_examples <- extract_examples_from_description(param_desc_raw)

      # Clean LaTeX/Rd markup for processing
      param_desc <- clean_rd_markup(param_desc_raw)

      # Create a mock function with formals for type inference
      func_value <- NULL
      if (!is.null(func_formals)) {
        # Create a simple function with the extracted formals
        tryCatch({
          func_value <- eval(call("function", func_formals, quote({})))
        }, error = function(e) {
          # Silently ignore if we can't create the mock function
        })
      }

      # Check for enum pattern (needs "string" keyword in description)
      enum_info <- extract_enum_from_formals(param_name, func_value, param_desc)

      # Infer type from description (needs "Character[]" pattern if present)
      param_type <- infer_param_type(param_name, param_desc, func_value)

      # Create clean description for output (strip R type prefixes)
      param_desc_clean <- strip_r_type_prefix(param_desc)

      # Build property
      prop_def <- list(
        type = param_type$type,
        description = param_desc_clean
      )

      if (!is.null(param_type$items)) {
        prop_def$items <- param_type$items
      }

      # Add enum if detected
      if (!is.null(enum_info) && enum_info$is_enum) {
        prop_def$enum <- enum_info$enum
        prop_def$default <- enum_info$default
      } else if (!is.null(func_value) && is.function(func_value)) {
        # Add default value for non-enum parameters
        formals_list <- formals(func_value)
        if (param_name %in% names(formals_list)) {
          default_val <- formals_list[[param_name]]
          serialized_default <- serialize_default_value(default_val)
          if (!is.null(serialized_default)) {
            prop_def$default <- serialized_default
          }
        }
      }

      # Add examples that were extracted before markup cleanup
      if (!is.null(param_examples) && length(param_examples) > 0) {
        prop_def$examples <- param_examples
      }

      properties$set(param_name, prop_def)

      # Determine if required
      # First check if we have function formals and can detect default value
      has_default_in_formals <- FALSE
      if (!is.null(func_value) && is.function(func_value)) {
        has_default_in_formals <- has_default_value(param_name, func_value)
      }

      # Also check description for default/optional indicators
      has_default_in_desc <- grepl(
        "default[:\\s]|if\\s+null|optional",
        param_desc,
        ignore.case = TRUE
      )

      # Parameter is required only if it has no default in formals AND
      # no default mentioned in description
      if (!has_default_in_formals && !has_default_in_desc) {
        required$add(param_name)
      }
    }
  })

  # Extract return value documentation
  returns <- NULL
  # Support both @return and @returns
  return_idx <- grep("^@returns?\\s+", clean_lines)

  if (length(return_idx) > 0) {
    # Find the start of return section
    return_start <- return_idx[1]
    # Find the end (next @ tag or end of block)
    return_end <- return_start + 1
    while (return_end <= length(clean_lines) &&
           !grepl("^@", clean_lines[return_end])) {
      return_end <- return_end + 1
    }

    # Collect return lines
    return_lines <- clean_lines[return_start:(return_end - 1)]

    # Remove @return/@returns tag from first line
    return_lines[1] <- sub("^@returns?\\s+", "", return_lines[1])

    # Parse \describe{} blocks if present
    return_text <- paste(return_lines, collapse = " ")

    # Check if it's a structured return with \describe{}
    if (grepl("\\\\describe\\{", return_text)) {
      # Extract items within \describe{...}
      # Handle nested braces by counting brace depth
      brace_depth <- 0
      describe_start <- regexpr("\\\\describe\\{", return_text)[1]
      if (describe_start > 0) {
        # Find matching closing brace
        chars <- strsplit(substring(return_text, describe_start), "")[[1]]
        describe_end <- describe_start - 1
        started <- FALSE

        for (i in seq_along(chars)) {
          if (chars[i] == "{") {
            brace_depth <- brace_depth + 1
            started <- TRUE
          } else if (chars[i] == "}") {
            brace_depth <- brace_depth - 1
            if (started && brace_depth == 0) {
              describe_end <- describe_start + i - 1
              break
            }
          }
        }

        if (describe_end > describe_start) {
          # Extract content between \describe{ and matching }
          describe_content <- substring(
            return_text,
            describe_start + 10,
            describe_end - 1
          )

          # Parse \item{name}{description} entries
          return_properties <- fastmap::fastmap()

          # Use a more robust item extraction
          pos <- 1
          while (pos <= nchar(describe_content)) {
            # Look for \item{
            item_start <- regexpr(
              "\\\\item\\{",
              substring(describe_content, pos)
            )
            if (item_start[1] < 0) break

            pos <- pos + item_start[1] - 1 + 6  # Move to after \item{

            # Extract first argument (name) - handle nested braces
            brace_depth <- 1
            name_start <- pos
            name_end <- pos
            substr_from_pos <- substring(describe_content, pos)
            chars <- strsplit(substr_from_pos, "")[[1]]

            for (i in seq_along(chars)) {
              if (chars[i] == "{") {
                brace_depth <- brace_depth + 1
              } else if (chars[i] == "}") {
                brace_depth <- brace_depth - 1
                if (brace_depth == 0) {
                  name_end <- pos + i - 2
                  pos <- pos + i
                  break
                }
              }
            }

            item_name <- trimws(
              substring(describe_content, name_start, name_end)
            )

            # Extract second argument (description) - handle nested braces
            if (substring(describe_content, pos, pos) == "{") {
              pos <- pos + 1
              brace_depth <- 1
              desc_start <- pos
              desc_end <- pos
              substr_from_pos <- substring(describe_content, pos)
              chars <- strsplit(substr_from_pos, "")[[1]]

              for (i in seq_along(chars)) {
                if (chars[i] == "{") {
                  brace_depth <- brace_depth + 1
                } else if (chars[i] == "}") {
                  brace_depth <- brace_depth - 1
                  if (brace_depth == 0) {
                    desc_end <- pos + i - 2
                    pos <- pos + i
                    break
                  }
                }
              }

              item_desc <- trimws(
                substring(describe_content, desc_start, desc_end)
              )

              # Clean LaTeX/Rd markup from description
              item_desc <- clean_rd_markup(item_desc)

              # Infer type from description
              item_type <- infer_param_type(item_name, item_desc, NULL)

              prop_def <- list(
                type = item_type$type,
                description = item_desc
              )

              if (!is.null(item_type$items)) {
                prop_def$items <- item_type$items
              }

              return_properties$set(item_name, prop_def)
            }
          }

          if (return_properties$size() > 0) {
            returns <- list(
              type = "object",
              properties = return_properties$as_list()
            )
          }
        }
      }
    } else {
      # Simple return description - try to infer type
      return_desc <- trimws(return_text)
      return_type <- infer_param_type("return", return_desc, NULL)

      returns <- list(
        type = return_type$type,
        description = return_desc
      )

      if (!is.null(return_type$items)) {
        returns$items <- return_type$items
      }
    }
  }

  # Extract implementation examples and execution time
  implementation_example <- NULL
  execution_time <- NULL

  examples_idx <- grep("^@examples\\s*", clean_lines)

  if (length(examples_idx) > 0) {
    # Find the start of examples section
    examples_start <- examples_idx[1] + 1
    # Find the end (next @ tag or end of block)
    examples_end <- examples_start
    while (examples_end <= length(clean_lines) &&
           !grepl("^@", clean_lines[examples_end])) {
      examples_end <- examples_end + 1
    }

    if (examples_end > examples_start) {
      examples_lines <- clean_lines[examples_start:(examples_end - 1)]

      # Extract all code lines (no longer looking for MCP example
      # response marker)
      code_lines <- examples_lines[nzchar(trimws(examples_lines))]

      if (length(code_lines) > 0 &&
          !all(startsWith(trimws(code_lines), "#"))) {
        # Strip the leading #' from each line to get actual R code
        code_text <- paste(code_lines, collapse = "\n")

        # Create display code (what ends up in the YAML)
        display_code <- paste0(code_text, "\n\n#\n")

        # Create execution code (captures the output)
        exec_code <- paste0(
          "res <- { ", code_text, " }\n",
          "tricobbler::mcp_describe(res, max_print = 5)"
        )

        timing <- system.time({
          # Execute in a new environment to avoid polluting globalenv
          # but allow access to globalenv (packages etc.)
          cat(c(exec_code, ""), sep = "\n")
          result_str <- eval(
            parse(text = exec_code),
            new.env(parent = globalenv())
          )
        })

        # Format output with comment prefix '#>'
        # Ensure result_str is not NULL/empty
        if (length(result_str) > 0) {
          formatted_output <- paste(
            sprintf("#> %s", result_str),
            collapse = "\n"
          )

          # Combine code and output
          implementation_example <- structure(
            paste(display_code, trimws(formatted_output), sep = "\n\n"),
            class = "literal"
          )
        } else {
           # Fallback if no output
           implementation_example <- structure(display_code, class = "literal")
        }

        # Store execution time in seconds
        execution_time <- as.numeric(timing["elapsed"])
      }
    }
  }

  # Create tool name
  tool_name <- paste0(pkg_name, "-", func_name)

  # Convert fastmap/fastqueue to regular list/vector
  properties_list <- properties$as_list()
  required_vec <- required$as_list()

  # Build tool definition
  tool_def <- list(
    name = tool_name,
    description = description,
    parameters = list(
      type = "object",
      properties = properties_list
    )
  )

  # Add required field if there are required parameters
  # Required must be an array according to JSON Schema spec
  if (length(required_vec) > 0) {
    tool_def$parameters$required <- as.list(unlist(required_vec))
  }

  # Add metadata fields
  if (!is.null(category)) {
    tool_def$category <- category
  }

  if (dangerous) {
    tool_def$dangerous <- TRUE
  }

  if (requires_approval) {
    tool_def$requires_approval <- TRUE
  }

  if (!is.null(returns)) {
    tool_def$returns <- returns
  }

  if (!is.null(output_format)) {
    tool_def$output_format <- output_format
  }

  if (!is.null(implementation_example)) {
    tool_def$implementation_example <- implementation_example
  }

  if (!is.null(execution_time)) {
    tool_def$execution_time <- execution_time
  }

  # Add class
  class(tool_def) <- c("tricobbler_mcp_tool", "list")

  tool_def
}

#' Build \verb{MCP} Tool Definitions from Package Documentation
#'
#' @description
#' Helper function for developers to scan the package's R source
#' files for functions marked with `@keywords mcp-tool` and generates
#' `YAML` tool definitions in `inst/mcp/tools/`. Run this after
#' documenting your package.
#'
#' @param path Character. Path to package root directory (default: `.`)
#' @param verbose Logical. Print progress messages (default: \code{TRUE})
#' @return Invisibly returns list of generated tool definitions
#' @examples
#'
#' \dontrun{
#'
#' # After documenting your package `devtools::document()`
#'
#' mcptool_build()
#'
#' }
#'
#' @export
mcptool_build <- function(path = ".", verbose = TRUE) {
  # Normalize path
  path <- normalizePath(path, mustWork = TRUE)

  # Check if this is a package directory
  desc_file <- file.path(path, "DESCRIPTION")
  if (!file.exists(desc_file)) {
    stop(
      "DESCRIPTION file not found in '",
      path,
      "'. Is this a package directory?"
    )
  }

  # Get package name
  pkg_name <- NULL
  tryCatch(
    {
      desc <- read.dcf(desc_file)
      pkg_name <- desc[1, "Package"]
    },
    error = function(e) {
      stop("Failed to read DESCRIPTION file: ", e$message)
    }
  )

  if (is.null(pkg_name) || !nzchar(pkg_name)) {
    stop("Could not determine package name from DESCRIPTION")
  }

  if (verbose) {
    message("Building MCP tools for package: ", pkg_name)
  }

  # Find all R source files
  r_dir <- file.path(path, "R")
  if (!dir.exists(r_dir)) {
    stop("R/ directory not found in '", path, "'")
  }

  r_files <- list.files(
    r_dir,
    pattern = "\\.R$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  if (length(r_files) == 0) {
    if (verbose) {
      message("No R files found")
    }
    return(invisible(list()))
  }

  # Parse R files to find mcp-tool functions
  # Note: We scan source files directly instead of using roxygen2::parse_file
  # because parse_file doesn't populate block$object$alias in standalone mode

  all_tools <- fastmap::fastqueue()

  lapply(r_files, function(r_file) {
    file_content <- readLines(r_file, warn = FALSE)

    # Find all lines with @keywords mcp-tool
    keyword_lines <- grep("^\\s*#'\\s*@keywords\\s+.*mcp-tool", file_content)

    if (length(keyword_lines) == 0) {
      return(NULL)
    }

    # For each @keywords mcp-tool line, extract the tool
    for (keyword_idx in keyword_lines) {
      # Find the first non-comment line after this keyword (function definition)
      func_line_idx <- keyword_idx + 1
      while (func_line_idx <= length(file_content)) {
        line <- file_content[func_line_idx]
        trimmed <- trimws(line)

        # Skip empty lines and comments
        if (nzchar(trimmed) && !startsWith(trimmed, "#'")) {
          break
        }
        func_line_idx <- func_line_idx + 1
      }

      if (func_line_idx > length(file_content)) {
        next # No function found
      }

      # Extract function name
      func_line <- file_content[func_line_idx]
      func_match <- regexec(
        "^\\s*([a-zA-Z_][a-zA-Z0-9._]*)\\s*<-\\s*function",
        func_line
      )
      matches <- regmatches(func_line, func_match)[[1]]

      if (length(matches) < 2) {
        next # Not a function definition
      }

      func_name <- matches[2]

      # Collect all roxygen comment lines before the function
      # Go backwards from keyword_idx to find the start of the block
      block_start <- keyword_idx
      while (block_start > 1) {
        prev_line <- trimws(file_content[block_start - 1])
        if (startsWith(prev_line, "#'")) {
          block_start <- block_start - 1
        } else {
          break
        }
      }

      # Collect all roxygen lines from block_start to
      # the line before func_line_idx
      block_lines <- trimws(
        file_content[seq(block_start, func_line_idx - 1, by = 1L)]
      )
      block_lines <- block_lines[startsWith(block_lines, "#'")]

      if (!length(block_lines)) {
        next # No roxygen block found
      }

      # Try to parse function formals
      func_formals <- NULL
      tryCatch({
        # Read up to 30 lines from function definition
        func_end_idx <- min(func_line_idx + 30, length(file_content))
        func_lines <- file_content[func_line_idx:func_end_idx]
        func_text <- paste(func_lines, collapse = " ")

        # Extract function(...) part - look for function(...) {
        # pattern. This handles nested parentheses in default values
        # like c("a", "b")
        func_match <- regexec(
          "function\\s*\\((.*?)\\)\\s*\\{",
          func_text
        )
        func_matches <- regmatches(func_text, func_match)[[1]]

        if (length(func_matches) >= 2) {
          # Create parseable function text
          fake_func_text <- paste0("function(", func_matches[2], ") {}")
          fake_func <- eval(parse(text = fake_func_text))
          func_formals <- formals(fake_func)
        }
      }, error = function(e) {
        if (verbose) {
          warning(
            "Could not parse function formals for '", func_name,
            "' in ", basename(r_file),
            ". Type inference from defaults will be skipped. ",
            "Error: ", e$message,
            call. = FALSE
          )
        }
      })

      # Extract tool definition
      if (verbose) {
        message(sprintf("Extracting tool: %s-%s", pkg_name, func_name))
      }
      tool <- extract_tool_from_roxygen_block(
        block_lines,
        func_name,
        pkg_name,
        r_file,
        func_formals = func_formals
      )

      if (!is.null(tool)) {
        all_tools$add(tool)
      }
    }
  })

  # Convert queue to list
  tools <- all_tools$as_list()

  if (length(tools) == 0) {
    if (verbose) {
      message("No functions with @keywords mcp-tool found")
    }
    return(invisible(list()))
  }

  if (verbose) {
    message("Found ", length(tools), " MCP tool(s)")
  }

  # Create inst/mcp/tools directory
  tools_dir <- file.path(path, "inst", "mcp", "tools")
  dir.create(tools_dir, recursive = TRUE, showWarnings = FALSE)

  # Get list of existing files for cleanup
  existing_files <- list.files(
    tools_dir,
    pattern = "\\.yaml$",
    full.names = TRUE
  )
  generated_files <- character()

  # Write each tool definition
  for (tool in tools) {
    if (!is.null(tool$name) && nzchar(tool$name)) {
      tool_file <- file.path(
        tools_dir,
        sprintf(
          "%s.yaml",
          gsub("[^a-zA-Z0-9_-]+", "-", tool$name)
        )
      )
      generated_files <- c(generated_files, tool_file)

      # Create tool definition with all fields
      tool_def <- list(
        name = tool$name,
        description = tool$description,
        parameters = tool$parameters
      )

      # Add optional metadata fields if present
      if (!is.null(tool$category)) {
        tool_def$category <- tool$category
      }

      if (!is.null(tool$dangerous) && tool$dangerous) {
        tool_def$dangerous <- TRUE
      }

      if (!is.null(tool$requires_approval) && tool$requires_approval) {
        tool_def$requires_approval <- TRUE
      }

      if (!is.null(tool$example_response)) {
        tool_def$example_response <- tool$example_response
      }

      if (!is.null(tool$returns)) {
        tool_def$returns <- tool$returns
      }

      if (!is.null(tool$output_format)) {
        tool_def$output_format <- tool$output_format
      }


      # Mark implementation_example as literal block scalar
      # for proper YAML formatting
      if (!is.null(tool$implementation_example)) {
        tool_def$implementation_example <- structure(
          "__Placeholder__\n",
          class = "literal"
        )
      }

      if (!is.null(tool$execution_time)) {
        tool_def$execution_time <- tool$execution_time
      }



      # Write YAML
      # yaml::as.yaml will use literal block style (|-)
      # for strings with class "literal"
      yaml_str <- yaml::as.yaml(
        x = tool_def,
        indent = 2L,
        line.sep = "\n"
      )
      yaml_str <- strsplit(yaml_str, "\n")[[1]]

      # Find placeholder
      if (length(tool_def$implementation_example)) {
        line_no <- which(grepl("\\s+__Placeholder__\\s*$", yaml_str))[[1]]
        indent <- gsub("__Placeholder__\\s*$", "", yaml_str[[line_no]])
        implementation_example_str <- strsplit(
          trimws(paste(tool$implementation_example, collapse = "\n")),
          "\n"
        )[[1]]
        implementation_example_str <- implementation_example_str[
          trimws(implementation_example_str) != ""
        ]
        implementation_example_str <- paste0(indent, implementation_example_str)
        yaml_str <- c(
          yaml_str[seq_len(line_no - 1)],
          implementation_example_str,
          yaml_str[-seq_len(line_no)]
        )
      }

      # Write to file
      cat(c(
        "# This file is automatically generated by tricobbler::mcptool_build(). ", # nolint: line_length_linter.
        "# Please never edit this file by hand!",
        yaml_str
      ), file = tool_file, sep = "\n")

      if (verbose) {
        message("  Generated: ", basename(tool_file))
      }

    }
  }

  # Clean up old tool files
  old_files <- setdiff(existing_files, generated_files)
  if (length(old_files) > 0) {
    for (old_file in old_files) {
      if (verbose) {
        message("  Removed outdated: ", basename(old_file))
      }
      unlink(old_file)
    }
  }

  if (verbose) {
    message("MCP tools built successfully in inst/mcp/tools/")
  }

  # Validate workflows against generated tools
  if (verbose) {
    message("\nValidating MCP workflows...")
  }

  workflow_dir <- file.path(path, "inst", "mcp", "workflows")
  if (dir.exists(workflow_dir)) {
    # tryCatch({
      # Load workflows manually (mcpflow_load_all may not be available)
      yaml_files <- list.files(
        workflow_dir,
        pattern = "\\.yaml$",
        full.names = TRUE
      )

      if (length(yaml_files) > 0) {
        # Get all generated tool names
        tool_names <- vapply(tools, function(t) t$name, character(1))

        # Process each workflow file
        for (yaml_file in yaml_files) {
          # workflow <- tryCatch({
            wf <- yaml::yaml.load_file(yaml_file)  # Use yaml.load_file instead
            if (!is.list(wf)) {
              warning(
                "Workflow file ", basename(yaml_file),
                " did not parse as a list",
                call. = FALSE
              )
              # NULL
              next
            } else {
              workflow <- wf
            }
          # }, error = function(e) {
          #   warning("Failed to parse workflow ", basename(yaml_file), ": ", e$message, call. = FALSE) # nolint: line_length_linter.
          #   NULL
          # })

          # if (is.null(workflow)) next

          workflow_name <- if (!is.null(workflow$name)) {
            workflow$name
          } else {
            tools::file_path_sans_ext(basename(yaml_file))
          }

          # Normalize mcp_tools field
          mcp_tools <- workflow$mcp_tools
          if (!is.null(mcp_tools)) {
            if (is.logical(mcp_tools)) {
              mcp_tools <- if (mcp_tools) "all" else character(0)
            } else if (is.character(mcp_tools) && length(mcp_tools) == 1) {
              normalized <- tolower(trimws(mcp_tools))
              if (normalized %in% c("yes", "true", "all")) {
                mcp_tools <- "all"
              } else if (normalized %in% c("no", "false", "~", "")) {
                mcp_tools <- character(0)
              }
            }
          }

          # Check mcp_tools field
          if (!is.null(mcp_tools) &&
              !identical(mcp_tools, "all") &&
              is.character(mcp_tools) &&
              length(mcp_tools) > 0) {
            missing_tools <- setdiff(mcp_tools, tool_names)
            if (length(missing_tools) > 0) {
              warning(
                "Workflow '", workflow_name,
                "' mcp_tools field references missing tools: ",
                paste(missing_tools, collapse = ", "),
                ". Please update workflow YAML.",
                call. = FALSE
              )
            }
          }

          # Check tool references in jobs
          if (!is.null(workflow$jobs) && is.list(workflow$jobs)) {
            job_tools <- character(0)
            for (job in workflow$jobs) {
              if (!is.null(job$steps) && is.list(job$steps)) {
                for (step in job$steps) {
                  if (!is.null(step$tool) && is.character(step$tool)) {
                    job_tools <- c(job_tools, step$tool)
                  }
                }
              }
            }
            job_tools <- unique(job_tools)

            if (length(job_tools) > 0) {
              missing_job_tools <- setdiff(job_tools, tool_names)
              if (length(missing_job_tools) > 0) {
                warning(
                  "Workflow '", workflow_name,
                  "' jobs reference missing tools: ",
                  paste(missing_job_tools, collapse = ", "),
                  ". Please update workflow YAML.",
                  call. = FALSE
                )
              }
            }
          }
        }

        if (verbose) {
          message("  Validated ", length(yaml_files), " workflow(s)")
        }
      }
    # }, error = function(e) {
    #   if (verbose) {
    #     message("  Workflow validation skipped: ", e$message)
    #   }
    # })
  } else {
    if (verbose) {
      message("  No workflows directory found")
    }
  }

  invisible(tools)
}

#' Load \verb{MCP} Tool Definitions from Package or Directory
#'
#' @param pkg Character. Package name or path to tools directory.
#'   If \code{pkg} contains path separators, space, or `.`, it is treated as a
#'   directory path. Otherwise, it is treated as a package name.
#' @param groups Character vector or NULL. Optional group filter(s)
#'   to load only tools from specific group(s). Groups are extracted
#'   from tool names following the pattern
#'   \code{pkg-mcp_tool_{group}_{action}}. Each group can be a regexp
#'   pattern. For example, "pipeline", "config", or
#'   "(pipeline|config)". If \code{NULL} (default), all tools are
#'   loaded.
#' @return List of \verb{MCP} tool definitions
#' @examples
#'
#' # Load all tools from package
#' tools <- mcptool_load_all("tricobbler")
#'
#' # Load only pipeline group tools
#' pipeline_tools <- mcptool_load_all("tricobbler", groups = "pipeline")
#'
#' # Load multiple groups
#' tools <- mcptool_load_all("tricobbler", groups = c("pipeline", "config"))
#'
#' # Load from directory
#' path <- system.file("mcp", "tools", package = "tricobbler")
#' tools <- mcptool_load_all(path)
#'
#' @export
mcptool_load_all <- function(pkg, groups = NULL) {
  # Detect if pkg is a path or package name
  # Paths contain /, \, space, or .
  is_path <- grepl("[/\\\\ .]", pkg)

  if (is_path) {
    # Treat as directory path
    tools_dir <- pkg
    if (!dir.exists(tools_dir)) {
      warning("Tools directory not found: '", tools_dir, "'")
      return(list())
    }
  } else {
    # Treat as package name
    pkg_path <- system.file(package = pkg)
    if (pkg_path == "") {
      stop("Package '", pkg, "' not found")
    }

    tools_dir <- file.path(pkg_path, "mcp", "tools")
    if (!dir.exists(tools_dir)) {
      warning("No MCP tools found in package '", pkg, "'")
      return(list())
    }
  }

  # Load all YAML files
  yaml_files <- list.files(tools_dir, pattern = "\\.yaml$", full.names = TRUE)

  # Filter by group if specified
  if (!is.null(groups)) {
    # Construct regexp pattern: ^[a-zA-Z0-9]+-mcp_tool_{group}
    # group can be regexp itself, so we just insert it
    yaml_files <- lapply(groups, function(group) {
      group_pattern <- sprintf("^[a-zA-Z0-9]+-mcp_tool_%s", group)

      # Filter files matching the group pattern
      yaml_files[grepl(group_pattern, basename(yaml_files))]
    })

    yaml_files <- sort(unique(unlist(yaml_files)))
  }

  if (length(yaml_files) == 0) {
    return(list())
  }

  tools <- lapply(yaml_files, function(f) {
    tryCatch(
      {
        mcptool_read(f)
      },
      error = function(e) {
        warning(
          "Failed to load MCP tool from ", basename(f), ": ",
          e$message,
          call. = FALSE
        )
        NULL
      }
    )
  })

  # Remove NULL entries
  tools <- tools[!vapply(tools, is.null, logical(1))]

  # Set names
  names(tools) <- vapply(tools, function(t) t$name %||% "", character(1))

  tools
}

#' Read \verb{MCP} tool definition from \code{YAML} file
#'
#' @description
#' Reads and parses a \verb{MCP} tool definition from a \code{YAML} file.
#' This is a low-level function used internally by \code{mcptool_load_all}
#' and \code{mcptool_path}.
#'
#' @param path Character. Path to the `YAML` file containing the
#'   tool definition.
#' @param check Logical. Whether to check if the function definition exists;
#'    default is \code{FALSE}
#' @param resolve Logical. Whether to try resolving the path, allowing the
#'    package tools to be loaded with `package-function_name` syntax; default
#'    if \code{TRUE}; set to \code{FALSE} if \code{path} should be treated
#'    as a file path without resolving.
#'
#' @return List containing the parsed tool definition.
#'   Throws an error if file cannot be read or parsed.
#'
#' @examples
#'
#' # Read directly from package using MCP tool name
#' mcptool_read("tricobbler-mcp_tool_search_packages")
#'
#'
#' path <- system.file(
#'   "mcp", "tools", "tricobbler-mcp_tool_search_packages.yaml",
#'   package = "tricobbler"
#' )
#'
#' tool_def <- mcptool_read(path, resolve = FALSE)
#'
#' # Check tool name and description
#' tool_def$name
#' tool_def$description
#'
#' @export
mcptool_read <- function(path, check = FALSE, resolve = TRUE) {
  if (resolve) {
    if (grepl("/|\\\\", path) || file.exists(path)) {
      path <- normalizePath(path, mustWork = FALSE)
      tools_dir <- dirname(path)
      tool_name <- gsub("\\.yaml$", "", basename(path))
      path <- mcptool_resolve_path(tool_name, tools_dir = tools_dir)
    } else {
      path <- mcptool_resolve_path(gsub("\\.yaml$", "", path))
    }
  }

  if (!file.exists(path)) {
    stop("Tool file not found: '", path, "'")
  }

  tool_def <- tryCatch(
    {
      yaml::read_yaml(path)
    },
    error = function(e) {
      stop(
        "Failed to parse tool YAML at '", path, "': ",
        e$message,
        call. = FALSE
      )
    }
  )
  # check if the function exists
  if (check) {
    mcptool_seek_function(tool_def)
  }

  # Add class
  class(tool_def) <- c("tricobbler_mcp_tool", "list")
  tool_def
}

mcptool_seek_function <- function(tool) {
  # check if the function exists
  if (grepl("^[^\\-]+\\-.+", tool$name)) {
    # this is package function
    tool_source <- strsplit(tool$name, "-", fixed = TRUE)[[1]]
    pkg <- tool_source[[1]]
    fname <- paste(tool_source[-1], collapse = "-")
    f <- asNamespace(pkg)[[fname]]
    if (!is.function(f)) {
      stop(pkg, ":::", fname, " is not a function")
    }
    return(f)
  }
  stop("Custom non-package defined MCP tool has not yet been implemented")
  # f <- trimws(paste(tool$impl, collapse = "\n"))
  # if (nzchar(f)) {
  #   f <- parse(text = f)
  # }
  # if (!is.function(f)) {
  #   stop("Custom MCP tool does not have ")
  # }
  # f
}

#' @export
format.tricobbler_mcp_tool <- function(
  x,
  method = c("summary", "yaml", "markdown"),
  ...
) {
  method <- match.arg(method)

  switch(
    method,
    "summary" = {
      # Summary format
      lines <- character()
      lines <- c(lines, sprintf("RAVE MCP Tool: %s", x$name %||% "<unnamed>"))

      if (!is.null(x$description) && nzchar(x$description)) {
        desc_lines <- strwrap(x$description, width = 70, prefix = "  ")
        lines <- c(lines, desc_lines)
      }

      # Parameters
      if (!is.null(x$parameters) && !is.null(x$parameters$properties)) {
        n_params <- length(x$parameters$properties)
        lines <- c(lines, sprintf("Parameters: %d", n_params))

        required_params <- x$parameters$required
        if (is.null(required_params)) {
          required_params <- character(0)
        }

        for (param_name in names(x$parameters$properties)) {
          param <- x$parameters$properties[[param_name]]
          is_required <- param_name %in% required_params
          req_marker <- if (is_required) "[required]" else "[optional]"

          type_str <- param$type %||% "unknown"
          if (!is.null(param$items) && !is.null(param$items$type)) {
            type_str <- sprintf("%s<%s>", type_str, param$items$type)
          }

          lines <- c(
            lines,
            sprintf(
              "  - %s %s (%s)",
              param_name,
              req_marker,
              type_str
            )
          )
        }
      }

      # Metadata
      if (!is.null(x$category)) {
        lines <- c(lines, sprintf("Category: %s", x$category))
      }
      if (isTRUE(x$dangerous)) {
        lines <- c(lines, "Dangerous: yes")
      }
      if (isTRUE(x$requires_approval)) {
        lines <- c(lines, "Requires approval: yes")
      }

      # Return type
      if (!is.null(x$returns)) {
        return_type <- x$returns$type %||% "unknown"
        lines <- c(lines, sprintf("Returns: %s", return_type))
      }

      paste(lines, collapse = "\n")
    },
    "yaml" = {
      # YAML format
      yaml::as.yaml(unclass(x))
    },
    "markdown" = {
      # Markdown format
      lines <- character()
      lines <- c(lines, sprintf("# %s", x$name %||% "<unnamed>"))
      lines <- c(lines, "")

      if (!is.null(x$description) && nzchar(x$description)) {
        lines <- c(lines, x$description)
        lines <- c(lines, "")
      }

      # Parameters section
      if (!is.null(x$parameters) && !is.null(x$parameters$properties)) {
        lines <- c(lines, "## Parameters")
        lines <- c(lines, "")

        required_params <- x$parameters$required
        if (is.null(required_params)) {
          required_params <- character(0)
        }

        for (param_name in names(x$parameters$properties)) {
          param <- x$parameters$properties[[param_name]]
          is_required <- param_name %in% required_params

          type_str <- param$type %||% "unknown"
          if (!is.null(param$items) && !is.null(param$items$type)) {
            type_str <- sprintf("%s<%s>", type_str, param$items$type)
          }

          req_marker <- if (is_required) "**required**" else "*optional*"
          lines <- c(
            lines,
            sprintf(
              "- **%s** (%s, %s)",
              param_name,
              type_str,
              req_marker
            )
          )

          if (!is.null(param$description) && nzchar(param$description)) {
            desc_lines <- strwrap(param$description, width = 70, prefix = "  ")
            lines <- c(lines, desc_lines)
          }

          if (!is.null(param$default)) {
            default_str <- if (is.character(param$default)) {
              sprintf('"%s"', param$default)
            } else {
              as.character(param$default)
            }
            lines <- c(lines, sprintf("  Default: %s", default_str))
          }

          if (!is.null(param$enum)) {
            enum_str <- paste(
              vapply(
                param$enum,
                function(e) {
                  if (is.character(e)) sprintf('"%s"', e) else as.character(e)
                },
                character(1)
              ),
              collapse = ", "
            )
            lines <- c(lines, sprintf("  Options: %s", enum_str))
          }

          lines <- c(lines, "")
        }
      }

      # Returns section
      if (!is.null(x$returns)) {
        lines <- c(lines, "## Returns")
        lines <- c(lines, "")

        return_type <- x$returns$type %||% "unknown"
        lines <- c(lines, sprintf("Type: `%s`", return_type))

        if (!is.null(x$returns$description) && nzchar(x$returns$description)) {
          lines <- c(lines, "")
          lines <- c(lines, x$returns$description)
        }

        if (!is.null(x$returns$properties)) {
          lines <- c(lines, "")
          lines <- c(lines, "### Properties")
          lines <- c(lines, "")

          for (prop_name in names(x$returns$properties)) {
            prop <- x$returns$properties[[prop_name]]
            prop_type <- prop$type %||% "unknown"
            prop_desc <- prop$description %||% ""
            lines <- c(
              lines,
              sprintf("- `%s` (%s): %s", prop_name, prop_type, prop_desc)
            )

            if (!is.null(prop$description) && nzchar(prop$description)) {
              desc_lines <- strwrap(prop$description, width = 70, prefix = "  ")
              lines <- c(lines, desc_lines)
            }
            lines <- c(lines, "")
          }
        }
      }

      # Metadata section
      metadata <- character()
      if (!is.null(x$category)) {
        metadata <- c(metadata, sprintf("- **Category**: %s", x$category))
      }
      if (isTRUE(x$dangerous)) {
        metadata <- c(metadata, "- **Dangerous**: yes")
      }
      if (isTRUE(x$requires_approval)) {
        metadata <- c(metadata, "- **Requires approval**: yes")
      }

      if (length(metadata) > 0) {
        lines <- c(lines, "## Metadata")
        lines <- c(lines, "")
        lines <- c(lines, metadata)
        lines <- c(lines, "")
      }

      paste(lines, collapse = "\n")
    }
  )
}

#' @export
print.tricobbler_mcp_tool <- function(
  x,
  method = c("summary", "yaml", "markdown"),
  ...
) {
  method <- match.arg(method)
  cat(format(x, method = method, ...), "\n")
  invisible(x)
}

#' Write \verb{MCP} tool definition to file
#'
#' @description
#' Writes a \verb{MCP} tool definition to a file in either \code{YAML}
#' or \code{Markdown} format.
#'
#' @param tool A \code{tricobbler_mcp_tool} object
#' @param path Character. Path to the output file
#' @param method Character. Output format: \code{"yaml"} (default) or
#' \code{"markdown"}
#' @param ... Additional arguments (currently unused)
#'
#' @return Invisibly returns the input tool object
#'
#' @examples
#'
#' # Load a tool from package
#' path <- mcptool_path("tricobbler-mcp_tool_search_packages")
#' tool <- mcptool_read(path)
#'
#' # Write as YAML to temporary file
#' mcptool_write(tool, stdout(), method = "yaml")
#'
#' # Write as Markdown
#' mcptool_write(tool, stdout(), method = "markdown")
#'
#' @export
mcptool_write <- function(tool, path, method = c("yaml", "markdown"), ...) {
  method <- match.arg(method)

  if (!inherits(tool, "tricobbler_mcp_tool")) {
    stop("tool must be a tricobbler_mcp_tool object")
  }

  switch(
    method,
    "yaml" = {
      # Write as YAML using save_yaml
      save_yaml(x = tool, file = path)
    },
    "markdown" = {
      # Write as Markdown using format method
      formatted <- format(tool, method = "markdown")
      writeLines(formatted, con = path)
    }
  )

  invisible(tool)
}

#' List \verb{MCP} tool names from a package or directory
#'
#' @description
#' Lists all available \verb{MCP} tool names from a package or directory.
#' This is a lightweight wrapper around \code{mcptool_load_all} that returns
#' only the tool names without loading full definitions.
#'
#' @param pkg Character. Package name or path to tools directory.
#'   If \code{pkg} contains path separators, space, or \code{'.'}, it is
#'   treated as a directory path. Otherwise, it is treated as a package name.
#' @param groups Character vector or NULL. Optional group filter(s)
#'   to list only tools from specific group(s). Groups are extracted
#'   from tool names following the pattern
#'   \code{pkg-mcp_tool_{group}_{action}}. Each group can be a regexp
#'   pattern. If \code{NULL} (default), all tools are listed.
#'
#' @return Character vector of tool names (in format `pkg-function_name`).
#'   Returns empty character vector if no tools found.
#'
#' @examples
#'
#' # List all tools from package
#' mcptool_list("tricobbler")
#'
#' # List only pipeline group tools
#' mcptool_list("tricobbler", groups = "pipeline")
#'
#' # List multiple groups
#' mcptool_list("tricobbler", groups = c("pipeline", "config"))
#'
#' # Or list tools from directory
#' path <- system.file("mcp", "tools", package = "tricobbler")
#' mcptool_list(path)
#'
#' @export
mcptool_list <- function(pkg, groups = NULL) {
  tools <- mcptool_load_all(pkg, groups = groups)
  names(tools)
}


# Helper function to format returns section from YAML
#' @keywords internal
#' @noRd
format_returns_section <- function(returns_def) {
  if (is.null(returns_def)) return("")

  # If returns is a simple string, return it
  if (is.character(returns_def) && length(returns_def) == 1) {
    return(returns_def)
  }

  # If returns has properties (object type), format them
  lines <- character()

  if (!is.null(returns_def$type)) {
    lines <- c(lines, paste0("Type: `", returns_def$type, "`"))
  }

  if (!is.null(returns_def$description)) {
    lines <- c(lines, paste0("\n", returns_def$description))
  }

  if (!is.null(returns_def$properties)) {
    lines <- c(lines, "\n\nProperties:")
    for (prop_name in names(returns_def$properties)) {
      prop <- returns_def$properties[[prop_name]]
      prop_type <- prop$type %||% "unknown"
      prop_desc <- prop$description %||% ""
      lines <- c(
        lines,
        paste0(
          "- `", prop_name, "` (", prop_type, "): ", prop_desc
        )
      )
    }
  }

  paste(lines, collapse = "\n")
}

mcptool_resolve_path <- function(tool_name, tools_dir = "../tools") {
  if (!is.character(tool_name) || length(tool_name) != 1 ||
      !nzchar(tool_name)) {
    stop("tool_name must be a non-empty character string")
  }

  # Check if tool_name is in pkg-function format
  # We look for at least one hyphen separating package and function
  has_pkg_prefix <- grepl("^[^\\-]+\\-.+", tool_name)

  if (has_pkg_prefix) {
    # Parse tool name
    parts <- strsplit(tool_name, "-", fixed = TRUE)[[1]]

    pkg_name <- trimws(parts[1])
    func_name <- paste(trimws(parts[-1]), collapse = "-")

    if (!nzchar(pkg_name) || !nzchar(func_name)) {
      stop("Tool name '", tool_name, "' has empty package or function name")
    }

    # Normalize tool name to use - internally
    normalized_tool_name <- paste0(pkg_name, "-", func_name)

    # Convert tool name to filename using same convention
    # as mcptool_build
    tool_filename <- paste0(
      gsub("[^a-zA-Z0-9_-]+", "-", normalized_tool_name),
      ".yaml"
    )

    # Strategy 1: Try finding in package
    pkg_path <- tryCatch({
      system.file(package = pkg_name)
    }, error = function(e) {
      ""
    })

    if (nzchar(pkg_path)) {
      pkg_tool_path <- file.path(pkg_path, "mcp", "tools", tool_filename)

      if (file.exists(pkg_tool_path)) {
        return(pkg_tool_path)
      }
    }

    # Strategy 2: Try local directory with normalized filename
    if (!is.null(tools_dir) && nzchar(tools_dir) && dir.exists(tools_dir)) {
      tool_path <- file.path(tools_dir, tool_filename)
      if (file.exists(tool_path)) {
        return(tool_path)
      }
    }

    # Tool not found
    error_msg <- paste0(
      "Tool '", tool_name, "' not found.\n",
      "  - Package '", pkg_name, "' lookup: ",
      local({
        if (!nzchar(pkg_path)) {
          "package not installed"
        } else {
          paste0(
            "tool file not found in package ('",
            file.path(pkg_path, "mcp", "tools"), "')"
          )
        }
      }),
      "\n  - Local directory lookup: ",
      local({
        if (is.null(tools_dir) || !nzchar(tools_dir)) {
          "skipped (no tools_dir specified)"
        } else if (!dir.exists(tools_dir)) {
          paste0("directory not found ('", tools_dir, "')")
        } else {
          paste0("tool file not found in '", tools_dir, "'")
        }
      })
    )
    stop(error_msg)

  } else {
    # Simple tool name - look up from local directory only
    if (is.null(tools_dir) || !nzchar(tools_dir)) {
      stop(
        "Tool '", tool_name,
        "' requires tools_dir to be specified (no package prefix)"
      )
    }

    if (!dir.exists(tools_dir)) {
      stop("Local tools directory not found: '", tools_dir, "'")
    }

    tool_filename <- tool_name
    if (!grepl("\\.yaml", tool_name, ignore.case = TRUE)) {
      tool_filename <- paste0(tool_name, ".yaml")
    }

    tool_path <- file.path(tools_dir, tool_filename)

    if (!file.exists(tool_path)) {
      stop("Tool '", tool_name, "' not found at '", tool_path, "'")
    }

    return(tool_path)
  }
}

#' Find path to a specific \verb{MCP} tool by name
#'
#' @description
#' Locates a \verb{MCP} tool YAML file from either an installed package
#' or a local directory. Tool names use `pkg-function_name` format where
#' the first hyphen separates the package name from the function name.
#' Returns the file path with the tool definition attached as an
#' attribute.
#'
#' @param tool_name Character. Tool name in format `pkg-function_name`.
#'   The first hyphen separates package name from function name
#'   (function names may contain hyphens).
#' @param tools_dir Character. Path to root directory for local tools
#'   look-up. Default is \code{"../tools"} (relative to workflows
#'   directory).
#'
#' @return Character string containing the path to the tool YAML file,
#'   with attribute `mcp_definition` containing the parsed tool definition.
#'   Throws an error if tool not found.
#'
#' @examples
#'
#' # Find tool from package
#' tool_path <- mcptool_path("tricobbler-mcp_tool_search_packages")
#' attr(tool_path, "mcp_definition")
#'
#'
#' \dontrun{
#'
#' # Simple tool name (looks in tools_dir only)
#' tool_path <- mcptool_path("my_tool", tools_dir = "./tools")
#'
#' }
#'
#' @export
mcptool_path <- function(tool_name, tools_dir = "../tools") {
  tool_path <- mcptool_resolve_path(tool_name = tool_name, tools_dir = tools_dir) # nolint: line_length_linter.
  tool_def <- mcptool_read(tool_path)
  attr(tool_path, "mcp_definition") <- tool_def
  return(tool_path)
}



#' Instantiate from RAVE \verb{MCP} tool definition
#'
#' @description Create an \pkg{ellmer} tool object from a loaded RAVE
#'   \verb{MCP} tool definition. This allows using RAVE pipelines and
#'   tools directly via the \pkg{ellmer} package.
#'
#' @param tool An object of class \code{tricobbler_mcp_tool} (loaded via
#'   \code{\link{mcptool_read}} or \code{\link{mcptool_load_all}}).
#' @param ... Additional arguments passed to \code{\link[ellmer]{tool}}.
#'
#' @return An \code{\link[ellmer]{ToolDef}} object ready to be registered
#'  with a chat session.
#'
#' @export
mcptool_instantiate <- function(tool, ...) {

  if (!inherits(tool, "tricobbler_mcp_tool") && is.list(tool)) {
    tool_list <- list()
    for (sub_tool in tool) {
      stopifnot(inherits(sub_tool, "tricobbler_mcp_tool"))
      tool_list[[sub_tool$name]] <- Recall(tool = sub_tool, ...)
    }
    return(tool_list)
  }

  stopifnot(inherits(tool, "tricobbler_mcp_tool"))

  if (!requireNamespace("ellmer", quietly = TRUE)) {
    stop("Package 'ellmer' is required to instantiate MCP tool.")
  }

  # Resolve function
  # We assume the function is exported or available in the search path
  fun_name <- tool$name
  if (is.null(fun_name)) {
    stop("Tool definition is missing 'name' field.")
  }

  # obtain the function
  impl <- mcptool_seek_function(tool)

  # wrap the function so the return is always JSON stringified
  wrapper_fun <- function() {
    call <- match.call()
    call[[1]] <- quote(impl)
    res <- eval(call)
    res <- mcp_describe(res)
    res
  }
  fmls <- formals(impl)
  fml_names <- names(fmls)
  if (length(fml_names)) {
    fmls <- fmls[!startsWith(fml_names, ".")]
  }
  formals(wrapper_fun) <- fmls

  # Helper to map types to ellmer types
  map_type <- function(param_def) {
    type <- param_def$type %||% "string"
    desc <- param_def$description %||% ""

    # Base arguments for type functions
    # Note: ellmer type functions usually take description as first arg
    args <- list(description = desc)

    # Enum handling - use type_enum() for enum fields
    if (!is.null(param_def$enum)) {
      return(ellmer::type_enum(
        values = as.character(param_def$enum),
        description = desc
      ))
    }

    if (type == "string") {
      do.call(ellmer::type_string, args)
    } else if (type == "integer") {
      do.call(ellmer::type_integer, args)
    } else if (type == "number") {
      do.call(ellmer::type_number, args)
    } else if (type == "boolean") {
      do.call(ellmer::type_boolean, args)
    } else if (type == "array") {
      if (!is.null(param_def$items)) {
        # Recursive call for items
        item_def <- param_def$items
        if (is.null(item_def$description)) item_def$description <- ""
        args$items <- map_type(item_def)
      }
      do.call(ellmer::type_array, args)
    } else if (type == "object") {
      do.call(ellmer::type_object, args)
    } else {
      # Fallback to string
      do.call(ellmer::type_string, args)
    }
  }

  # Prepare arguments list
  arguments <- list()
  if (!is.null(tool$parameters) && !is.null(tool$parameters$properties)) {
    for (name in names(tool$parameters$properties)) {
      arguments[[name]] <- map_type(tool$parameters$properties[[name]])
    }
  }

  # Build rich description from YAML fields
  description_parts <- list()

  # Start with base description
  base_desc <- tool$description %||% ""
  if (nzchar(base_desc)) {
    description_parts <- c(description_parts, base_desc)
  }

  # Add category section
  if (!is.null(tool$category) && nzchar(tool$category)) {
    description_parts <- c(
      description_parts,
      paste0("\n\n## Category\n", tool$category)
    )
  }

  # Add implementation example section
  if (!is.null(tool$implementation_example) &&
      nzchar(tool$implementation_example)) {
    description_parts <- c(
      description_parts,
      paste0(
        "\n\n## Implementation Example\n\n```r\n",
        tool$implementation_example,
        "\n```"
      )
    )
  }

  # Add returns section
  if (!is.null(tool$returns)) {
    returns_text <- format_returns_section(tool$returns)
    if (nzchar(returns_text)) {
      # Determine output format description
      output_format <- tool$output_format %||% "text"
      if (output_format == "json") {
        format_desc <- "**Output format**: JSON"
      } else {
        format_desc <- paste0(
          "**Output format**: R console output ",
          "(via `tricobbler::mcp_describe()`), not JSON."
        )
      }
      description_parts <- c(
        description_parts,
        paste0(
          "\n\n## Returns\n\n", format_desc,
          "\n\nThe output represents the following structure:\n\n",
          returns_text
        )
      )
    }
  }

  # Combine all parts into final description
  final_description <- paste(description_parts, collapse = "")

  # Prepare MCP protocol hints for tool_annotations()
  annotation_args <- list()

  # Map our custom fields to MCP hints where applicable
  if (!is.null(tool$dangerous) && isTRUE(tool$dangerous)) {
    annotation_args$destructive_hint <- TRUE
  }

  # Keep only flag-type fields in annotations
  flag_fields <- c("requires_approval", "execution_time")

  for (field in flag_fields) {
    if (!is.null(tool[[field]])) {
      annotation_args[[field]] <- tool[[field]]
    }
  }

  # Create tool_annotations object (always create it, even if empty)
  annotations <- do.call(ellmer::tool_annotations, annotation_args)

  ellmer::tool(
    fun = wrapper_fun,
    description = final_description,
    arguments = arguments,
    name = gsub("[^a-zA-Z0-9_-]+", "-", fun_name),
    annotations = annotations,
    ...
  )
}

r_to_mcp_output <- function(x, max_size = 20480) {
  return(mcp_describe(x, max_print = max_size))
}

