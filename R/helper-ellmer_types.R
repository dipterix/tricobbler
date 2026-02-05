# ---------------------------------------------------------------------------
# Helper: map_type_to_ellmer
# ---------------------------------------------------------------------------
#' Convert Type Definitions to ellmer Type Indicators
#'
#' @description Converts JSON Schema type definitions (from YAML or lists) to
#'   \pkg{ellmer} type indicator objects for use with
#'   \code{chat$chat_structured()}.
#'
#' @param type_def A list representing a JSON Schema type definition, or a
#'   character string specifying a simple type (e.g., "string", "integer").
#'   Supported types: "string", "integer", "number", "boolean", "array",
#'   "object". Arrays can specify \code{items} for element types. Objects can
#'   specify \code{properties} for nested structure.
#'
#' @return An \pkg{ellmer} type indicator object (e.g.,
#'   \code{ellmer::type_string()}, \code{ellmer::type_object()}).
#'
#' @examples
#' \dontrun{
#' # Simple string type
#' map_type_to_ellmer("string")
#'
#' # Complex object type from YAML-like definition
#' map_type_to_ellmer(list(
#'   type = "object",
#'   properties = list(
#'     name = list(type = "string", description = "User name"),
#'     age = list(type = "integer", description = "User age")
#'   )
#' ))
#' }
#'
#' @seealso \code{\link{StatePolicy}} for how \code{return_type} uses this
#'   function, \code{\link[ellmer]{type_string}} and related functions for
#'   \pkg{ellmer} type indicators
#' @export
map_type_to_ellmer <- function(type_def) {
  # Handle character shorthand (e.g., "string", "integer")
  if (is.character(type_def)) {
    type_def <- list(type = type_def)
  }

  if (!is.list(type_def)) {
    stop("`type_def` must be a list or character string")
  }

  type <- type_def$type %||% "string"
  desc <- type_def$description %||% ""

  # Handle enum types first (special case)
  if (!is.null(type_def$enum)) {
    return(ellmer::type_enum(
      values = as.character(type_def$enum),
      description = desc
    ))
  }

  # Handle required field (defaults to TRUE)
  required <- type_def$required %||% TRUE

  # Map JSON Schema types to ellmer types
  switch(
    type,
    "string" = ellmer::type_string(description = desc, required = required),
    "integer" = ellmer::type_integer(description = desc, required = required),
    "number" = ellmer::type_number(description = desc, required = required),
    "boolean" = ellmer::type_boolean(description = desc, required = required),
    "array" = {
      args <- list(description = desc, required = required)
      if (!is.null(type_def$items)) {
        item_def <- type_def$items
        if (is.null(item_def$description)) {
          item_def$description <- ""
        }
        args$items <- map_type_to_ellmer(item_def)
      } else {
        # Default to string items if not specified

        args$items <- ellmer::type_string()
      }
      do.call(ellmer::type_array, args)
    },
    "object" = {
      # Build named properties for type_object
      props <- list()
      if (!is.null(type_def$properties)) {
        for (prop_name in names(type_def$properties)) {
          prop_def <- type_def$properties[[prop_name]]
          if (is.null(prop_def$description)) {
            prop_def$description <- ""
          }
          props[[prop_name]] <- map_type_to_ellmer(prop_def)
        }
      }
      # type_object uses .description and ... for properties
      do.call(ellmer::type_object, c(
        list(.description = desc, .required = required),
        props
      ))
    },
    # Fallback to string for unknown types
    ellmer::type_string(description = desc, required = required)
  )
}
