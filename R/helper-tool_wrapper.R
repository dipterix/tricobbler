# sanitize_string_arg <- function(x) {
#   if (length(x) == 1 && is.character(x) && startsWith(x, '["')) {
#     x <- jsonlite::fromJSON(x)
#   }
#   x
# }
#
# convert_from_type <- function(x, type) {
#   if (length(x) == 1 && is.character(x) && !is.na(x)) {
#     x <- trimws(x)
#     if (grepl("^<[a-zA-Z0-9].*>", x)) {
#       message(x)
#       stop("NO XML tag accepted. Use pure JSON!")
#     }
#   }
#   call_pkg_fun("ellmer", "convert_from_type", x = x, type = type)
# }


tool2 <- function(
    fun,
    description,
    ...,
    arguments = list(),
    name = NULL,
    convert = TRUE,
    annotations = list()) {

  # Filter out TypeIgnore entries: they are meant to hide args from the LLM,

  # not to be serialized as JSON schema properties. ellmer::tool() strips them
  # at the top level, but tool2() nests all arguments inside a TypeObject
  # (json_args), so TypeIgnore would survive and cause as_json() to fail
  # for providers that lack an as_json(Provider, TypeIgnore) method (e.g. Ollama).
  arguments <- Filter(
    function(x) !S7::S7_inherits(x, ellmer::TypeIgnore),
    arguments
  )

  required <- length(arguments) > 0
  json_args_def <- do.call(ellmer::type_object, c(arguments, list(.required = required)))

  fml_names <- names(formals(fun))

  wrapper_fun <- function(json_args = "{}", ...) {

    print(json_args)

    if (!is.character(json_args)) {
      json_args_str <- jsonlite::toJSON(json_args, auto_unbox = TRUE)
    } else {
      json_args_str <- json_args
    }

    args <- tryCatch(
      {
        # message(json_args)
        json_args <- jsonlite::fromJSON(json_args_str, simplifyVector = TRUE)

        args <- convert_from_type(
          x = json_args,
          type = json_args_def
        )
        if (!is.list(args)) {
          args <- as.list(args)
        }

        args_dots <- list(...)
        nms_dots <- names(args_dots)

        if (length(nms_dots)) {
          nms_dots <- nms_dots[startsWith(nms_dots, ".") & !nms_dots %in% names(args)]
          if (length(nms_dots) > 0) {
            args[nms_dots] <- args_dots[nms_dots]
          }
        }

        if (!"..." %in% fml_names) {
          args <- args[names(args) %in% fml_names]
        }

        # print(as.call(c(list(str2lang(tl@name)), args)))

        # print(args)
        result <- do.call(fun, args)
        # print(result)
        result

      },
      error = function(e) {

        reason <- sprintf(
          "Calling tool `%s` with argument\n\n```json\n%s\n```\n\n results in error: %s",
          tl@name,
          json_args_str,
          paste(conditionMessage(e), collapse = "\n")
        )
        ellmer::tool_reject(reason = reason)

        stop(e)

      }
    )

  }

  tl <- ellmer::tool(
    fun = wrapper_fun,
    description = paste(description, collapse = "\n"),
    ...,
    arguments = list(
      json_args = json_args_def,
      "..." = ellmer::type_ignore()
    ),
    name = name,
    convert = TRUE,
    annotations = annotations
  )
  tl
}
