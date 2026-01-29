property_enum <- function(...) {
  enum <- as.character(unlist(c(...)))
  stopifnot(length(enum) > 0)
  S7::new_property(
    class = S7::class_character,
    validator = function(value) {
      if (length(value) != 1) {
        return("length of value must be 1")
      }
      if (!isTRUE(value %in% enum)) {
        return(sprintf(
          "invalid value %s: available choices are: %s",
          sQuote(value),
          paste(sQuote(enum), collapse = ", ")
        ))
      }
    },
    default = enum[[1]]
  )
}

property_version <- function(...) {
  S7::new_property(
    class = S7::class_character,
    validator = function(value) {
      v <- package_version(value, strict = FALSE)
      if(length(v) != 1 || is.na(v)) {
        return(sprintf("invalid version specification %s", sQuote(version)))
      }
      return(NULL)
    },
    ...
  )
}
