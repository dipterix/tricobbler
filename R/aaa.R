concatern <- function(x, collapse = "\n", trim_lines = FALSE, trim_collapsed = TRUE) {
  x <- unlist(x)
  if(trim_lines) {
    x <- trimws(x)
  }
  x <- paste(x, collapse = collapse)
  if(trim_collapsed) {
    x <- trimws(x)
  }
  x
}
