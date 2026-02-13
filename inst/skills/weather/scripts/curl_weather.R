#' @description Fetch weather from wttr.in
#' ... <- This can be defined as tricobbler MCP style documentaion
#' The function name is the agent ID
#'
#' @details
#' The \code{location} parameter can be provided in three ways
#' (highest to lowest precedence):
#' \enumerate{
#'   \item Dependency: if a previous agent produces \code{location}
#'     via \code{depends_on}.
#'   \item State-level: set in \code{StatePolicy@@parameters$args}.
#'   \item Global: set on \code{MasterPolicy@@parameters} and declared
#'     in \code{global_parameters:} of \code{tricobbler.yaml}. The
#'     runtime cascades global parameters automatically when the
#'     state-level value is absent.
#' }
#'
#' Agents can also retrieve parameters explicitly via
#' \code{.runtime$get_parameter("location")} for finer control.
curl_weather <- function(location, .runtime = NULL) {
  # DEBUG: .runtime is an instance of AgentRuntime
  # or we need to find ways to sneak runtime into agent function
  if (is.null(.runtime)) {
    logger <- message
  } else {
    logger <- .runtime$logger
    if (missing(location) || inherits(location, "error")) {
      location <- .runtime$get_parameter("location")
    }
  }
  logger(sprintf("Fetching weather for '%s' from wttr.in...", location))

  # Call wttr.in for JSON output
  url <- sprintf("https://wttr.in/%s?format=j1",
                 utils::URLencode(location))
  response <- jsonlite::fromJSON(url, simplifyVector = TRUE)

  # Extract the key data from wttr.in response
  current <- response$current_condition
  if (is.data.frame(current)) {
    current <- as.list(current[1L, ])
  } else if (is.list(current)) {
    current <- current[[1L]]
  }

  list(
    source = "wttr.in",
    location = location,
    current = list(
      temp_c = current$temp_C,
      temp_f = current$temp_F,
      feels_like_c = current$FeelsLikeC,
      feels_like_f = current$FeelsLikeF,
      condition = if (is.data.frame(current$weatherDesc))
        current$weatherDesc$value[1L]
      else if (is.list(current$weatherDesc))
        current$weatherDesc[[1L]]$value
      else
        as.character(current$weatherDesc),
      humidity = current$humidity,
      wind_mph = current$windspeedMiles,
      wind_dir = current$winddir16Point,
      visibility = current$visibility,
      uv_index = current$uvIndex
    )
  )

}

# ------------------------------------------------------------------
# CLI entry-point (docopt)
# Guard: sys.nframe() == 0 means top-level (Rscript), not source()'d
# ------------------------------------------------------------------
if (sys.nframe() == 0L) {

  doc <- "Fetch current weather from wttr.in.

Usage:
  curl_weather.R <location>
  curl_weather.R --help

Arguments:
  <location>  City name or location string (e.g., 'Boston', 'London')

Options:
  --help  Show this help message.
"

  args <- tricobbler::docopt(doc)
  result <- curl_weather(location = args$location)
  cat(jsonlite::toJSON(result, auto_unbox = TRUE, pretty = TRUE))
  cat("\n")
}
