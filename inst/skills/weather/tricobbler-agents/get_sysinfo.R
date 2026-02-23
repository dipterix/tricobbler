get_sysinfo <- function() {
  geo_info <- list(
    tz = Sys.timezone(),
    locale = Sys.getlocale("LC_TIME")
  )
  tz <- Sys.timezone()
  locale <- Sys.getlocale("LC_TIME")
  ip <- tryCatch(
    trimws(readLines("https://api.ipify.org", warn = FALSE)),
    error = function(e) "unknown"
  )
  tryCatch(
    {
      json <- trimws(readLines(sprintf("http://ip-api.com/json/%s", ip), warn = FALSE))
      info <- jsonlite::fromJSON(json)
      nms <- c("country", "countryCode", "region", "regionName", "city", "zip")
      geo_info[nms] <- info[nms]
    },
    error = function(e) "unknown"
  )

  geo_info
}
