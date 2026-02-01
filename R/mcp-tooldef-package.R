#' List Available Help Topics for R Package
#'
#' @description
#' Get a list of all available help topics (functions, classes, datasets, etc.)
#' documented in a specific R package. Returns topic names with their titles
#' to help discover what documentation is available.
#'
#' @param package_name Character string, the exact name of the package to list
#'   help topics from (e.g., \code{"tricobbler"}, \code{"base"}, 
#'   \code{"utils"}). This parameter is required.
#'
#' @return A list containing:
#' \describe{
#'   \item{success}{Logical, whether help topics were retrieved successfully}
#'   \item{package}{Character, the package name that was queried}
#'   \item{topics}{Data frame with columns \code{topic} (topic name) and
#'     \code{title} (brief description), or list of topic information}
#'   \item{count}{Integer, total number of help topics found}
#'   \item{error}{Character, error message if success is FALSE}
#' }
#'
#' @examples
#'
#' # Example output structure from calling the tool
#' tricobbler:::mcp_tool_docs_package_help_topics("utils")
#'
#' @keywords mcp-tool mcp-category-docs
#' @noRd
mcp_tool_docs_package_help_topics <- function(package_name) {
  # Validate input
  if (missing(package_name) || !is.character(package_name) ||
      length(package_name) != 1 || package_name == "") {
    return(list(
      success = FALSE,
      error = "package_name parameter is required and must be a non-empty string" # nolint: line_length_linter.
    ))
  }

  # Check if package exists
  if (system.file(package = package_name) == "") {
    return(list(
      success = FALSE,
      package = package_name,
      error = sprintf(
        "Package '%s' is not installed or not available",
        package_name
      )
    ))
  }

  # Get help database for package
  help_db <- utils::help.search(".", package = package_name, rebuild = FALSE)

  if (length(help_db$matches) == 0 || nrow(help_db$matches) == 0) {
    return(list(
      success = TRUE,
      package = package_name,
      topics = data.frame(topic = character(0), title = character(0)),
      count = 0,
      warning = sprintf("No help topics found in package '%s'", package_name)
    ))
  }

  # Extract topics and titles
  topics <- data.frame(
    topic = as.character(help_db$matches[, "Topic"]),
    title = as.character(help_db$matches[, "Title"]),
    stringsAsFactors = FALSE
  )

  list(
    success = TRUE,
    package = package_name,
    topics = topics,
    count = nrow(topics)
  )
}

# MCP Tool Implementation Functions - Package Documentation
# These functions are called by MCP tools to access R package documentation
# Each function is documented with \pkg{roxygen2} tags that are used to generate
# the tool specifications in `inst/mcp`

#' Get R Package Help Page
#'
#' @description
#' Retrieve and format help documentation for an R function or topic from
#' a specific package or search across all packages. Returns the help content
#' in human-readable text format suitable for AI assistance.
#'
#' @param topic Character string, the name of the help topic or function to
#'   look up. This can be a function name, class name, or any documented topic.
#'   Examples: \code{"pipeline"}, \code{"PipelineTools"}, \code{"mcp_describe"}.
#' @param package_name Character string, the exact name of the package to search
#'   in (e.g., \code{"tricobbler"}). If empty string \code{""} (default),
#'   searches across all loaded packages. Specifying the package name is
#'   recommended for faster and more accurate results.
#'   Examples: \code{"tricobbler"}, \code{"base"}, \code{"utils"}.
#'
#' @return A list containing:
#' \describe{
#'   \item{success}{Logical, whether the help page was found and retrieved}
#'   \item{topic}{Character, the topic that was searched for}
#'   \item{package}{Character, the package where the topic was found 
#'         (if specified or discovered)}
#'   \item{content}{Character, formatted help documentation as plain text}
#'   \item{error}{Character, error message if success is FALSE}
#' }
#'
#' @examples
#'
#' # Example output from calling the tool
#' res <- tricobbler:::mcp_tool_docs_help_page("mean", "base")
#' strsplit(gsub("[^a-zA-Z0-9 .\n\t-]+", "", res$content), "\n")[[1]]
#'
#' @keywords mcp-tool mcp-category-docs
#' @noRd
mcp_tool_docs_help_page <- function(topic, package_name = "") {
  # Validate inputs
  if (
    missing(topic) || !is.character(topic) || length(topic) != 1 || topic == ""
  ) {
    return(list(
      success = FALSE,
      error = "topic parameter is required and must be a non-empty string"
    ))
  }

  if (!is.character(package_name) || length(package_name) != 1) {
    return(list(
      success = FALSE,
      error = "package_name must be a single character string (use empty string to search all packages)" # nolint: line_length_linter.
    ))
  }

  if (package_name == "") {
    if (grepl("[:]+", topic)) {
      topic <- strsplit(topic, "[:]+", perl = TRUE)[[1]]
      package_name <- topic[[1]]
      topic <- topic[[2]]
    } else {
      # find from all loaded packages
      db <- utils::help.search("^pipeline$", fields = c("alias", "concept"))
      if (!length(db$matches)) {
        return(list(
          success = FALSE,
          error = sprintf(
            "unable to find the topic `%s`. Please try <package:function> format or pass package argument explicitly", # nolint: line_length_linter.
            topic
          )
        ))
      }
      package_name <- db$matches$Package[[1]]
      topic <- db$matches$Topic[[1]]
    }
  }

  # Get help file
  args <- list(topic = topic, help_type = "text")
  if (package_name != "") {
    args$package <- package_name
  }
  help_file <- do.call(utils::help, args)

  # Check if help was found
  if (length(help_file) == 0) {
    pkg_msg <- if (package_name == "") {
      "any loaded package"
    } else {
      sprintf("package '%s'", package_name)
    }
    return(list(
      success = FALSE,
      topic = topic,
      package = package_name,
      error = sprintf("No help found for topic '%s' in %s", topic, pkg_msg)
    ))
  }

  # Get the resolved help file name from help_file
  # help_file contains the path(s) to the help file
  help_path <- as.character(help_file)
  resolved_name <- basename(help_path)[1]  # Get the first match

  # Get the Rd database for the package
  rd_db <- tools::Rd_db(package_name)

  # Look up the Rd object using the resolved name
  rd_file_name <- paste0(resolved_name, ".Rd")
  rd_obj <- rd_db[[rd_file_name]]

  if (is.null(rd_obj)) {
    return(list(
      success = FALSE,
      topic = topic,
      package = package_name,
      error = sprintf(
        "Could not find help documentation for topic '%s' in package '%s'",
        topic,
        package_name
      )
    ))
  }

  # Convert Rd to plain text using tools::Rd2txt (exported function)
  # capture.output returns a character vector (one element per line), so 
  # collapse it
  formatted_lines <- utils::capture.output({
      tools::Rd2txt(rd_obj)
  })
  formatted_content <- paste(formatted_lines, collapse = "\n")

  # Extract package name from help_file attributes or use provided package_name
  actual_package <- if (package_name == "") {
    # Try to extract from help_file structure
    pkg_info <- attr(help_file, "topic")
    if (!is.null(pkg_info) && length(pkg_info) > 0) {
      pkg_info[1]
    } else {
      "unknown"
    }
  } else {
    package_name
  }

  list(
    success = TRUE,
    topic = topic,
    package = actual_package,
    content = formatted_content
  )
}



#' List Available Vignettes for R Package
#'
#' @description
#' Get a list of all available vignettes (long-form documentation, tutorials,
#' and guides) for a specific R package. Vignettes provide detailed examples
#' and workflows beyond standard help pages. To read a vignette, use
#' \code{tricobbler-mcp_tool_docs_vignette(package_name, vignette)}.
#'
#' @param package_name Character string, the exact name of the package to list
#'   vignettes from (e.g., \code{"tricobbler"}, \code{"dplyr"},
#'   \code{"ggplot2"}). This parameter is required.
#'
#' @return A list containing:
#' \describe{
#'   \item{success}{Logical, whether vignette list was retrieved successfully}
#'   \item{package}{Character, the package name that was queried}
#'   \item{vignettes}{Data frame with columns \code{name} (vignette
#'     filename without extension),
#'     \code{title} (vignette title), \code{file} (full filename), and
#'     \code{pdf} (PDF path if available)}
#'   \item{count}{Integer, total number of vignettes found}
#'   \item{error}{Character, error message if success is FALSE}
#' }
#'
#' @examples
#'
#' # Example output from calling the tool
#' tricobbler:::mcp_tool_docs_available_vignettes("data.table")
#'
#' @keywords mcp-tool mcp-category-docs
#' @noRd
mcp_tool_docs_available_vignettes <- function(package_name) {
  # Validate input
  if (
    missing(package_name) ||
      !is.character(package_name) ||
      length(package_name) != 1 ||
      package_name == ""
  ) {
    return(list(
      success = FALSE,
      error = "package_name parameter is required and must be a non-empty string" # nolint: line_length_linter.
    ))
  }

  # Check if package exists
  if (system.file(package = package_name) == "") {
    return(list(
      success = FALSE,
      package = package_name,
      error = sprintf(
        "Package '%s' is not installed or not available",
        package_name
      )
    ))
  }

  # Get vignette information
  vigs <- utils::vignette(package = package_name)

  if (length(vigs$results) == 0 || nrow(vigs$results) == 0) {
    return(list(
      success = TRUE,
      package = package_name,
      vignettes = data.frame(
        name = character(0),
        title = character(0),
        package = character(0)
      ),
      count = 0,
      message = sprintf("No vignettes found in package '%s'", package_name)
    ))
  }

  # Extract vignette information
  # vigs$results has columns: Package, LibPath, Item, Title
  vignettes <- data.frame(
    name = as.character(vigs$results[, "Item"]),
    title = as.character(vigs$results[, "Title"]),
    package = as.character(vigs$results[, "Package"]),
    stringsAsFactors = FALSE
  )

  list(
    success = TRUE,
    package = package_name,
    vignettes = vignettes,
    count = nrow(vignettes)
  )
}

#' Get R Package Vignette Content
#'
#' @description
#' Retrieve the full content of a specific vignette from an R package.
#' Vignettes are long-form documentation that provide tutorials, workflows,
#' and detailed examples. The content is returned as plain text.
#'
#' @param package_name Character string, the exact name of the package
#'   containing the vignette (e.g., \code{"tricobbler"}, \code{"dplyr"},
#'   \code{"ggplot2"}).
#'   This parameter is required.
#' @param vignette Character string, the name of the vignette to retrieve
#'   (without file extension). Use
#'   \code{tricobbler-mcp_tool_docs_available_vignettes} to see available
#'   vignettes. If not specified, defaults to the package name.
#'   Examples: \code{"introduction"}, \code{"dplyr"}, \code{"ggplot2-specs"}.
#'
#' @return A list containing:
#' \describe{
#'   \item{success}{Logical, whether the vignette was found and retrieved}
#'   \item{package}{Character, the package name}
#'   \item{vignette}{Character, the vignette name}
#'   \item{title}{Character, vignette title if available}
#'   \item{content}{Character, full vignette content as plain text}
#'   \item{error}{Character, error message if success is FALSE}
#' }
#'
#' @examples
#'
#' # Example output from calling the tool
#' res <- tricobbler:::mcp_tool_docs_vignette("data.table", "datatable-faq")
#' strsplit(res$content, "\n")[[1]]
#'
#' @keywords mcp-tool mcp-category-docs
#' @noRd
mcp_tool_docs_vignette <- function(package_name, vignette = package_name) {
  # Validate inputs
  if (missing(package_name) || !is.character(package_name) ||
      length(package_name) != 1 || package_name == "") {
    return(list(
      success = FALSE,
      error = paste(
        "package_name parameter is required and",
        "must be a non-empty string"
      )
    ))
  }

  if (!is.character(vignette) || length(vignette) != 1 || vignette == "") {
    return(list(
      success = FALSE,
      error = "vignette parameter must be a non-empty string"
    ))
  }

  # Check if package exists
  if (system.file(package = package_name) == "") {
    return(list(
      success = FALSE,
      package = package_name,
      vignette = vignette,
      error = sprintf(
        "Package '%s' is not installed or not available",
        package_name
      )
    ))
  }

  # Find vignette files in the doc directory
  doc_dir <- system.file("doc", package = package_name)

  if (doc_dir == "") {
    return(list(
      success = FALSE,
      package = package_name,
      vignette = vignette,
      error = sprintf(
        "No documentation directory found for package '%s'",
        package_name
      )
    ))
  }

  # Look for vignette source files in priority order
  extensions <- c(".Rmd", ".rmd", ".md", ".Rnw", ".rnw", ".R", ".r")
  vig_path <- NULL

  for (ext in extensions) {
    candidate <- file.path(doc_dir, paste0(vignette, ext))
    if (file.exists(candidate)) {
      vig_path <- candidate
      break
    }
  }

  if (is.null(vig_path)) {
    return(list(
      success = FALSE,
      package = package_name,
      vignette = vignette,
      error = sprintf(
        "Vignette '%s' not found in package '%s'. Use tricobbler-mcp_tool_docs_available_vignettes to see available vignettes.", # nolint: line_length_linter.
        vignette, package_name
      )
    ))
  }

  # Read vignette content
  content <- readLines(vig_path, warn = FALSE)
  formatted_content <- paste(content, collapse = "\n")

  # Extract title from vignette metadata if available
  title <- vignette
  title_match <- grep(
    "^%?\\\\VignetteIndexEntry\\{(.*)\\}",
    content,
    value = TRUE
  )
  if (length(title_match) > 0) {
    title <- sub("^%?\\\\VignetteIndexEntry\\{(.*)\\}.*", "\\1", title_match[1])
  }

  list(
    success = TRUE,
    package = package_name,
    vignette = vignette,
    title = title,
    content = formatted_content
  )
}

#' Search for R Packages by Keyword
#'
#' @description
#' Search for R packages that match a keyword or pattern in their name,
#' title, or description. Searches CRAN package database for matches.
#' Useful for discovering packages related to a specific topic.
#'
#' @param query Character string, the search query. Can be a keyword, partial
#'   package name, or topic. The search is case-insensitive and matches against
#'   package names, titles, and descriptions.
#'   Examples: \code{"pipeline"}, \code{"visualization"}, \code{"data"}.
#' @param max_results Integer, maximum number of results to return. Default is 
#'   20. Use this to limit output size for broad searches.
#'   Example values: \code{10}, \code{50}, \code{100}.
#'
#' @return A list containing:
#' \describe{
#'   \item{success}{Logical, whether the search completed successfully}
#'   \item{query}{Character, the search query that was used}
#'   \item{packages}{Data frame with columns \code{package} (package name),
#'     \code{version} (version string), \code{title} (package title),
#'     \code{description} (brief description), \code{date} (publication date),
#'     and \code{maintainer} (maintainer name)}
#'   \item{count}{Integer, total number of packages matching the query}
#'   \item{returned}{Integer, number of results returned 
#'         (limited by max_results)}
#'   \item{error}{Character, error message if success is FALSE}
#' }
#'
#' @examples
#'
#' # Example output from calling the tool
#' tricobbler:::mcp_tool_search_packages("tricobbler")
#'
#' @keywords mcp-tool mcp-category-discovery
#' @noRd
mcp_tool_search_packages <- function(query, max_results = 20) {
  # Validate inputs
  if (
    missing(query) || !is.character(query) || length(query) != 1 || query == ""
  ) {
    return(list(
      success = FALSE,
      error = "query parameter is required and must be a non-empty string"
    ))
  }

  if (!is.numeric(max_results) || length(max_results) != 1 || max_results < 1) {
    return(list(
      success = FALSE,
      error = "max_results must be a positive integer"
    ))
  }

  # Use pkgsearch if available, otherwise return error
  if (!requireNamespace("pkgsearch", quietly = TRUE)) {
    return(list(
      success = FALSE,
      error = "Package 'pkgsearch' is required for searching CRAN packages. Install it with: install.packages('pkgsearch')" # nolint: line_length_linter.
    ))
  }

  # Search CRAN using pkgsearch
  search_result <- pkgsearch::pkg_search(query = query, size = max_results)

  if (nrow(search_result) == 0) {
    return(list(
      success = TRUE,
      query = query,
      packages = data.frame(
        package = character(0),
        version = character(0),
        title = character(0),
        description = character(0),
        date = character(0),
        maintainer = character(0)
      ),
      count = 0,
      returned = 0,
      message = sprintf("No packages found matching query: %s", query)
    ))
  }

  # Extract relevant fields
  matched_pkgs <- data.frame(
    package = search_result$package,
    version = as.character(search_result$version),
    title = search_result$title,
    # Truncate long descriptions
    description = substr(search_result$description, 1, 200),
    date = as.character(search_result$date),
    maintainer = search_result$maintainer_name,
    stringsAsFactors = FALSE
  )

  list(
    success = TRUE,
    query = query,
    packages = matched_pkgs,
    count = nrow(search_result),
    returned = nrow(matched_pkgs)
  )
}

#' Get R Package Information
#'
#' @description
#' Retrieve detailed metadata and information about a specific R package,
#' including version, dependencies, maintainer, and description. This provides
#' comprehensive package details beyond basic help topics.
#'
#' @param package_name Character string, the exact name of the package to get
#'   information about (e.g., \code{"tricobbler"}, \code{"ggplot2"},
#'   \code{"dplyr"}).
#'   This parameter is required.
#'
#' @return A list containing:
#' \describe{
#'   \item{success}{Logical, whether package information was retrieved
#'     successfully}
#'   \item{package}{Character, the package name}
#'   \item{info}{List containing package metadata:
#'     \code{version}, \code{title}, \code{description}, \code{author},
#'     \code{maintainer}, \code{license}, \code{depends},
#'     \code{imports}, \code{suggests}, \code{url}, \code{bugreports},
#'     and other DESCRIPTION fields}
#'   \item{error}{Character, error message if success is FALSE}
#' }
#'
#' @examples
#'
#' # Example output from calling the tool
#' res <- tricobbler:::mcp_tool_search_package_info("tricobbler")
#' strsplit(res$info$description, "\n")[[1]]
#'
#' @keywords mcp-tool mcp-category-info
#' @noRd
mcp_tool_search_package_info <- function(package_name) {
  # Validate input
  if (missing(package_name) || !is.character(package_name) ||
      length(package_name) != 1 || package_name == "") {
    return(list(
      success = FALSE,
      error = "package_name parameter is required and must be a non-empty string" # nolint: line_length_linter.
    ))
  }

  # Check if package exists
  package_path <- system.file(package = package_name)
  if (package_path == "") {
    return(list(
      success = FALSE,
      package = package_name,
      error = sprintf(
        "Package '%s' is not installed or not available",
        package_name
      )
    ))
  }

  # Get package description
  pkg_desc <- utils::packageDescription(package_name)

  if (is.null(pkg_desc) || inherits(pkg_desc, "try-error")) {
    return(list(
      success = FALSE,
      package = package_name,
      error = sprintf(
        "Could not retrieve information for package '%s'",
        package_name
      )
    ))
  }

  # fs <- list.files(
  #   package_path,
  #   all.files = FALSE,
  #   full.names = FALSE,
  #   recursive = TRUE,
  #   include.dirs = FALSE
  # )
  # fs <- gsub("[/|\\\\]+", "/", fs)
  #
  # Exclude meta directories
  # fs <- fs[grepl(
  #   "^(?!Meta/|help/|docs/|R/|html/|DESCRIPTION|INDEX|WORDLIST)",
  #   fs,
  #   perl = TRUE
  # )]

  # Extract key fields (convert to list, handling NULL values)
  info <- list(
    version = pkg_desc$Version %||% NA_character_,
    title = pkg_desc$Title %||% NA_character_,
    description = pkg_desc$Description %||% NA_character_,
    license = pkg_desc$License %||% NA_character_,
    depends = pkg_desc$Depends %||% NA_character_,
    imports = pkg_desc$Imports %||% NA_character_,
    suggests = pkg_desc$Suggests %||% NA_character_,
    url = pkg_desc$URL %||% NA_character_,
    bugreports = pkg_desc$BugReports %||% NA_character_,
    repository = pkg_desc$Repository %||% NA_character_
  )

  list(
    success = TRUE,
    package = package_name,
    info = info
  )
}

#' List Files in R Package
#'
#' @description
#' Get a list of all non-standard files in an R package directory, excluding
#' meta directories (Meta/, help/, docs/, R/, html/), and standard files
#' (DESCRIPTION, INDEX, WORDLIST). Useful for discovering package resources
#' like data files, examples, and additional assets. To read a package file,
#' use `tricobbler-mcp_tool_read_package_file` tool.
#'
#' @param package_name Character string, the exact name of the package to list
#'   files from (e.g., \code{"tricobbler"}, \code{"dplyr"}, \code{"ggplot2"}).
#'   This parameter is required.
#'
#' @return A list containing:
#' \describe{
#'   \item{success}{Logical, whether file list was retrieved successfully}
#'   \item{package}{Character, the package name that was queried}
#'   \item{files}{Character vector, relative paths to files in the package
#'     (excluding Meta/, help/, docs/, R/, html/, DESCRIPTION, INDEX, WORDLIST).
#'     The paths are always relative to the package root}
#'   \item{error}{Character, error message if success is FALSE}
#' }
#'
#' @examples
#'
#' # Example output from calling the tool
#' ret <- tricobbler:::mcp_tool_search_package_files("data.table")
#'
#' ret$files
#'
#' @keywords mcp-tool mcp-category-discovery
#' @noRd
mcp_tool_search_package_files <- function(package_name) {
  # Validate input
  if (missing(package_name) || !is.character(package_name) ||
      length(package_name) != 1 || package_name == "") {
    return(list(
      success = FALSE,
      error = paste(
        "package_name parameter is required and",
        "must be a non-empty string"
      )
    ))
  }

  # Check if package exists
  package_path <- system.file(package = package_name)
  if (package_path == "") {
    return(list(
      success = FALSE,
      package = package_name,
      error = sprintf(
        "Package '%s' is not installed or not available",
        package_name
      )
    ))
  }

  fs <- list.files(
    package_path,
    all.files = FALSE,
    full.names = FALSE,
    recursive = TRUE,
    include.dirs = FALSE
  )
  fs <- gsub("[/|\\\\]+", "/", fs)

  # Exclude meta directories and standard files
  fs <- fs[grepl(
    "^(?!Meta/|help/|docs/|R/|html/|DESCRIPTION|INDEX|WORDLIST)",
    fs,
    perl = TRUE
  )]

  list(
    success = TRUE,
    package = package_name,
    files = fs
  )
}

#' Read Content from R Package File
#'
#' @description
#' Read a specific file from an R package directory and return its content
#' as text. Supports reading partial content with line-based pagination
#' for large files. Useful for inspecting package vignettes, documentation
#' source files, datasets, and other resources.
#'
#' @param relpath Character string, the relative path to the file within the
#'   package directory. Must not contain \code{".."} or special characters
#'   (only letters, digits, underscores, dashes, dots, and path separators
#'   allowed). Use \code{tricobbler-mcp_tool_search_package_files} to
#'   discover available files. Examples: \code{"inst/examples/demo.R"},
#'   \code{"doc/introduction.Rmd"}.
#' @param package_name Character string, the exact name of the package
#'   containing the file (e.g., \code{"tricobbler"}, \code{"dplyr"},
#'   \code{"ggplot2"}).
#'   This parameter is required.
#' @param begin Integer, the starting line number to read from (1-indexed).
#'   Default is \code{1L} (including the first line. Use this for pagination
#'   or reading specific sections. Example values: \code{1}, \code{50},
#'   \code{100}.
#' @param n Integer, the maximum number of lines to read. Default is \code{20}.
#'   Combined with \code{begin}, this enables reading files in chunks.
#'   Example values: \code{50}, \code{100}, \code{500}.
#'
#' @return A list containing:
#' \describe{
#'   \item{success}{Logical, whether the file was read successfully}
#'   \item{package}{Character, the package name}
#'   \item{relpath}{Character, the relative path that was read}
#'   \item{range}{Integer vector of length 2, one-indexed range
#'     \code{[start, end)} indicating the actual lines read}
#'   \item{content}{Character vector, file content with each element
#'     representing one line}
#'   \item{error}{Character, error message if success is FALSE}
#' }
#'
#' @examples
#'
#' # Example output from calling the tool
#'
#' tricobbler:::mcp_tool_read_package_file(
#'   relpath = "mcp/DEVELOPER_GUIDE.md",
#'   package_name = "tricobbler",
#'   begin = 1,
#'   n = 3
#' )
#'
#' tricobbler:::mcp_tool_read_package_file(
#'   relpath = "mcp/SPECIFICATION-TOOLS.md",
#'   package_name = "tricobbler",
#'   begin = 1,
#'   n = 3
#' )
#'
#'
#' @keywords mcp-tool mcp-category-discovery
#' @noRd
mcp_tool_read_package_file <- function(relpath, package_name,
                                       begin = 1L, n = 20) {

  if (
    length(relpath) != 1 ||
      !is.character(relpath) ||
      is.na(relpath) ||
      grepl("[^a-zA-Z0-9_/|\\\\.-]", relpath) ||
      grepl("[.]{2}", relpath)
  ) {
    return(list(
      success = FALSE,
      error = "relpath parameter should not contain `..` or any character other than letters, digits, underscores, dashes, and dots" # nolint: line_length_linter.
    ))
  }

  # Validate input
  if (missing(package_name) || !is.character(package_name) ||
      length(package_name) != 1 || package_name == "") {
    return(list(
      success = FALSE,
      error = "package_name parameter is required and must be a non-empty string" # nolint: line_length_linter.
    ))
  }

  # Check if package exists
  package_path <- system.file(package = package_name)
  if (package_path == "") {
    return(list(
      success = FALSE,
      package = package_name,
      error = sprintf(
        "Package '%s' is not installed or not available",
        package_name
      )
    ))
  }

  path <- file.path(package_path, relpath)
  if (!file.exists(path)) {
    return(list(
      success = FALSE,
      package = package_name,
      error = sprintf(
        "Path '%s' is not available from package '%s'",
        relpath,
        package_name
      )
    ))
  }

  begin <- as.integer(begin)
  n <- as.integer(n)
  if (is.na(n)) {
    n <- -1
  }
  if (!isTRUE(begin >= 1L)) {
    begin <- 1L
  }


  s <- readLines(con = path, n = n + begin - 1, warn = FALSE)
  if (begin > 1) {
    s <- s[ -seq_len(begin - 1) ]
  }

  list(
    success = TRUE,
    package = package_name,
    relpath = relpath,
    range = c(begin, begin + length(s) - 1),
    content = s
  )
}

#' Run R Code
#'
#' @description
#' Execute R code and capture printed values, text output, messages,
#' warnings, and errors.
#'
#' ## CORE RULES (FOLLOW STRICTLY)
#' - MUST work incrementally: each call should do one small, well-defined task
#' - MUST create no more than one rendered figure per tool call.
#'   Use separate calls for multiple figures.
#' - MUST NOT use this tool to "talk to the user". Explanations and
#'   interpretation belong in the assistant message
#' - MUST read any error messages carefully
#' - MUST NOT make more than 2 attempts to fix an error
#'     - After 2 failed attempts: stop, summarize what you tried, include the
#'       error(s), and propose the next change without executing it.
#'
#' ## SAFETY REQUIREMENTS (MUST FOLLOW)
#' - This code runs in a global environment. Write code that is safe,
#'   reversible, and non-destructive
#' - MUST NOT perform any of the following UNLESS the user explicitly
#'   requests it and you first show the code and target paths/URLs:
#'     - File writes or modifications (persistent output, overwriting, deleting)
#'     - System/shell execution (system, system2, pipe, shell)
#'     - Network requests
#'     - Package installation or updates
#' - SHOULD NOT change global state (options, environment variables,
#'   working directory, etc.)
#'     - Working directory, options and environment variables are reset
#'       between tool calls
#' - MUST use temporary files for any ephemeral storage needs
#'   (\code{tempfile()})
#'
#' ## CODE AND OUTPUT STYLE
#' - ALWAYS write clear, concise, and idiomatic R code, preferring packages and
#'   functions from the tidyverse ecosystem when available
#' - PREFER less than 50 lines of code per tool call
#' - SHOULD use code comments to explain only the non-obvious parts of the code
#'     - AVOID using comments to literally describe the code
#' - DO return results implicitly (\code{x}, not \code{print(x)})
#' - DO make the last expression the object you want to show (e.g. a data frame,
#'   tibble, list or scalar)
#' - AVOID \code{print()} and \code{cat()} unless necessary. If \code{cat()}
#'   is unavoidable, you MUST use a SINGLE \code{cat()} call and keep it concise
#' - PREFER returning structured objects (tibbles, data frames, lists) and brief
#'   summaries (\code{head()}, \code{str()}, \code{summary()})
#' - AVOID extremely large outputs; show summaries and return key results
#'
#' @param code Character string, the R code to execute. Must be valid R code.
#'   Keep code concise and focused on a single task.
#'
#' @return A list containing:
#' \describe{
#'   \item{success}{Logical, whether the code executed without errors}
#'   \item{output}{Character, captured console output (prints, messages,
#'     warnings)}
#'   \item{value}{The return value from the code execution (if success = TRUE)}
#'   \item{error}{Character, error message if execution failed}
#'   \item{warnings}{Character vector, any warnings generated during execution}
#'   \item{messages}{Character vector, any messages generated during execution}
#' }
#'
#' @examples
#'
#' # Example: Simple calculation
#' tricobbler:::mcp_tool_run_r("mean(1:10)")
#'
#' # Example: Data frame operation
#' tricobbler:::mcp_tool_run_r("head(mtcars, 3)")
#'
#' @keywords mcp-tool mcp-category-execution
#' @noRd
mcp_tool_run_r <- function(code, .state_env = fastmap::fastmap()) {
  # Validate input
  if (missing(code) || !is.character(code) || length(code) != 1 || code == "") {
    return(list(
      success = FALSE,
      error = "code parameter is required and must be a non-empty string"
    ))
  }

  # Save current state to restore after execution
  old_wd <- getwd()
  old_opts <- options()
  old_env <- as.list(Sys.getenv())

  # Restore state on exit
  on.exit({
    setwd(old_wd)
    options(old_opts)
    # Restore only environment variables that existed before
    current_env <- names(Sys.getenv())
    old_env_names <- names(old_env)
    # Remove new env vars
    new_vars <- setdiff(current_env, old_env_names)
    if (length(new_vars) > 0) {
      do.call(Sys.unsetenv, as.list(new_vars))
    }
    # Restore old env vars
    do.call(Sys.setenv, old_env)
  }, add = TRUE)

  # Capture output, warnings, messages, and errors
  output_text <- character(0)
  warnings_list <- character(0)
  messages_list <- character(0)
  result_value <- NULL

  # Use withCallingHandlers to capture warnings and messages
  # Use tryCatch to capture errors and determine success
  exec_result <- tryCatch(
    {
      output_text <- capture.output({
        result_value <- withCallingHandlers(
          eval(parse(text = code), envir = .GlobalEnv),
          warning = function(w) {
            warnings_list <<- c(warnings_list, conditionMessage(w))
            invokeRestart("muffleWarning")
          },
          message = function(m) {
            messages_list <<- c(messages_list, conditionMessage(m))
            invokeRestart("muffleMessage")
          }
        )
      })
      list(success = TRUE, value = result_value)
    },
    error = function(e) {
      list(success = FALSE, error = conditionMessage(e))
    }
  )

  # Build result list
  result <- list(
    success = exec_result$success,
    output = paste(output_text, collapse = "\n")
  )

  if (exec_result$success) {
    result$value <- exec_result$value
  } else {
    result$error <- exec_result$error
  }

  if (length(warnings_list) > 0) {
    result$warnings <- warnings_list
  }

  if (length(messages_list) > 0) {
    result$messages <- messages_list
  }

  result
}
