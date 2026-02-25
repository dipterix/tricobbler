# ---------------------------------------------------------------------------
# Resource Discovery for Shiny Integration
# ---------------------------------------------------------------------------

#' Discover Available Resources
#'
#' @description
#' Scans for available \verb{MCP} tools, skills, and workflows that can be
#' loaded into a chat session. Each resource type is independently
#' configurable: pass \code{TRUE} for auto-detection, a character vector
#' of package names or directory paths for explicit sources, or
#' \code{FALSE}/\code{NULL} to disable.
#'
#' @param tools logical, character vector, or \code{NULL}. Controls
#'   \verb{MCP} tool discovery.
#'   \code{TRUE} (default): discover tools from
#'   \code{"tricobbler"} package;
#'   character vector: package names or directory paths to scan;
#'   \code{FALSE}/\code{NULL}: skip tool discovery.
#' @param skills logical, character vector, or \code{NULL}. Controls
#'   skill discovery.
#'   \code{TRUE} (default): scan built-in skills from
#'   \code{system.file("skills", package = "tricobbler")};
#'   character vector: directory paths containing \code{SKILL.md} files;
#'   \code{FALSE}/\code{NULL}: skip skill discovery.
#' @param workflows logical, character vector, or \code{NULL}. Controls
#'   workflow discovery.
#'   \code{TRUE} (default): scan built-in workflow files from
#'   \code{system.file("skills", package = "tricobbler")};
#'   character vector: file paths or directory paths containing
#'   \code{tricobbler-workflow.yaml} files;
#'   \code{FALSE}/\code{NULL}: skip workflow discovery.
#'
#' @returns A list with three \code{data.frame} components:
#' \describe{
#'   \item{\code{tools}}{columns: \code{name}, \code{description},
#'     \code{source}}
#'   \item{\code{skills}}{columns: \code{name}, \code{description},
#'     \code{path}}
#'   \item{\code{workflows}}{columns: \code{name}, \code{description},
#'     \code{file}}
#' }
#'
#' @examples
#' # Discover everything available
#' resources <- discover_resources()
#' resources$tools
#' resources$skills
#'
#' # Only discover MCP tools from tricobbler
#' resources <- discover_resources(
#'   tools = "tricobbler",
#'   skills = FALSE,
#'   workflows = FALSE
#' )
#'
#' @export
discover_resources <- function(tools = TRUE,
                               skills = TRUE,
                               workflows = TRUE) {

  tool_df <- discover_mcp_tools(tools)
  skill_df <- discover_skills(skills)
  wf_df <- discover_workflows(workflows)

  list(
    tools = tool_df,
    skills = skill_df,
    workflows = wf_df
  )
}


#' Discover MCP tools
#' @param tools logical or character; see \code{\link{discover_resources}}
#' @returns \code{data.frame} with columns \code{name}, \code{description},
#'   \code{source}
#' @noRd
discover_mcp_tools <- function(tools) {
  empty <- data.frame(
    name = character(0L),
    description = character(0L),
    source = character(0L),
    stringsAsFactors = FALSE
  )

  if (isFALSE(tools) || is.null(tools)) {
    return(empty)
  }

  if (isTRUE(tools)) {
    sources <- "tricobbler"
  } else {
    sources <- as.character(tools)
  }

  frames <- lapply(sources, function(src) {
    loaded <- tryCatch(
      mcptool_load_all(src),
      error = function(e) list(),
      warning = function(w) {
        suppressWarnings(mcptool_load_all(src))
      }
    )
    if (length(loaded) == 0L) {
      return(empty)
    }
    data.frame(
      name = names(loaded),
      description = vapply(loaded, function(t) {
        desc <- t$description %||% ""
        if (is.character(desc) && length(desc) == 1L) desc else ""
      }, character(1L)),
      source = rep(src, length(loaded)),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, c(frames, list(make.row.names = FALSE)))
}


#' Discover skills
#' @param skills logical or character; see \code{\link{discover_resources}}
#' @returns \code{data.frame} with columns \code{name}, \code{description},
#'   \code{path}
#' @noRd
discover_skills <- function(skills) {
  empty <- data.frame(
    name = character(0L),
    description = character(0L),
    path = character(0L),
    stringsAsFactors = FALSE
  )

  if (isFALSE(skills) || is.null(skills)) {
    return(empty)
  }

  if (isTRUE(skills)) {
    skills <- "tricobbler"
  } else {
    skills <- as.character(skills)
  }

  skill_paths <- lapply(skills, function(skill) {
    if (package_installed(skill)) {
      # This is a package
      paths <- c(
        list.dirs(
          system.file("skills", package = skill),
          full.names = TRUE,
          recursive = FALSE
        ),
        list.dirs(
          file.path(tools::R_user_dir(skill, which = "data"), "skills"),
          full.names = TRUE,
          recursive = FALSE
        )
      )
    } else {
      paths <- as.character(skill)
    }

    paths <- paths[dir.exists(paths)]
    if (!length(paths)) { return() }

    paths <- normalizePath(paths, mustWork = FALSE)

    # Expand directories: each dir should contain SKILL.md directly,
    paths <- lapply(paths, function(path) {
      list.files(
        path,
        pattern = "SKILL.md",
        all.files = FALSE,
        full.names = TRUE,
        recursive = TRUE,
        include.dirs = FALSE
      )
    })

    paths <- unlist(paths)

    dirname(paths)
  })

  skill_paths <- unname(unlist(skill_paths, recursive = TRUE, use.names = FALSE))

  frames <- lapply(skill_paths, function(p) {
    parsed <- tryCatch(
      parse_skill_md(file.path(p, "SKILL.md")),
      error = function(e) NULL
    )
    if (is.null(parsed)) {
      return(empty)
    }
    nm <- parsed$name
    if (is.null(nm) || !nzchar(nm)) {
      nm <- basename(p)
    }
    data.frame(
      name = nm,
      description = parsed$description %||% "",
      path = normalizePath(p, mustWork = FALSE),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, c(frames, list(make.row.names = FALSE)))
}


#' Discover workflows
#' @param workflows logical or character; see
#'   \code{\link{discover_resources}}
#' @returns \code{data.frame} with columns \code{name}, \code{skill_name},
#'   \code{description}, \code{file}
#' @noRd
discover_workflows <- function(workflows) {
  empty <- data.frame(
    name = character(0L),
    skill_name = character(0L),
    description = character(0L),
    file = character(0L),
    stringsAsFactors = FALSE
  )

  if (isFALSE(workflows) || is.null(workflows)) {
    return(empty)
  }

  skills <- discover_skills(skills = workflows)

  if (!length(skills)) { return(empty) }

  sel <- file.exists(file.path(skills$path, "tricobbler-workflow.yaml"))

  if (!any(sel)) { return(empty) }

  workflows <- fastmap::fastqueue()

  frames <- lapply(which(sel), function(ii) {
    path <- skills$path[[ii]]
    skill_name <- skills$name[[ii]]
    wf_file <- normalizePath(file.path(path, "tricobbler-workflow.yaml"))
    tryCatch(
      {
        wf <- yaml::read_yaml(wf_file)
        re <- lapply(wf$workflows, function(wf_li) {
          data.frame(
            name = wf_li$name,
            skill_name = skill_name,
            description = trimws(paste(wf_li$description, collapse = "\n")),
            file = wf_file,
            stringsAsFactors = FALSE
          )
        })
        do.call("rbind", re)
      },
      error = function(e) {
        empty
      }
    )
  })

  do.call(rbind, c(frames, list(make.row.names = FALSE)))
}
