#!/usr/bin/env Rscript

#' Skill Initializer - Creates a new skill from template
#'
#' @examples
#' # Rscript scripts/r/init_skill.R my-skill --path skills/public
#' # Rscript scripts/r/init_skill.R my-skill --path skills/public --resources scripts,references
#' # Rscript scripts/r/init_skill.R my-skill --path skills/public --resources scripts --examples

"Usage:
  init_skill.R <skill_name> --path <path> [--resources <resources>] [--examples]
  init_skill.R -h | --help
  init_skill.R --version

Arguments:
  <skill_name>                   Skill name (normalised to hyphen-case)

Options:
  -h --help                      Show this help message and exit
  --version                      Show version information
  --path <path>                  Output directory for the skill
  --resources <resources>        Comma-separated list: scripts,references,assets
  --examples                     Create example files inside resource directories
" -> doc

parsed <- tricobbler::docopt(doc, version = "init_skill.R v1.0.0")

# ---- Constants ---------------------------------------------------------------

MAX_SKILL_NAME_LENGTH <- 64L
ALLOWED_RESOURCES <- c("scripts", "references", "assets")

SKILL_TEMPLATE <- '---
name: %s
description: "[TODO: Complete and informative explanation of what the skill does and when to use it. Include WHEN to use this skill.]"
---

# %s

## Overview

[TODO: 1-2 sentences explaining what this skill enables]

## Quick Start

[TODO: Add a concise example of how to use this skill]

## Resources (optional)

Create only the resource directories this skill actually needs.
Delete this section if no resources are required.
'

EXAMPLE_SCRIPT <- '#!/usr/bin/env Rscript

#\' Example helper script for %s
#\'
#\' This is a placeholder script that can be executed directly.
#\' Replace with actual implementation or delete if not needed.

"Usage:
  example.R [--help]
  example.R --version

Options:
  -h --help    Show this help message and exit
  --version    Show version information
" -> doc

parsed <- tricobbler::docopt(doc, version = "example.R v0.1.0")
cat("This is an example script for %s\\n")
# TODO: Add actual script logic here
'

EXAMPLE_REFERENCE <- '# Reference Documentation for %s

This is a placeholder for detailed reference documentation.
Replace with actual reference content or delete if not needed.

## When Reference Docs Are Useful

Reference docs are ideal for:
- Comprehensive API documentation
- Detailed workflow guides
- Complex multi-step processes
- Information too lengthy for main SKILL.md
'

# ---- Helpers -----------------------------------------------------------------

normalize_skill_name <- function(name) {
  name <- tolower(trimws(name))
  name <- gsub("[^a-z0-9]+", "-", name)
  name <- gsub("^-+|-+$", "", name)
  name <- gsub("-{2,}", "-", name)
  name
}

title_case_skill_name <- function(name) {
  parts <- strsplit(name, "-", fixed = TRUE)[[1L]]
  paste(
    vapply(parts, function(w) {
      paste0(toupper(substring(w, 1, 1)), substring(w, 2))
    }, character(1L)),
    collapse = " "
  )
}

parse_resources <- function(raw) {
  if (is.null(raw) || !nzchar(raw)) return(character(0L))
  res <- trimws(strsplit(raw, ",", fixed = TRUE)[[1L]])
  res <- res[nzchar(res)]
  invalid <- setdiff(res, ALLOWED_RESOURCES)
  if (length(invalid)) {
    cat(sprintf("[ERROR] Unknown resource type(s): %s\n", paste(invalid, collapse = ", ")))
    cat(sprintf("   Allowed: %s\n", paste(sort(ALLOWED_RESOURCES), collapse = ", ")))
    quit(status = 1L)
  }
  unique(res)
}

# ---- Main logic --------------------------------------------------------------

skill_name <- normalize_skill_name(parsed$skill_name)
if (!nzchar(skill_name)) {
  cat("[ERROR] Skill name must include at least one letter or digit.\n")
  quit(status = 1L)
}
if (nchar(skill_name) > MAX_SKILL_NAME_LENGTH) {
  cat(sprintf(
    "[ERROR] Skill name '%s' is too long (%d characters). Maximum is %d.\n",
    skill_name, nchar(skill_name), MAX_SKILL_NAME_LENGTH
  ))
  quit(status = 1L)
}
if (skill_name != parsed$skill_name) {
  cat(sprintf("Note: Normalised skill name from '%s' to '%s'.\n",
              parsed$skill_name, skill_name))
}

resources <- parse_resources(parsed$resources)
if (isTRUE(parsed$examples) && length(resources) == 0L) {
  cat("[ERROR] --examples requires --resources to be set.\n")
  quit(status = 1L)
}

out_path <- parsed$path
skill_dir <- normalizePath(file.path(out_path, skill_name), mustWork = FALSE)
skill_title <- title_case_skill_name(skill_name)

cat(sprintf("Initializing skill: %s\n", skill_name))
cat(sprintf("   Location: %s\n", out_path))
if (length(resources)) {
  cat(sprintf("   Resources: %s\n", paste(resources, collapse = ", ")))
  if (isTRUE(parsed$examples)) cat("   Examples: enabled\n")
} else {
  cat("   Resources: none (create as needed)\n")
}
cat("\n")

# Check existence
if (dir.exists(skill_dir)) {
  cat(sprintf("[ERROR] Skill directory already exists: %s\n", skill_dir))
  quit(status = 1L)
}

# Create directory
dir.create(skill_dir, recursive = TRUE, showWarnings = FALSE)
cat(sprintf("[OK] Created skill directory: %s\n", skill_dir))

# Write SKILL.md
skill_content <- sprintf(SKILL_TEMPLATE, skill_name, skill_title)
writeLines(skill_content, file.path(skill_dir, "SKILL.md"))
cat("[OK] Created SKILL.md\n")

# Create resource directories
for (res in resources) {
  res_dir <- file.path(skill_dir, res)
  dir.create(res_dir, recursive = TRUE, showWarnings = FALSE)

  if (isTRUE(parsed$examples)) {
    if (res == "scripts") {
      writeLines(sprintf(EXAMPLE_SCRIPT, skill_name, skill_name),
                 file.path(res_dir, "example.R"))
      cat("[OK] Created scripts/example.R\n")
    } else if (res == "references") {
      writeLines(sprintf(EXAMPLE_REFERENCE, skill_title),
                 file.path(res_dir, "api_reference.md"))
      cat("[OK] Created references/api_reference.md\n")
    } else if (res == "assets") {
      writeLines("# Placeholder asset file\n",
                 file.path(res_dir, "example_asset.txt"))
      cat("[OK] Created assets/example_asset.txt\n")
    }
  } else {
    cat(sprintf("[OK] Created %s/\n", res))
  }
}

cat(sprintf("\n[OK] Skill '%s' initialized successfully at %s\n", skill_name, skill_dir))
cat("\nNext steps:\n")
cat("1. Edit SKILL.md to complete the TODO items and update the description\n")
if (length(resources)) {
  if (isTRUE(parsed$examples)) {
    cat("2. Customise or delete the example files in resource directories\n")
  } else {
    cat("2. Add resources to scripts/, references/, assets/ as needed\n")
  }
} else {
  cat("2. Create resource directories only if needed (scripts/, references/, assets/)\n")
}
cat("3. Run the validator when ready to check the skill structure\n")
