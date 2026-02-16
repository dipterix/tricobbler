#!/usr/bin/env Rscript

#' Quick Validator for Skill Structure
#'
#' Validates a skill's SKILL.md frontmatter and basic directory structure.
#'
#' @examples
#' # Rscript scripts/r/quick_validate.R skill-name --path skills/public
#' # Rscript scripts/r/quick_validate.R skill-name --path skills/public --quiet

"Usage:
  quick_validate.R <skill_name> --path <path> [--quiet]
  quick_validate.R -h | --help
  quick_validate.R --version

Arguments:
  <skill_name>           Name of the skill to validate

Options:
  -h --help              Show this help message and exit
  --version              Show version information
  --path <path>          Parent directory containing the skill
  --quiet                Only report errors (suppress non-error messages)
" -> doc

parsed <- tricobbler::docopt(doc, version = "quick_validate.R v1.0.0")

# ---- Constants ---------------------------------------------------------------

MAX_NAME_LENGTH <- 64L
MAX_DESCRIPTION_LENGTH <- 1024L
ALLOWED_FRONTMATTER_KEYS <- c(
  "name", "description", "license", "allowed-tools", "metadata"
)

# ---- Helpers -----------------------------------------------------------------

info <- function(msg, ...) {
  if (!isTRUE(parsed$quiet)) {
    cat(sprintf(msg, ...), "\n")
  }
}

# ---- Main logic --------------------------------------------------------------

skill_dir <- normalizePath(
  file.path(parsed$path, parsed$skill_name),
  mustWork = FALSE
)

errors <- character(0L)
warnings <- character(0L)

# 1. Check directory exists
if (!dir.exists(skill_dir)) {
  cat(sprintf("[ERROR] Skill directory not found: %s\n", skill_dir))
  quit(status = 1L)
}
info("[  OK] Skill directory exists: %s", skill_dir)

# 2. Check SKILL.md exists
skill_md_path <- file.path(skill_dir, "SKILL.md")
if (!file.exists(skill_md_path)) {
  cat("[ERROR] SKILL.md not found.\n")
  quit(status = 1L)
}
info("[  OK] SKILL.md found")

# 3. Parse frontmatter
raw <- readLines(skill_md_path, warn = FALSE)
body_text <- paste(raw, collapse = "\n")

# Find frontmatter delimiters
if (length(raw) < 3L || raw[1L] != "---") {
  errors <- c(errors, "SKILL.md has no YAML frontmatter (must start with '---').")
} else {
  end_idx <- which(raw[-1L] == "---")
  if (length(end_idx) == 0L) {
    errors <- c(errors, "SKILL.md frontmatter is not closed (missing second '---').")
  } else {
    end_idx <- end_idx[1L] + 1L
    yaml_text <- paste(raw[2L:(end_idx - 1L)], collapse = "\n")

    meta <- tryCatch(
      yaml::yaml.load(yaml_text),
      error = function(e) {
        errors <<- c(errors, sprintf("YAML parse error: %s", conditionMessage(e)))
        NULL
      }
    )

    if (!is.null(meta)) {
      info("[  OK] YAML frontmatter parsed successfully")

      # 3a. Check for unknown keys
      unknown <- setdiff(names(meta), ALLOWED_FRONTMATTER_KEYS)
      if (length(unknown)) {
        warnings <- c(warnings, sprintf(
          "Unknown frontmatter key(s): %s", paste(unknown, collapse = ", ")
        ))
      }

      # 3b. Validate 'name'
      skill_name <- meta[["name"]]
      if (is.null(skill_name) || !nzchar(skill_name)) {
        errors <- c(errors, "Frontmatter 'name' is missing or empty.")
      } else {
        if (!grepl("^[a-z0-9]+(-[a-z0-9]+)*$", skill_name)) {
          errors <- c(errors, sprintf(
            "Skill name '%s' is not valid. Use lowercase letters, digits, and hyphens (no leading/trailing).",
            skill_name
          ))
        }
        if (nchar(skill_name) > MAX_NAME_LENGTH) {
          errors <- c(errors, sprintf(
            "Skill name is too long (%d chars, max %d).",
            nchar(skill_name), MAX_NAME_LENGTH
          ))
        }
      }

      # 3c. Validate 'description'
      desc <- meta[["description"]]
      if (is.null(desc) || !nzchar(desc)) {
        errors <- c(errors, "Frontmatter 'description' is missing or empty.")
      } else {
        if (grepl("<[^>]+>", desc, perl = TRUE)) {
          errors <- c(errors, "Description should not contain HTML/XML-like angle brackets.")
        }
        if (nchar(desc) > MAX_DESCRIPTION_LENGTH) {
          errors <- c(errors, sprintf(
            "Description is too long (%d chars, max %d).",
            nchar(desc), MAX_DESCRIPTION_LENGTH
          ))
        }
        if (grepl("\\[TODO", desc, ignore.case = TRUE)) {
          warnings <- c(warnings, "Description contains TODO placeholder - please update.")
        }
      }

      info("[  OK] Frontmatter keys validated")
    }
  }
}

# 4. Check for forbidden files
forbidden <- c(
  "CREATION_SUMMARY.md", "README_IMPLEMENTATION.md", "VERIFICATION.md"
)
found_forbidden <- forbidden[file.exists(file.path(skill_dir, forbidden))]
if (length(found_forbidden)) {
  for (f in found_forbidden) {
    errors <- c(errors, sprintf("Forbidden file found: %s (delete it)", f))
  }
}

# ---- Report ------------------------------------------------------------------

cat("\n------- Validation Report -------\n")
if (length(warnings)) {
  for (w in warnings) cat(sprintf("[WARN] %s\n", w))
}
if (length(errors)) {
  for (e in errors) cat(sprintf("[FAIL] %s\n", e))
  cat(sprintf("\nResult: FAILED (%d error(s), %d warning(s))\n",
              length(errors), length(warnings)))
  quit(status = 1L)
} else {
  info("Result: PASSED (%d warning(s))", length(warnings))
}
