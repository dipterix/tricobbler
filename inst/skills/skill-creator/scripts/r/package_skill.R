#!/usr/bin/env Rscript

#' Skill Packager - Validates then zips a skill into a .skill archive
#'
#' @examples
#' # Rscript scripts/r/package_skill.R my-skill --path skills/public --out dist
#' # Rscript scripts/r/package_skill.R my-skill --path skills/public --out dist --force

"Usage:
  package_skill.R <skill_name> --path <path> --out <out> [--force]
  package_skill.R -h | --help
  package_skill.R --version

Arguments:
  <skill_name>           Name of the skill to package

Options:
  -h --help              Show this help message and exit
  --version              Show version information
  --path <path>          Parent directory containing the skill
  --out <out>            Output directory for the .skill archive
  --force                Overwrite existing .skill file
" -> doc

parsed <- tricobbler::docopt(doc, version = "package_skill.R v1.0.0")

# ---- Run validation first ----------------------------------------------------

skill_dir <- normalizePath(
  file.path(parsed$path, parsed$skill_name),
  mustWork = FALSE
)

if (!dir.exists(skill_dir)) {
  cat(sprintf("[ERROR] Skill directory not found: %s\n", skill_dir))
  quit(status = 1L)
}

# Run quick_validate.R from the same directory as this script
args <- commandArgs(trailingOnly = FALSE)
script_path <- sub("^--file=", "", args[grep("^--file=", args)])
if (length(script_path)) {
  validator <- file.path(dirname(script_path), "quick_validate.R")
} else {
  validator <- "quick_validate.R"
}

if (file.exists(validator)) {
  cat("[....] Running validation...\n")
  validate_exit <- system2(
    command = "Rscript",
    args = c(
      shQuote(validator),
      parsed$skill_name,
      "--path", shQuote(parsed$path),
      "--quiet"
    )
  )
  if (validate_exit != 0L) {
    cat("[ERROR] Validation failed. Fix errors before packaging.\n")
    quit(status = 1L)
  }
  cat("[  OK] Validation passed.\n")
} else {
  cat("[WARN] Validator not found, skipping validation step.\n")
}

# ---- Package -----------------------------------------------------------------

out_dir <- parsed$out
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

archive_name <- paste0(parsed$skill_name, ".skill")
archive_path <- normalizePath(
  file.path(out_dir, archive_name), mustWork = FALSE
)

if (file.exists(archive_path) && !isTRUE(parsed$force)) {
  cat(sprintf(
    "[ERROR] Archive already exists: %s\nUse --force to overwrite.\n",
    archive_path
  ))
  quit(status = 1L)
}

# Gather all files to include
files_to_zip <- list.files(skill_dir, recursive = TRUE, all.files = FALSE)

# Create the zip archive
old_wd <- setwd(skill_dir)
on.exit(setwd(old_wd), add = TRUE)

result <- tryCatch({
  if (file.exists(archive_path)) file.remove(archive_path)
  utils::zip(archive_path, files = files_to_zip, flags = "-r9Xq")
  TRUE
}, error = function(e) {
  cat(sprintf("[ERROR] Failed to create archive: %s\n", conditionMessage(e)))
  FALSE
})

if (isTRUE(result)) {
  info <- file.info(archive_path)
  size_kb <- round(info$size / 1024, 1L)
  cat(sprintf(
    "[  OK] Packaged %d files into %s (%.1f KB)\n",
    length(files_to_zip), archive_path, size_kb
  ))
} else {
  quit(status = 1L)
}
