# TriCobbler DevOps Guide

## Development Workflow

### Load & Test During Development

```r
# Load package into R session
devtools::load_all()

# Test specific functionality
source("adhoc/test-contract-flow.R")

# Run all tests
rcmdcheck::rcmdcheck(args = c("--as-cran", "--run-donttest"))
```

**When to use:**
- After making changes to R code
- Before committing changes
- During active development

---

## Package Checks

### Full CRAN Check

✅ **CORRECT:**
```r
# Run complete R CMD check with CRAN-like settings
rcmdcheck::rcmdcheck(args = c("--as-cran", "--run-donttest"))
```

❌ **WRONG - Incomplete checks:**
```r
# Missing CRAN-specific checks
rcmdcheck::rcmdcheck()

# Missing --run-donttest flag
rcmdcheck::rcmdcheck(args = "--as-cran")

# Using devtools::check() instead (less comprehensive)
devtools::check()

# Using R CMD check without proper flags
# R CMD check .
```

**What this checks:**
- Package structure and metadata
- Documentation completeness
- Code quality and standards
- Example execution
- Test execution (including `\donttest{}` blocks)
- NAMESPACE exports
- ASCII-only compliance in R/ and vignettes/

**Required before:** Pull requests, releases, CRAN submissions

**Fix all:**
- ❌ ERRORS (blocking)
- ⚠️ WARNINGS (blocking)
- ℹ️ NOTES (fix relevant ones)

**Ignore these notes:**
- "CRAN incoming feasibility" (only relevant during CRAN submission)
- "Unable to verify time" (system-specific)

---

## Documentation Updates

### Regenerate Documentation

```r
# Generate .Rd files from roxygen2 comments
devtools::document()

# After updating MCP tool documentation
mcptool_build()

# Check spelling
spelling::spell_check_package()
```

**When to run:**
- After changing roxygen2 comments in R/ files
- After modifying `@title`, `@description`, `@param`, `@field`, `@returns`, etc.
- After creating/modifying MCP tools (functions with `@keywords mcp-tool`)

**Important:** 
- `mcptool_build()` auto-generates YAML files in `inst/mcp/tools/`
- Never edit YAML files manually - they will be overwritten

---

## ASCII Compliance Check

```r
# Check specific file
tools::showNonASCIIfile("R/class-policy.R")

# Check all R files
sapply(list.files("R", pattern = "\\.R$", full.names = TRUE), tools::showNonASCIIfile)

# Check vignettes
sapply(list.files("vignettes", pattern = "\\.(Rmd|Rnw)$", full.names = TRUE), tools::showNonASCIIfile)
```

**Common violations:**
- En-dash (–), em-dash (—) → use hyphen (-)
- Curly quotes ("") → use straight quotes ("")
- Non-breaking hyphen (‑) → use regular hyphen (-)

---

## Pre-Commit Checklist

```r
# 1. Load and test
devtools::load_all()
rcmdcheck::rcmdcheck(args = c("--as-cran", "--run-donttest"))

# 2. Update documentation
devtools::document()
mcptool_build()  # If MCP tools changed

# 3. Check ASCII compliance
tools::showNonASCIIfile("R/")

# 4. Run CRAN check
rcmdcheck::rcmdcheck(args = c("--as-cran", "--run-donttest"))

# 5. Check spelling
spelling::spell_check_package()
```

---

## Common Issues & Solutions

### Issue: "Undocumented parameters in Rd file"

**Cause:** Missing `@param` for S7 properties or function arguments

**Fix:** Add `@param` tags in roxygen2 comments
```r
#' @param name character, the object name
#' @param version character, version string
```

### Issue: "Objects in \usage without \alias in documentation"

**Cause:** Missing `@export` or incorrect documentation structure

**Fix:** Add `@export` for public functions/classes

### Issue: "Non-ASCII characters in code"

**Cause:** Smart quotes, en-dashes from copy-pasting

**Fix:** 
```r
tools::showNonASCIIfile("R/problematic-file.R")
# Replace non-ASCII characters manually
```

### Issue: "Examples fail during R CMD check"

**Cause:** Examples writing to root directory or using unavailable resources

**Fix:** Use `tempdir()`/`tempfile()` in examples
```r
#' @examples
#' outfile <- tempfile(fileext = ".yaml")
#' manifest_write(manifest, outfile)
```

### Issue: "MCP tools not appearing"

**Cause:** Forgot to run `mcptool_build()` after documentation changes

**Fix:**
```r
mcptool_build()
# Check inst/mcp/tools/ for updated YAML files
```

---

## Build & Install

### Install from Source

```r
# Install locally
devtools::install()

# Install with vignettes
devtools::install(build_vignettes = TRUE)
```

### Build Package Tarball

```bash
R CMD build .
```

### Check Built Tarball

```bash
R CMD check --as-cran tricobbler_*.tar.gz
```

---

## Continuous Integration Notes

### Required Checks
- R CMD check with `--as-cran --run-donttest`
- Test coverage (run `devtools::test()`)
- Documentation builds without errors
- Spelling check passes

### Environment Setup
- R version: Latest stable + previous version
- Platform: Linux, macOS, Windows
- Dependencies: Install from DESCRIPTION

---

## References

- Development rules: [DEVELOPMENT-RULES.md](DEVELOPMENT-RULES.md)
- S7/R6 documentation: [S7-R6-DOCUMENTATION.md](S7-R6-DOCUMENTATION.md)
- devtools documentation: https://devtools.r-lib.org/
- R CMD check documentation: https://r-pkgs.org/r-cmd-check.html
