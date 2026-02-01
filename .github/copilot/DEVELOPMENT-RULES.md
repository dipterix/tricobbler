# TriCobbler Development Rules

**READ THIS FIRST before writing any code or documentation.**

This document contains non-negotiable conventions that prevent CRAN check failures, documentation errors, and repeated mistakes. Each rule explains WHY it exists and shows both correct and incorrect patterns.

---

## Quick Checklist (TL;DR)

Use this checklist to verify your work before committing. Click items for detailed explanations.

- [ ] **⛔ NEVER edit inst/WORDLIST or use spelling::update_wordlist() - Use Rd markup instead** → [Section 6](#6-spelling-and-wordlist-management)
- [ ] Check DESCRIPTION for dependencies before importing packages → [Section 1](#1-package-dependencies)
- [ ] NEVER use `library()` or `require()` in package code → [Section 1](#1-package-dependencies)
- [ ] Use `pkg::fun()` or `@importFrom` in R/tricobbler-package.R for Imports → [Section 1](#1-package-dependencies)
- [ ] Use `package_installed()` and `call_pkg_fun()` for Suggests packages → [Section 1](#1-package-dependencies)
- [ ] Reuse utility helpers from R/aaa.R, don't reimplement → [Section 1](#1-package-dependencies)
- [ ] S7 classes use `@param`, not `@field` → [Section 2](#2-documentation-s7-vs-r6)
- [ ] R6 fields documented with `@field` above each field, not at class level → [Section 2](#2-documentation-s7-vs-r6)
- [ ] All types lowercase, comma separator: `type, description` → [Section 3](#3-type-and-description-format)
- [ ] Examples use `tempdir()`/`tempfile()`, not root or `adhoc/` → [Section 4](#4-file-placement-rules)
- [ ] Throwaway scripts go in `adhoc/`, not project root → [Section 4](#4-file-placement-rules)
- [ ] No non-ASCII characters in R/, vignettes/, inst/ → [Section 5](#5-ascii-only-content-in-r-code)
- [ ] NEVER edit inst/WORDLIST - fix spelling issues properly → [Section 6](#6-spelling-and-wordlist-management)
- [ ] Use \code{}, \verb{}, \pkg{} in roxygen2 docs instead of adding to WORDLIST → [Section 6](#6-spelling-and-wordlist-management)
- [ ] 2-space indentation, no tabs → [Section 7](#7-code-formatting)
- [ ] MCP tools have `@keywords mcp-tool mcp-category-*` and `@noRd` → [Section 8](#8-mcp-tool-conventions)
- [ ] Ran `mcptool_build()` after MCP tool doc changes → [Section 8](#8-mcp-tool-conventions)
- [ ] Examples run without `\dontrun{}` unless truly necessary → [Section 9](#examples-best-practices)

---

## 1. Package Dependencies

### Why This Matters

TriCobbler relies on specific packages for its core functionality. Understanding when and how to use each dependency ensures code correctness and prevents unnecessary imports.

**Check DESCRIPTION first:** Always look at `DESCRIPTION` file at the project root to see what packages are actually in `Imports` and `Suggests`.

### Core Dependencies

**S7** - Modern object system for immutable policy classes
- **Use for:** `Manifest`, `MasterPolicy`, `StatePolicy`, `Agent` definitions
- **Import:** `S7::new_class()`, `S7::new_property()`, `S7::class_*`
- **Don't use for:** Runtime state management (use R6 instead)

```r
# Correct usage
MasterPolicy <- S7::new_class(
  name = "MasterPolicy",
  properties = list(version = S7::class_character)
)
```

**R6** - Reference class system for mutable runtime objects
- **Use for:** `Scheduler`, `Context`, stateful execution managers
- **Import:** `R6::R6Class`
- **Don't use for:** Configuration blueprints (use S7 instead)

```r
# Correct usage
Context <- R6::R6Class(
  "Context",
  public = list(
    initialize = function(id) { self$id <- id }
  )
)
```

**ellmer** - LLM integration for agent capabilities
- **Use for:** Agent communication, LLM-powered workflow steps
- **Import:** Functions for LLM chat/completion
- **When to use:** Only in agent execution code, not in policy definitions

**yaml** - Serialization for manifest files
- **Use for:** Reading/writing manifest YAML files via `manifest_read()` / `manifest_write()`
- **Import:** `yaml::read_yaml()`, `yaml::write_yaml()`
- **Don't use directly:** Use wrapper functions `manifest_read()` / `manifest_write()` instead

### Dependency Rules

1. **Never add dependencies without justification** - Each dependency increases maintenance burden
2. **Use `Imports` for runtime dependencies** - S7, R6, ellmer, yaml belong here
3. **Use `Suggests` for optional features** - Testing packages, development tools
4. **Avoid overlapping functionality** - Don't import multiple packages that do the same thing

### CRITICAL: Never Use `library()` or `require()` in Package Code

**Rule:** R packages must NEVER call `library()` or `require()` in their code. This violates CRAN policy and breaks namespace management.

❌ **WRONG - Multiple violations:**
```r
# In R/my-function.R
library(jsonlite)          # NO! Never library() in packages
require(yaml)              # NO! Never require() in packages

my_function <- function(x) {
  toJSON(x)                # Fails - jsonlite not in namespace
}
```

✅ **CORRECT - For `Imports` packages:**

**Option 1:** Use explicit namespace (recommended for occasional use)
```r
# In R/my-function.R
my_function <- function(x) {
  jsonlite::toJSON(x)      # Explicit namespace - always works
}
```

**Option 2:** Add `@importFrom` in package-level docs (recommended for frequent use)
```r
# In R/tricobbler-package.R
#' @importFrom jsonlite toJSON fromJSON
#' @importFrom yaml read_yaml write_yaml
NULL

# In R/my-function.R
my_function <- function(x) {
  toJSON(x)                # Works because of @importFrom
}
```

✅ **CORRECT - For `Suggests` packages (optional dependencies):**

Use utility functions from `R/aaa.R`:
```r
# In R/my-function.R
my_function <- function(x) {
  # Check if package is installed
  if(!package_installed("jsonlite")) {
    stop("jsonlite package required but not installed")
  }
  
  # Call function from optional package
  call_pkg_fun("jsonlite", "toJSON", x)
}
```

**Where to add `@importFrom`:**
- Always in `R/tricobbler-package.R` (package-level documentation)
- Never scattered across multiple files

### Reuse Utility Helpers

**Important:** TriCobbler has utility helpers in `R/aaa.R` (and future `R/helpers-*.R` files). Always check and reuse these before writing your own.

**Available in `R/aaa.R`:**
- `package_installed(pkg)` - Check if package is installed
- `call_pkg_fun(pkg, fun, ...)` - Call function from optional package
- Other utilities (check file for current list)

**Anti-pattern:**
```r
# DON'T write your own when helpers exist
if(!requireNamespace("jsonlite", quietly = TRUE)) {  # NO! Use package_installed() instead
  stop("jsonlite required")
}
```

✅ **CORRECT - Reuse existing helpers:**
```r
if(!package_installed("jsonlite")) {
  stop("Package `jsonlite` required. Please install it first: `install.packages('jsonlite')`")
}
```

---

## 2. Documentation: S7 vs R6

### Why This Matters

S7 and R6 have fundamentally different documentation requirements because they use different object systems. Mixing them causes roxygen2 to generate incorrect documentation and NAMESPACE exports to fail.

**Critical difference:**
- **S7 constructors are R functions** → Properties are function parameters → Use `@param`
- **R6 classes define fields** → Fields are object properties → Use `@field`

For detailed examples, see [S7-R6-DOCUMENTATION.md](S7-R6-DOCUMENTATION.md).

### S7 Classes: Always Use `@param`

**Why:** When you call `Dog(name = "Lola", age = 11)`, you're calling a function that S7 auto-generates from the properties list. Roxygen2 needs `@param` to document function parameters.

✅ **CORRECT:**
```r
#' @title Dog Class
#' @description Represents a dog with name and age
#' @param name character, the dog's name
#' @param age numeric, the dog's age in years
#' @export
Dog <- S7::new_class(
  name = "Dog",
  properties = list(
    name = S7::class_character,
    age = S7::class_numeric
  )
)
```

❌ **WRONG - Using @field for S7:**
```r
#' @field name character, the dog's name  # NO! This documents a field, not a parameter
#' @field age numeric, the dog's age
Dog <- S7::new_class(...)
```

**Result:** Documentation will be incomplete, parameters won't appear in help files.

### R6 Classes: Use `@field` for Fields, `@param` for Method Parameters

**Why:** R6 separates class-level fields from method-level parameters. Fields belong to the object, method parameters are temporary arguments.

✅ **CORRECT:**
```r
#' @title Context
#' @description Manages execution environment
#' @export
Context <- R6::R6Class(
  "Context",
  public = list(
    #' @field id character, unique identifier
    id = NULL,
    
    #' @field rootpath character, root directory path
    rootpath = NULL,
    
    #' @description Initialize context
    #' @param id character, identifier
    #' @param rootpath character, root path
    initialize = function(id, rootpath) {
      self$id <- id
      self$rootpath <- rootpath
    },
    
    #' @description Log a message
    #' @param message character, message to log
    #' @param level character, log level
    logger = function(message, level = "INFO") {
      # implementation
    }
  ),
  active = list(
    #' @field memory_path character, path to memory storage
    memory_path = function() {
      file.path(self$rootpath, "memory")
    }
  )
)
```

❌ **WRONG - Multiple mistakes:**
```r
#' @field id character, unique identifier        # NO! Fields documented at class level
#' @field rootpath character, root directory     # (This was YOUR mistake)
#' @export
Context <- R6::R6Class(
  "Context",
  public = list(
    id = NULL,                                    # Missing @field here!
    rootpath = NULL,                              # Missing @field here!
    
    #' @param id character                        # Correct for method
    initialize = function(id) {
      self$id <- id
    },
    
    #' @param message character                   # Correct for method
    #' @field level character                     # NO! level is a parameter, not a field!
    logger = function(message, level = "INFO") {
      # ...
    }
  ),
  active = list(
    memory_path = function() {                    # Missing @field!
      file.path(self$rootpath, "memory")
    }
  )
)
```

**Key rule:** `@field` goes **immediately above** the field/active binding definition **inside** the R6Class list, NOT at the class level before `R6Class`.

---

## 3. Type and Description Format

### Why This Matters

Consistency in documentation format improves readability and matches R package conventions. Many roxygen2 tags describe non-sentences (fragments), so sentence capitalization is inappropriate.

### The Format: `type, description`

✅ **CORRECT:**
```r
#' @param name character, the dog's name
#' @param age numeric, the dog's age in years
#' @param is_active logical, whether the object is active
#' @field config list, configuration parameters
#' @returns character, the formatted output string
```

❌ **WRONG - Multiple issues:**
```r
#' @param name Character. The dog's name           # Capital type, period separator, capital description
#' @param age Numeric. The dog's age in years.     # Treated like a sentence
#' @param is_active LOGICAL - whether active       # All caps, dash separator
#' @field config List: Configuration parameters    # Colon separator
```

**Rules:**
1. **Type:** lowercase (character, numeric, logical, list, data.frame, etc.)
2. **Separator:** comma with space (`, `)
3. **Description:** lowercase start (it's a fragment, not a sentence)
4. **No terminal period** (unless description is multiple sentences)

**Examples:**
```r
#' @param x numeric, input vector
#' @param verbose logical, whether to print progress messages
#' @field results list, computation results. May contain NULL values.  # Multiple sentences OK
#' @returns data.frame, summary table with columns id, name, value
```

---

## 4. File Placement Rules

### Why This Matters

**Two different contexts:**
1. **Documentation examples** (`@examples` in roxygen2): Must use `tempdir()`/`tempfile()` for CRAN compliance
2. **Terminal scripts** (throwaway code you write to execute commands): Must use `adhoc/` folder to avoid cluttering project root

### Rule 1: Documentation Examples - Use `tempdir()`/`tempfile()`

✅ **CORRECT:**
```r
#' @examples
#' # Create example manifest
#' manifest <- Manifest(
#'   master = MasterPolicy(name = "example", version = "1.0.0", stages = "idle"),
#'   states = list(StatePolicy(name = "s1", stage = "idle", agent_id = "agent1"))
#' )
#' 
#' # Save to temporary file
#' outfile <- tempfile(fileext = ".yaml")
#' manifest_write(manifest, outfile)
#' 
#' # Or use tempdir()
#' outdir <- tempdir()
#' manifest_write(manifest, file.path(outdir, "manifest.yaml"))
```

❌ **WRONG - Writing to root or adhoc:**
```r
#' @examples
#' manifest <- Manifest(...)
#' 
#' # Writes to package root - CRAN violation!
#' manifest_write(manifest, "manifest.yaml")
#' 
#' # Also wrong - don't use adhoc/ in documentation examples
#' manifest_write(manifest, "adhoc/manifest.yaml")
```

**Why `tempdir()`/`tempfile()` for examples:**
- Standard R package convention
- CRAN compliant (temp files cleaned up automatically)
- Works in all environments (user systems, CI, CRAN checks)

### Rule 2: Terminal Scripts - Use `adhoc/` Folder

**Anti-pattern you keep doing:**
When you need to create a throwaway script to execute long terminal commands (because the command is too complex for one line), you sometimes write them to the project root:

❌ **WRONG:**
```r
# You create this in project root:
# /Users/dipterix/projects/tricobbler/run_analysis.R
# /Users/dipterix/projects/tricobbler/temp_script.R
# /Users/dipterix/projects/tricobbler/test_manifest.yaml
```

✅ **CORRECT:**
```r
# Create scripts in adhoc/ folder instead:
# adhoc/run_analysis.R
# adhoc/temp_script.R  
# adhoc/test_manifest.yaml
```

**Why `adhoc/` for throwaway scripts:**
- Git-ignored (see `.gitignore`)
- Clear intent (temporary/testing code)
- Easy to clean (`rm -rf adhoc/`)
- Never committed to package
- Keeps project root clean

**Summary:**
- **roxygen2 `@examples`:** Use `tempdir()`/`tempfile()`
- **Throwaway terminal scripts:** Use `adhoc/` folder
- **Project root:** NEVER write files here (neither examples nor scripts)

---

## 5. ASCII-Only Content in R Code

### Why This Matters

R packages must be portable across all systems (Windows, Mac, Linux with different locales). Non-ASCII characters cause encoding errors on systems with different character sets, failing `R CMD check --as-cran`.

### Where This Applies

**ASCII-only required:**
- All files in `R/` directory (source code and roxygen2 documentation)
- All files in `vignettes/` directory
- All files in `inst/` directory
- DESCRIPTION file

**Unicode allowed (for readability):**
- Instruction files (`.github/` folder, including this file)
- README.md
- News/changelog files

**Note:** This guide uses Unicode symbols (✅ ❌ →) for clarity, which is acceptable in instruction files but would fail CRAN checks in R/ code.

### The Rule: Replace Non-ASCII Characters in R/ and vignettes/

Common violations:
- En-dash (–) to hyphen (-)
- Em-dash (—) to double hyphen (--)
- Curly quotes ("") to straight quotes ("")
- Non-breaking hyphen (‑) to regular hyphen (-)
- Checkmarks (✓ ✅) to [x] or "yes"
- Cross marks (✗ ❌) to [_] or "no"

✅ **CORRECT:**
```r
#' @description A multi-stage workflow with priority-based execution
#' @param x A "special" value
```

❌ **WRONG:**
```r
#' @description A multi–stage workflow with priority‑based execution  # En-dash, non-breaking hyphen
#' @param x A "special" value  # Curly quotes
```

**How to check:**
```r
tools::showNonASCIIfile("R/class-policy.R")
tools::showNonASCIIfile("vignettes/introduction.Rmd")
```

**Why this keeps happening:** Copy-pasting from rendered documentation or word processors introduces smart quotes and en-dashes automatically.

---

## 6. Spelling and WORDLIST Management

### Why This Matters

The `inst/WORDLIST` file is a **LAST RESORT** for unavoidable technical acronyms (like "MCP", "CMD"). Adding terms to WORDLIST masks real spelling errors and reduces documentation quality. Instead, fix the underlying issue by using proper Rd formatting or rephrasing.

### CRITICAL RULE: Never Edit inst/WORDLIST

**Rule:** When `spelling::spell_check_package()` reports words, treat them as **actual spelling issues** to fix, NOT words to add to WORDLIST.

**Important:** Never edit `man/*.Rd` files directly! They are auto-generated from roxygen2 comments in `R/` files. Always edit the roxygen2 documentation in the source R files, then run `devtools::document()` to regenerate the `.Rd` files.

❌ **WRONG - Adding to WORDLIST:**
```r
# Running spelling::spell_check_package()
# Found: "deserialization", "validator", "LLM"
# 
# WRONG RESPONSE: Adding these to inst/WORDLIST
```

❌ **WRONG - Editing .Rd files directly:**
```r
# Editing man/StatePolicy.Rd directly - changes will be overwritten!
```

✅ **CORRECT - Fix the roxygen2 documentation in R/ files:**
```r
# Edit R/class-policy.R (not man/StatePolicy.Rd):

#' @description Uses \code{validator} functions for validation
#' @param llm_client object, \pkg{ellmer} LLM client
#' @details The \verb{deserialization} process converts YAML...

# Then regenerate:
devtools::document()
```

### How to Fix Spelling Issues

**Priority 1: Use Rd Markup (Preferred)**

Use the appropriate Rd markup to indicate the word is intentional:

**For code/functions/arguments:** Use `\code{}`
```r
#' @description The \code{manifest_read()} function deserializes YAML
#' @param validator function, \code{validator(x)} checks validity
```

**For package names:** Use `\pkg{}`
```r
#' @description Integrates with \pkg{ellmer} for LLM support
#' @importFrom yaml read_yaml
```

**For class names:** Use `\code{}`
```r
#' @description Creates a \code{StatePolicy} object
#' @seealso \code{\link{MasterPolicy}}, \code{\link{Manifest}}
```

**For technical terms/jargon:** Use `\verb{}`
```r
#' @details The \verb{deserialization} step reconstructs objects
#' @section Priority: Uses \verb{fail-fast} semantics
```

**For acronyms (MCP, API, YAML, JSON, etc.):** Use `\verb{}`
```r
#' @description \verb{MCP} tool extraction and type inference
#' @details Uses \verb{API} endpoints for \verb{JSON} serialization
```

**Note:** The `\verb{}` markup should prevent spell check errors. If spell check still flags the word after adding `\verb{}`, you likely have plain text instances of the word elsewhere in the file that need to be wrapped.

**Priority 2: Rephrase to Avoid the Word**

When markup doesn't fit naturally, rephrase using simpler words:

**For plurals/possessives:** Rephrase to avoid
```r
# Instead of "validator's" or "validators"
❌ validator's output
✅ output from the validator

❌ multiple validators
✅ multiple validation functions
```

**For other cases:** Use simpler/standard words
```r
# Instead of "lowercased"
❌ stages are lowercased
✅ stages are converted to lowercase

# Instead of "rollout"
❌ staged rollout
✅ gradual deployment

# Instead of "fallbacks"
❌ with fallbacks
✅ with alternative approaches
```

### WORDLIST Should Remain Empty

**Policy:** `inst/WORDLIST` must be empty. ALL flagged terms should be handled via Rd markup or rephrasing.

**⚠️ CRITICAL: NEVER edit `inst/WORDLIST` directly**
**⚠️ CRITICAL: NEVER use `spelling::update_wordlist()` or any spelling package function to add words**

**For acronyms:** Use `\verb{}` markup consistently
```r
# Correct approach for MCP, CMD, API, etc.
❌ Model Context Protocol (MCP)    # Don't spell out - just use acronym
❌ Adding "MCP" to WORDLIST        # Don't add to WORDLIST
❌ spelling::update_wordlist()     # NEVER use this
✅ \verb{MCP} protocol              # Use \verb{} - this prevents spell check errors
✅ \verb{CMD} commands              # Wrap EVERY instance
✅ \verb{API} endpoint              # Must be consistent throughout file
```

**Critical:** Every instance of the acronym in the file must be wrapped in `\verb{}`. If `spelling::spell_check_package()` still flags it, you have missed a plain text instance somewhere. Use grep to find all instances: `grep -n "MCP" R/yourfile.R` and ensure each is wrapped.

**For package names:** Use `\pkg{}` markup
```r
✅ \pkg{ellmer} package
✅ \pkg{fastmap} utilities
```

### Anti-Patterns

❌ **NEVER EVER edit WORDLIST or use spelling update functions:**
```r
# These actions are FORBIDDEN:
spelling::update_wordlist()           # NEVER
spelling::update_wordlist("inst/WORDLIST")  # NEVER
Manually editing inst/WORDLIST        # NEVER
Adding any words to inst/WORDLIST     # NEVER
```

❌ **Adding acronyms:**
```
# NEVER add these to WORDLIST - use \verb{} instead:
MCP
CMD
API
JSON
YAML
```

❌ **Adding technical terms:**
```
# NEVER add these to WORDLIST:
deserialization
validator
validators
lowercased
fallbacks
rollout
```

❌ **Adding class names:**
```
# NEVER add these - use \code{} instead:
StatePolicy
MasterPolicy
ContractState
```

❌ **Adding package-specific jargon:**
```
# NEVER add these - rephrase or use \verb{}:
TriCobbler's
deserializes
```

### Check Your Work

After fixing spelling issues, verify:
```r
spelling::spell_check_package()  # Should return empty or minimal results
tools::showNonASCIIfile("man/")  # Verify no non-ASCII in generated .Rd files
```

---

## 7. Code Formatting

### Why This Matters

Consistent formatting improves readability and reduces merge conflicts. R's standard is 2-space indentation.

### The Rules

1. **Indentation:** 2 spaces (no tabs)
2. **No trailing whitespace**
3. **Line length:** Prefer < 80 characters (not strict for uncommented R code). However, if a comment line exceeds 80 characters (including the leading and trailing white spaces), you MUST fix it, especially in roxygen2 `@examples` tags, since R-CRAN checks will reject the package if any example line exceeds 80 characters.

✅ **CORRECT:**
```r
MasterPolicy <- S7::new_class(
  name = "MasterPolicy",
  parent = BasePolicy,
  properties = list(
    version = property_version(),
    stages = S7::new_property(
      class = S7::class_character,
      validator = function(value) {
        if(length(value) == 0) {
          return("number of stages cannot be 0")
        }
      }
    )
  )
)
```

❌ **WRONG - Mixed indentation:**
```r
MasterPolicy <- S7::new_class(
    name = "MasterPolicy",  # 4 spaces
	parent = BasePolicy,     # Tab character
  properties = list(        # 2 spaces
      version = property_version()  # 6 spaces
  )
)
```

---

## 7. MCP Tool Conventions

### Why This Matters

MCP tools auto-generate YAML specifications from roxygen2 documentation. Manual edits to YAML get overwritten, and missing keywords prevent tool discovery.

### The Rules

1. **Tool marker:** Add `@keywords mcp-tool` to any function that should be an MCP tool
2. **Category marker:** Add `@keywords mcp-category-NAME` (discovery, docs, execution, config, info)
3. **No Rd file:** Add `@noRd` to prevent .Rd generation (YAML is the documentation)
4. **Never edit YAML manually:** Files in `inst/mcp/tools/` are auto-generated by `mcptool_build()`

✅ **CORRECT:**
```r
#' List Available Pipelines
#'
#' @description Discovers all installed pipelines
#' @returns list, containing pipelines (character vector) and count (integer)
#' @keywords mcp-tool mcp-category-discovery
#' @noRd
mcp_tool_pipeline_list <- function() {
  pipelines <- list_pipelines()
  list(pipelines = pipelines, count = length(pipelines))
}
```

❌ **WRONG - Multiple issues:**
```r
#' List Available Pipelines
#'
#' @description Discovers all installed pipelines
#' @returns A list                              # Vague return description
#' @export                                      # NO! Use @noRd for MCP tools
# Missing @keywords mcp-tool
# Missing @keywords mcp-category-*
mcp_tool_pipeline_list <- function() {
  # ...
}
```

**After changing MCP tool docs:** Always run `mcptool_build()` to regenerate YAML.

**Anti-pattern:**
- Editing `inst/mcp/tools/tricobbler-mcp_tool_*.yaml` directly
- Forgetting to run `mcptool_build()` after doc changes

---

## 8. Documentation Tags Reference

### Standard Tags for S7 Exported Classes

```r
#' @title ClassName
#' @description Brief description of the class
#' @param prop1 type, description
#' @param prop2 type, description
#' @examples
#' ClassName(prop1 = "value", prop2 = 123)
#' @export
```

### Standard Tags for S7 Internal/Abstract Classes

```r
#' @title BaseClassName
#' @description Brief description
#' @param prop1 type, description
#' @keywords internal
```

Note: `@keywords internal` hides from package index, no `@export`.

### Standard Tags for R6 Classes

```r
#' @title ClassName
#' @description Brief description of class
#' @export
ClassName <- R6::R6Class(
  "ClassName",
  public = list(
    #' @field field1 type, description
    field1 = NULL,
    
    #' @description Method description
    #' @param arg1 type, description
    #' @returns type, description
    method_name = function(arg1) { }
  )
)
```

### Standard Tags for Functions

```r
#' @title Function Name
#' @description What the function does
#' @param x type, description
#' @param y type, description
#' @returns type, description
#' @examples
#' 
#' function_name(x = 1, y = 2)
#' 
#' @export
```

### Examples Best Practices

**Rule:** Avoid adding `\dontrun{}` or `\donttest{}` unless absolutely necessary.

✅ **CORRECT - Examples run during R CMD check:**
```r
#' @examples
#' 
#' manifest <- Manifest(
#'   master = MasterPolicy(name = "test", version = "1.0.0", stages = "idle"),
#'   states = list(StatePolicy(name = "s1", stage = "idle", agent_id = "a1"))
#' )
#' print(manifest)
#' 
```

❌ **WRONG - Unnecessary \dontrun:**
```r
#' @examples
#' \dontrun{
#'   # This example works fine and should run!
#'   manifest <- Manifest(...)
#'   print(manifest)
#' }
```

**When to use `\dontrun{}` or `\donttest{}`:**
- Code requires external resources (network, database)
- Examples with very long runtime (> 5 seconds)
- Platform-specific code that won't work on CRAN servers

**For interactive examples:** Use `if(interactive()) {}` instead of `\dontrun{}`
```r
#' @examples
#' 
#' # This runs during R CMD check
#' manifest <- Manifest(...)
#' 
#' # This only runs interactively
#' if(interactive()) {
#'   View(manifest)
#'   # or other interactive operations
#' }
#' 
```

**Important:** If you find existing `\dontrun{}` or `\donttest{}` in code, do NOT remove them without understanding why they were added.

---

## 9. Common Mistakes Summary

### YOU Keep Making These Mistakes:

1. **Using `@field` before `R6Class` definition** instead of inside the list
   - ❌ WRONG: Class-level `@field` documentation
   - ✅ CORRECT: Field-level `@field` immediately above each field

2. **Creating files in root directory**
   - ❌ WRONG in examples: `manifest_write(manifest, "file.yaml")` or `manifest_write(manifest, "adhoc/file.yaml")`
   - ✅ CORRECT in examples: `manifest_write(manifest, tempfile(fileext = ".yaml"))`
   - ❌ WRONG for scripts: Creating `run_script.R` in project root
   - ✅ CORRECT for scripts: Creating `adhoc/run_script.R`

3. **Using `Type. Description` format**
   - ❌ WRONG: `@param x Character. The input value.`
   - ✅ CORRECT: `@param x character, the input value`

4. **Mixing `@param` and `@field` for S7 classes**
   - ❌ WRONG: `@field name character` for S7 property
   - ✅ CORRECT: `@param name character, property description`

5. **Forgetting `@keywords mcp-tool` for MCP functions**
   - ❌ WRONG: Just `@export` for MCP tool
   - ✅ CORRECT: `@keywords mcp-tool mcp-category-*` + `@noRd`

6. **Adding unnecessary `\dontrun{}` or `\donttest{}` in examples**
   - ❌ WRONG: Wrapping working examples in `\dontrun{}`
   - ✅ CORRECT: Let examples run during R CMD check unless they require external resources
   - ⚠️ CAUTION: Don't remove existing `\dontrun{}` without understanding why it's there

---

## References

- Full S7/R6 examples: [S7-R6-DOCUMENTATION.md](S7-R6-DOCUMENTATION.md)
- S7 documentation: https://rconsortium.github.io/S7/
- roxygen2 documentation: https://roxygen2.r-lib.org/
