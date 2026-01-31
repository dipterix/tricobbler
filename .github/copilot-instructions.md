# TriCobbler Package - Agent Guide

## Architecture Overview

**Core Philosophy:** Multi-agent workflow orchestration combining immutable policy definitions (S7) with mutable execution state (R6).

### Three-Layer Architecture

1. **Policy Layer (S7 - Immutable)**: Blueprint definitions in [class001-policy.R](R/class001-policy.R)
   - `MasterPolicy`: Workflow version + allowed stages (e.g., "triage", "planning", "executing")
   - `StatePolicy`: Individual state metadata (stage name, description, parameters)
   - `Manifest`: Validates all MasterPolicy stages have corresponding StatePolicy entries

2. **Contract Layer (S7 - Immutable)**: Execution agreements in [class002-contract.R](R/class002-contract.R)
   - `ContractExecutor`: Callable function wrapping LLM/script execution logic (inherits `S7::class_function`)
   - `StageContract`: Per-stage execution spec (executor, tools, validators, retries, timeout, fallback)
   - `Contract`: Master agreement linking Manifest to StageContracts with global resources

3. **State Layer (R6 - Mutable)**: Runtime tracking in [class003-contractstate.R](R/class003-contractstate.R)
   - `SubContractState`: Tracks one stage's execution (status, attempts, I/O, timing, validation)
   - `ContractState`: Orchestrates all stage states and workflow progression

**Data Flow:** User creates Manifest → User defines Contract with executors/validators → System creates ContractState → System executes stages through SubContractStates

### The TriCobbler Trinity (WIP)

Specialized agents in [class010-baseagent.R](R/class010-baseagent.R):
- `BaseAgent`: R6 wrapper around `ellmer::Chat` with context window monitoring
- Future: `Planner`, `Worker`, `Librarian` subclasses for decomposition/execution/memory

## Code Conventions

**Class System:**
- S7 for immutable structures (policies, contracts). Document with `@field` for properties
- R6 for mutable state and agents. Document with `@field` for fields/active bindings
- Never mix: S7 uses `@` accessor, R6 uses `$`

**ASCII-Only:** Package must be portable. Replace en-dashes (–) and non-breaking hyphens (‑) with ASCII `-`. Use `tools::showNonASCIIfile(file)` to find violations.

**Formatting:** 2-space indentation, no tabs.

## Developer Workflows

**Load & Test:**
```r
devtools::load_all()        # Load package before testing
source("adhoc/test-contract-flow.R")  # Run example workflow
devtools::test()            # Run testthat suite
```

**Package Checks:**
```r
rcmdcheck::rcmdcheck(args = c("--as-cran", "--run-donttest"))      # Full R CMD check (ask before fixing issues); do NOT use devtools::check()
devtools::document()        # Regenerate Rd files from roxygen2
spelling::spell_check_package()  # Check spellings (don't edit inst/WORDLIST)
```

Please try to fix all the errors and warnings. For notes, please fix as much as you can. Some notes irrelevate to the code such as "Checking CRAN incoming feasibility" or "Unable to verify time" can be ignored.

**Documentation Rules:**
- S7 exported classes: `@title`, `@description`, `@param` (for constructor args), `@field` (for properties), `@examples`, `@export`
- S7 internal/abstract: Add `@keywords internal`, omit `@export`
- R6 classes: `@title`, `@description`, `@field` (for public/active), `@export`
- Never include `set.seed()` in examples

**Adhoc Scripts:** Use `adhoc/` for throwaway tests (not committed to tests/testthat)

## Key Integration Points

**Dependencies:** `S7`, `R6`, `ellmer` (LLM chat), `yaml` (config parsing). Only use base-R + DESCRIPTION imports.

**Validation Pattern:** Contract validators return `TRUE` or character error message (see [adhoc/test-contract-flow.R](adhoc/test-contract-flow.R#L115-L120))

