# TriCobbler Package - Agent Guide

## Architecture Overview

**Core Philosophy:** Simplified workflow orchestration with immutable policy blueprints (S7) and mutable runtime execution (R6). Focus on manifest→instance pattern.

**Two-Layer Architecture:**
1. **Policy Layer (S7)**: `Manifest` contains `MasterPolicy` + `StatePolicy[]` - See [R/class-policy.R](R/class-policy.R)
2. **Runtime Layer (R6)**: `Scheduler` orchestrates execution, `Context` persists results - See [R/class-scheduler.R](R/class-scheduler.R)

**Data Flow:** Manifest blueprint → Agents registered → Scheduler executes by priority → Context records results

## Critical Rules

**⚠️ MANDATORY FIRST STEP: Always read `.github/copilot/DEVELOPMENT-RULES.md` (lines 1-40) BEFORE performing ANY task** - This contains non-negotiable conventions that prevent repeated mistakes.

**ENFORCEMENT RULE:** You MUST call `read_file` on `.github/copilot/DEVELOPMENT-RULES.md` (lines 1-40) as your FIRST action when the user requests:
- Documentation changes (roxygen2, .Rd files)
- Spell checking (`spellchk`, `spelling::spell_check_package()`)
- Code formatting or style fixes
- Adding/modifying package dependencies
- Creating/editing examples
- MCP tool changes

**DO NOT proceed with the task until you have read the rules.** The checklist tells you exactly how to handle each scenario correctly. 

## Essential Guides

Additional documentation is available in `.github/copilot/` for:
- DevOps workflows (load, test, check, build) - See `.github/copilot/DEVOPS-GUIDE.md`
- S7 vs R6 documentation examples - See `.github/copilot/S7-R6-DOCUMENTATION.md`

