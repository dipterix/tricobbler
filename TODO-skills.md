# TODO: Skills Roadmap

## M1: Virtual environment support for skill scripts

**Status: In progress**

Add runtime configuration to the Skill R6 class so that the caller (scheduler
or user) can specify environment setup commands for script execution. The skill
itself does not know its runtime — the orchestrator provides it.

### Design

Runtime config is passed at Skill construction time, NOT in SKILL.md:

```r
skill <- Skill$new(
  path = "path/to/skill",
  runtime = list(
    python = list(setup = "source .venv/bin/activate"),
    r = list(args = c("--no-save")),
    shell = list(setup = "")
  )
)
```

- `runtime$python$setup` — shell command to run before `python3` (e.g. venv
  activation, conda activate)
- `runtime$r$args` — extra args for Rscript (default: `c("--no-save")`)
- `runtime$shell$setup` — shell preamble before bash scripts

### Implementation

1. `Skill$new()` accepts `runtime` parameter (list, default empty)
2. `Skill` stores it as a public field
3. `build_script_command()` accepts optional `runtime` config:
   - **Python**: wraps in `bash -c "setup && python3 script.py args..."`
   - **R**: prepends args to Rscript prefix_args
   - **Shell**: prepends setup to script execution
4. `tool_fn_script()` passes runtime config to `build_script_command()`

### Acceptance criteria

- A skill with `runtime$python$setup` activates the env before Python scripts
- A skill with no `runtime` works exactly as before (backward compatible)
- R scripts default to `--no-save` when `runtime$r$args` not specified

---

## M2: Set up Python virtual environment for skill-creator

**Status: Not started**

The skill-creator scripts (init_skill.py, package_skill.py, quick_validate.py)
require `pyyaml`. Set up a project-level venv.

### Steps

1. Create `.venv` under project root: `python3 -m venv .venv`
2. Install dependencies: `.venv/bin/pip install pyyaml`
3. Add `.venv/` to `.gitignore` and `.Rbuildignore`
4. Test: create Skill with runtime config pointing to the venv, call a script

---

## M3: Test skill-creator on real example (import-bids)

**Status: Not started**

Use the skill-creator to generate a new skill from
https://github.com/rave-ieeg/rave-gists/blob/main/import-bids.R

~900-line R script that imports BIDS neuroimaging data into RAVE. Goal:
package it as a skill an AI agent can use to import data.

### Steps

1. Load skill-creator into an ellmer::Chat with runtime pointing to .venv
2. Have the LLM create an `import-bids` skill
3. Test the generated skill loads via `Skill$new()`
4. Test `make_tools()` returns valid tools
5. Document issues and iterate

---

## M4: R version of skill-creator scripts

**Status: Not started**

Implement R equivalents under `inst/skills/skill-creator/scripts/r/`:

- `init_skill.R` — mirrors `scripts/py/init_skill.py`
- `package_skill.R` — mirrors `scripts/py/package_skill.py`
- `quick_validate.R` — mirrors `scripts/py/quick_validate.py`

R scripts use docopt for CLI parsing. Both R and Python versions coexist.

---

## M5: Manual end-to-end test

**Status: Not started (requires M1-M4)**

1. Load skill-creator into an ellmer::Chat
2. Create a domain-specific skill
3. Load new skill into a fresh Chat
4. Use the skill to perform the actual task
5. Document pain points and iterate
