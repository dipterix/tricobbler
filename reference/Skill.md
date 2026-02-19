# Skill: Load and Serve Skill Directories

R6 class that loads a skill directory (containing `SKILL.md`, optional
`scripts/`, and reference files) and exposes it as a single
[`tool`](https://ellmer.tidyverse.org/reference/tool.html) definition
via the `$make_tools()` method.

## Value

A named list with one
[`tool`](https://ellmer.tidyverse.org/reference/tool.html) object. The
name is `skill_{name}`.

## Details

A skill directory must contain a `SKILL.md` file with optional YAML
`frontmatter` (`name`, `description`, `metadata`) and a markdown body.
The directory may also contain:

- Reference files: top-level files (other than `SKILL.md`) and files
  inside directories whose name starts with `"reference"`
  (case-insensitive), e.g. `reference/`, `references/`, `Reference/`

- A `scripts/` subdirectory with callable R, shell, or python scripts
  (executed via `CLI` only)

Runtime configuration (e.g. `virtualenv` activation, extra interpreter
arguments) is provided at construction time by the scheduler or user,
not embedded in the skill definition itself. This follows the “Immutable
Policy, Mutable Runtime” principle.

The `$make_tools()` method returns a single
[`tool`](https://ellmer.tidyverse.org/reference/tool.html) named
`skill_{name}` that dispatches on an `action` parameter:

- `"readme"`:

  Returns the `SKILL.md` body. Must be called first to unlock other
  actions.

- `"reference"`:

  Reads reference files with optional line range and grep filtering
  (only available when reference files exist)

- `"script"`:

  Executes scripts via `CLI` with
  [`processx::run()`](http://processx.r-lib.org/reference/run.md) (only
  available when scripts exist)

## Public fields

- `description`:

  character, skill description from `SKILL.md` `frontmatter`

- `body`:

  character, the markdown body of `SKILL.md`

- `scripts`:

  named list, script metadata discovered from the `scripts/`
  subdirectory (see `Details`)

- `file_choices`:

  character, relative paths of reference files available for the
  references action (top-level files excluding `SKILL.md`, plus files in
  `reference*/` directories)

- `runtime`:

  list, per-language runtime configuration provided at construction time
  by the scheduler or user. Recognized keys: `python` (with `setup`,
  `args`), `r` (with `setup`, `args`), `shell` (with `setup`). `NULL`
  means use system defaults.

## Active bindings

- `name`:

  character, skill name (lowercase letters and dashes only, read-only)

- `path`:

  character, absolute path to the skill directory (read-only)

## Methods

### Public methods

- [`Skill$new()`](#method-TricobblerSkill-new)

- [`Skill$parse()`](#method-TricobblerSkill-parse)

- [`Skill$make_tools()`](#method-TricobblerSkill-make_tools)

------------------------------------------------------------------------

### Method `new()`

Initialize a new `Skill` from a skill directory

#### Usage

    Skill$new(path, runtime = NULL)

#### Arguments

- `path`:

  character, path to the skill directory (must contain `SKILL.md`)

- `runtime`:

  list or `NULL`, per-language runtime configuration. Recognized keys:
  `python` (with `setup`, `args`), `r` (with `setup`, `args`), `shell`
  (with `setup`). The scheduler or user provides this at construction
  time; the skill definition itself does not specify its own runtime.

------------------------------------------------------------------------

### Method [`parse()`](https://rdrr.io/r/base/parse.html)

Parse the `SKILL.md` file and update fields

#### Usage

    Skill$parse()

------------------------------------------------------------------------

### Method `make_tools()`

Produce a single
[`tool`](https://ellmer.tidyverse.org/reference/tool.html) definition
for this skill

#### Usage

    Skill$make_tools()

## Examples

``` r
skill_dir <- system.file("skills", "weather", package = "tricobbler")
if (nzchar(skill_dir) && dir.exists(skill_dir)) {
  skill <- Skill$new(skill_dir)
  skill$name
  skill$description

  # Produce ellmer tool definitions
  tools <- skill$make_tools()
  names(tools)

  # With runtime config for Python virtualenv
  skill_py <- Skill$new(skill_dir, runtime = list(
    python = list(setup = "source .venv/bin/activate")
  ))
}
```
