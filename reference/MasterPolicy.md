# Master Workflow Policy Defining Stages and Version

Concrete policy that defines the overall workflow version and the set of
allowed stages. Inherits from
[`BasePolicy`](http://dipterix.org/tricobbler/reference/BasePolicy.md).

## Usage

``` r
MasterPolicy(
  name = character(0),
  description = character(0),
  version = character(0),
  stages = c("triage", "planning", "executing"),
  parameters = list()
)
```

## Arguments

- name:

  character, name of the policy (non-blank)

- description:

  character, human-readable description

- version:

  character, version string in `"major.minor.patch"` format (e.g.,
  `"1.0.0"`)

- stages:

  character, non-empty vector of unique, lowercase stage names. Each
  element may contain only letters, digits, underscores, or dashes.
  Defaults to `c("triage", "planning", "executing")`

- parameters:

  list, additional free-form parameters for the workflow

## Details

### Stages as Workflow Vocabulary

The `stages` property defines the symbolic vocabulary of workflow phases
(e.g., `"triage"`, `"planning"`, `"executing"`). These are macro-level
phase names that must be implemented by at least one
[`StatePolicy`](http://dipterix.org/tricobbler/reference/StatePolicy.md)
object in the
[`Manifest`](http://dipterix.org/tricobbler/reference/Manifest.md).

The reserved stage names `"ready"`, `"error"`, and `"human"` cannot be
used because they are managed internally by the
[`Scheduler`](http://dipterix.org/tricobbler/reference/Scheduler.md).

### Stage Naming Conventions

- Stages are automatically converted to lowercase for consistency

- Must contain only letters (a-z), digits (0-9), underscores (`_`), or
  dashes (`-`)

- Must be unique (case-insensitive) within a workflow

- Cannot be blank or `NA`

### Immutability

`MasterPolicy` objects are immutable (S7 value semantics). Once created,
they serve as a stable reference for the
[`Manifest`](http://dipterix.org/tricobbler/reference/Manifest.md). Use
the `@` accessor to read properties (e.g., `policy@stages`).

## Examples

``` r
MasterPolicy(
  name = "example",
  version = "1.0.0",
  stages = c("idle", "triage", "planning"),
  parameters = list()
)
#> MasterPolicy (S7 class) - `example`(1.0.0)
#> Stages: idle, triage, planning
```
