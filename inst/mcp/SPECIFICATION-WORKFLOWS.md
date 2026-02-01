# RAVE MCP Workflows YAML Specification

This document describes the YAML format specification for RAVE MCP (Model Context Protocol) workflows. Workflows define structured guidance for AI assistants, including tool usage patterns, job sequences, best practices, and examples.

## Overview

MCP workflows are YAML files that guide AI assistants in using MCP tools effectively. They provide:
- Tool discovery and usage guidance
- Multi-step job definitions with dependencies
- Best practices and warnings
- Concrete examples with expected conversation flows

Workflows are typically used to:
1. Generate system prompts for AI chat sessions
2. Define reusable patterns for common tasks
3. Document tool usage sequences and dependencies
4. Provide validation callbacks for tool execution order

## YAML File Structure

Each MCP workflow is defined in a separate YAML file with the following structure:

```yaml
# ============================================================
# METADATA (required: name)
# ============================================================
name: workflow_name
description: "Brief description of the workflow"
version: "1.0.0"
category: guide
tags:
  - tag1
  - tag2

# ============================================================
# TOOLS
# ============================================================
mcp_tools: true  # or false, "auto", "all", or ["tool1", "tool2"]

tool_guide:
  - tool: "pkg-function_name"
    category: category_name
    when: "When to use this tool"
    notes: "Additional usage notes"
    dangerous: false
    requires_approval: false
    preconditions:
      - "Condition that must be met"

# ============================================================
# GUIDANCE
# ============================================================
overview: |
  Multi-line overview text describing the workflow's purpose
  and general guidance.

best_practices:
  - title: "Practice Title"
    do: |
      What you should do
    dont: "What you should avoid"

warnings:
  - "Warning message 1"
  - "Warning message 2"

# ============================================================
# JOBS
# ============================================================
jobs:
  job-name:
    name: "Human-Readable Job Name"
    description: "What this job accomplishes"
    if: "condition_expression"
    strategy:
      parallel: false
      max_concurrent: 4
    steps:
      - name: "Step name"
        tool: "pkg-function_name"
        description: "What this step does"
        with:
          param1: value1
        loop:
          over: "collection"
          item: "item_name"
          until: "condition"
          interval: "5 seconds"
        validation:
          check: "validation expression"
          on_fail: "error"
        dangerous: false
        requires_approval: false

# ============================================================
# EXAMPLES
# ============================================================
examples:
  - trigger: "User says something like this"
    flow:
      - tool: "pkg-function_name"
        says: "Assistant response while calling tool"
        with:
          param1: value1
      - action: "action_name"
        says: "Response for non-tool action"

# ============================================================
# SETTINGS
# ============================================================
settings:
  dangerous: false
  requires_approval: false
  estimated_duration: "5 minutes"
```

---

## Field Descriptions

### Metadata Fields

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `name` | string | **Yes** | Unique identifier for the workflow (snake_case recommended) |
| `description` | string | No | Brief description of the workflow's purpose |
| `version` | string | No | Semantic version (e.g., "1.0.0") |
| `category` | string | No | Workflow category (e.g., "guide", "analysis", "setup") |
| `tags` | string[] | No | Array of tags for discovery and filtering |

### Tools Section

#### `mcp_tools`

Controls which MCP tools are loaded with the workflow:

| Value | Behavior |
|-------|----------|
| `true` | Auto-detect tools from `jobs`, `tool_guide`, and `examples` |
| `false` | Load no tools |
| `"auto"` | Same as `true` |
| `"all"` | Same as `true` |
| `["tool1", "tool2"]` | Load only the specified tools |

#### `tool_guide`

Array of tool usage guidance entries. Each entry provides context about when and how to use a specific tool:

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `tool` | string | **Yes** | Tool name in format `pkg-function_name` |
| `category` | string | No | Logical grouping (e.g., "discovery", "execution", "monitoring") |
| `when` | string | No | When to use this tool |
| `notes` | string | No | Additional usage notes or tips |
| `dangerous` | boolean | No | Whether tool performs destructive/irreversible actions |
| `requires_approval` | boolean | No | Whether user approval is needed before execution |
| `preconditions` | string[] | No | Conditions that must be met before using the tool |

### Guidance Section

#### `overview`

Multi-line string providing general guidance about the workflow. Typically describes:
- The system or domain the workflow operates in
- General capabilities enabled by the workflow
- Important context for AI assistants

#### `best_practices`

Array of best practice entries:

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `title` | string | **Yes** | Short title for the practice |
| `do` | string | **Yes** | What to do (recommended approach) |
| `dont` | string | **Yes** | What to avoid (anti-patterns) |

#### `warnings`

Array of warning strings. Critical information that should be prominently displayed:
- Safety considerations
- Common pitfalls
- Required user interactions

### Jobs Section

Jobs define multi-step workflows with dependencies and execution control.

#### Job Definition

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `name` | string | No | Human-readable job name |
| `description` | string | No | What the job accomplishes |
| `if` | string | No | Condition expression for job execution |
| `strategy` | object | No | Execution strategy configuration |
| `steps` | array | **Yes** | Array of step definitions |

#### Strategy Object

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `parallel` | boolean | `false` | Whether steps can run in parallel |
| `max_concurrent` | integer | No limit | Maximum concurrent step executions |

#### Step Definition

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `name` | string | No | Step identifier/name |
| `tool` | string | Conditional | Tool to call (mutually exclusive with `action`) |
| `action` | string | Conditional | Non-tool action (e.g., "filter", "validate", "present") |
| `description` | string | No | What this step does |
| `with` | object | No | Parameters to pass to tool/action |
| `loop` | object | No | Loop configuration |
| `validation` | object | No | Validation configuration |
| `dangerous` | boolean | No | Whether step is dangerous |
| `requires_approval` | boolean | No | Whether step requires user approval |

**Note**: Each step must have either `tool` or `action`, but not both.

#### Loop Configuration

| Field | Type | Description |
|-------|------|-------------|
| `over` | string | Collection to iterate over |
| `item` | string | Variable name for current item |
| `until` | string | Condition to stop looping |
| `interval` | string | Delay between iterations (e.g., "5 seconds") |

**Loop types**:
- **Collection loop**: Uses `over` (and optionally `item`) to iterate
- **Condition loop**: Uses `until` (and optionally `interval`) to poll

#### Validation Configuration

| Field | Type | Description |
|-------|------|-------------|
| `check` | string | Validation expression or description |
| `on_fail` | string | Action on failure: `"error"`, `"warn"`, `"skip"` |

### Examples Section

Concrete examples showing expected conversation flows:

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `trigger` | string | **Yes** | User message that triggers this flow |
| `flow` | array | **Yes** | Array of flow steps |

#### Flow Step

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `tool` | string | Conditional | Tool being called (mutually exclusive with `action`) |
| `action` | string | Conditional | Non-tool action description |
| `says` | string | No | What the assistant says during/after this step |
| `with` | object | No | Parameters for the tool call |

### Settings Section

Global workflow settings:

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `dangerous` | boolean | `false` | Whether the workflow as a whole is dangerous |
| `requires_approval` | boolean | `false` | Whether the workflow requires user approval |
| `estimated_duration` | string | No | Expected execution time (e.g., "5 minutes") |

---

## Tool Name Format

Tool names follow the pattern: `{package}-{function_name}`

- First hyphen separates package from function
- Function names may contain underscores
- Examples:
  - `ravepipeline-mcp_tool_pipeline_load`
  - `ravepipeline-mcp_tool_pipeline_list`

---

## Action Types

Actions represent non-tool operations in job steps or example flows:

| Action | Description |
|--------|-------------|
| `filter` | Filter/parse previous results |
| `validate` | Validate parameters or state |
| `present` | Present information to user |
| `confirm` | Request user confirmation |
| `evaluate` | Evaluate/compare results |
| `aggregate` | Aggregate data across iterations |
| `suggest` | Suggest corrections or next steps |
| `parse_error` | Parse and categorize errors |
| `await_approval` | Wait for explicit user approval |
| `filter_and_present` | Combined filter and present |

---

## Validation

The `mcpflow_validate_yaml()` function performs strict schema validation:

### Required Fields
- `name` must be a non-empty string

### Type Validation
- `description`: string
- `version`: string
- `category`: string
- `tags`: array of strings
- `mcp_tools`: boolean, string, or array of strings
- `tool_guide`: array of objects
- `overview`: string
- `best_practices`: array of objects
- `warnings`: array of strings
- `jobs`: named list/object
- `examples`: array of objects
- `settings`: object

### Tool Reference Validation
When `available_tools` is provided to `mcpflow_validate()`, all tool references are validated:
- Tools in `jobs` steps
- Tools in `tool_guide` entries
- Tools in `examples` flow steps

---

## File Naming Convention

Workflow YAML files are named using the pattern:

```
{workflow_name}.yaml
```

Example: `rave_pipeline_class_guide.yaml`

---

## Output Directory

Workflow files are stored in:

```
inst/mcp/workflows/
```

---

## R Functions

### Loading Workflows

```r
# List available workflows
mcpflow_list("ravepipeline")

# Read a workflow
wf <- mcpflow_read("ravepipeline::rave_pipeline_class_guide")
wf <- mcpflow_read("/path/to/workflow.yaml")

# Load all workflows from package
workflows <- mcpflow_load_all("ravepipeline")
```

### Validation

```r
# Lightweight validation
result <- mcpflow_validate(workflow)

# Strict YAML schema validation
result <- mcpflow_validate(workflow, strict = TRUE)

# With tool reference checking
result <- mcpflow_validate(workflow, available_tools = c("pkg-tool1", "pkg-tool2"))
```

### Export

```r
# Export to markdown
mcpflow_write(workflow, "output.md", method = "markdown")

# Export to YAML
mcpflow_write(workflow, "output.yaml", method = "yaml")

# Export workflows and tools
mcpflow_export(workflows, output_dir = "export/", format = "both", save_tools = TRUE)
```

### Instantiation

```r
# Create chat with workflow
chat <- mcpflow_instantiate(workflow, chat_provider = "ollama")

# With job validation
chat <- mcpflow_instantiate(workflow, use_job_validator = TRUE)
```

---

## Markdown Conversion

Workflows can be converted to Markdown for use as system prompts. The `convert_workflow_to_markdown()` function generates:

1. **Header**: Workflow name
2. **Description**: Workflow description
3. **Overview**: General guidance text
4. **Tool Guide**: Formatted tool usage guidance
5. **Best Practices**: Do/Don't recommendations
6. **Warnings**: Critical alerts
7. **Jobs**: Step-by-step job definitions
8. **Examples**: Trigger/response patterns

---

## Complete Example

```yaml
name: data_analysis_workflow
description: "Workflow for analyzing scientific data"
version: "1.0.0"
category: analysis
tags:
  - data-science
  - statistics

mcp_tools: true

tool_guide:
  - tool: "mypackage-load_data"
    category: setup
    when: "User wants to load a dataset"
    preconditions:
      - "File path is provided"

  - tool: "mypackage-run_analysis"
    category: execution
    when: "Data is loaded and parameters are set"
    dangerous: true
    requires_approval: true

overview: |
  This workflow guides data analysis tasks.
  It supports loading data, configuring analysis parameters,
  and executing statistical analyses.

best_practices:
  - title: "Validate Data First"
    do: |
      1. Load the data
      2. Check for missing values
      3. Verify data types
    dont: "Run analysis on unchecked data"

warnings:
  - "Large datasets may take significant time to process"
  - "Always backup data before destructive operations"

jobs:
  load-and-analyze:
    name: "Load and Analyze Data"
    description: "Complete workflow from loading to results"
    steps:
      - name: "Load data"
        tool: "mypackage-load_data"
        with:
          validate: true

      - name: "Run analysis"
        tool: "mypackage-run_analysis"
        requires_approval: true

      - name: "Present results"
        action: "present"
        description: "Show analysis results with interpretation"

examples:
  - trigger: "Analyze my data file"
    flow:
      - tool: "mypackage-load_data"
        says: "Loading your data file..."

      - tool: "mypackage-run_analysis"
        says: "Running the analysis. This may take a moment..."

      - action: "present"
        says: "Here are the results with interpretation..."

settings:
  dangerous: false
  requires_approval: false
  estimated_duration: "2-5 minutes"
```
