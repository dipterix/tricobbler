# Workflow YAML Serialization — Implementation Plan

## Goal

Add `tricobbler-workflow.yaml` format that stores manifest(s) + agent definitions in a single file.
Schedulers can save to and load from this format, drastically reducing boilerplate in orchestration scripts.

## YAML Schema

```yaml
workflows:
  - master:
      name: "sandboxed-import-bids-skill"
      description: "..."
      version: "0.1.0"
      parameters: { user_prompt: "...", skill_out_dir: "..." }
    stages:
      triage:
        - name: extract_paths
          agent_id: triage_chat
          description: "..."
          accessibility: none
          critical: yes
          priority: 200
          parameters:
            return_type: { type: object, properties: { ... } }
          depends_on: {}
        - name: create_scoped_tools
          agent_id: tool_maker
          ...
      executing:
        - name: create_skill
          agent_id: executor_chat
          ...

agents:
  - id: triage_chat
    type: chat
    provider: ollama
    model: "qwen3-coder:30b"
    base_url: "${ config$ollama$base_url %||% 'http://127.0.0.1:11434' }"
    params:
      max_tokens: 16384
    extra_args: {}
    extra_headers: []
  - id: tool_maker
    type: script
    source: "agents/tool_maker.R"
    function: "tool_maker_fn"
  - id: executor_chat
    type: chat
    provider: ollama
    model: "qwen3-coder:30b"
    base_url: "${ config$ollama$base_url %||% 'http://127.0.0.1:11434' }"
    params:
      max_tokens: 16384
  - id: validator
    type: script
    source: "agents/validator.R"
    function: "validate_skill_fn"
  - id: reviewer_chat
    type: script
    source: "agents/reviewer.R"
    function: "review_skill_wrapper"
```

### Key design choices

- `stages` is a **named list** (stage name -> array of states). Individual states omit
  the `stage` field (implied by parent key); it is injected back on read.
- `workflows` is a list supporting multiple workflows sharing an `agents` pool.
- `agents` section lives at the same level as `workflows`.

### Agent `type` values

| type | Description | Required fields |
|------|-------------|-----------------|
| `chat` | ellmer Chat object | `provider`, `model`; optional `base_url`, `params`, `extra_args`, `extra_headers` |
| `script` | Source an R file, get a function | `source` (relative to YAML), `function` (function name) |
| `tool_definition` | tricobbler MCP tool YAML | `tool` (path or name) |
| `package_function` | `"pkg::fun"` reference | `package_function` (e.g., `"utils::read.csv"`) |

### Provider name mapping

A lookup table maps `provider@name` (display) <-> `chat_*()` suffix:

```
"Anthropic"        <-> "anthropic"
"AWS/Bedrock"      <-> "aws_bedrock"
"Azure/OpenAI"     <-> "azure_openai"
"Cloudflare"       <-> "cloudflare"
"Databricks"       <-> "databricks"
"DeepSeek"         <-> "deepseek"
"gitHub"           <-> "github"
"Google/Gemini"    <-> "google_gemini"
"Google/Vertex"    <-> "google_vertex"
"Groq"             <-> "groq"
"HuggingFace"      <-> "huggingface"
"Mistral"          <-> "mistral"
"Ollama"           <-> "ollama"
"OpenAI"           <-> "openai"
"OpenRouter"       <-> "openrouter"
"Perplexity"       <-> "perplexity"
"PortkeyAI"        <-> "portkey"
"Snowflake/Cortex" <-> "snowflake"
"VLLM"             <-> "vllm"
```

Fallback when LUT misses: `tolower(gsub("[^a-zA-Z0-9]", "_", provider@name))`.

Provider `params` (from `ellmer::params()`) are serialized; `credentials` is
**never** serialized (env vars per ellmer convention).

---

## Implementation Steps

### 1. Add `config` property to Agent (R/class-baseagent.R)

New nullable `list | NULL` property, default `NULL`. Stores the serializable agent
configuration so `$save_workflow()` can recover it.

### 2. Create R/workflow-io.R

New file containing:

- **Provider LUT** — bidirectional mapping `provider@name` <-> chat suffix
- **`extract_chat_config(chat)`** — takes Chat object, returns config list via `chat$get_provider()`
- **`workflow_save(file, manifest, agents, append)`** — low-level write
  - `manifest`: a `Manifest` object (or list thereof)
  - `agents`: named list of agent config lists (Chat objects auto-converted via `extract_chat_config()`)
  - `append = TRUE`: merge into existing file by name/ID
- **`workflow_load(file, name)`** — low-level read
  - `name = NULL`: return available workflow names
  - `name = "..."`: return `list(manifest = Manifest, agents = list(Agent, ...))`
  - Agent reconstruction: `chat` -> `ellmer::chat()`, `script` -> `source()` + `as_agent()`,
    `tool_definition` -> `as_agent(path)`, `package_function` -> `as_agent("pkg::fun")`
- **`workflow_list(file)`** — alias for `workflow_load(file, name = NULL)`

### 3. Add `Scheduler$save_workflow()` method (R/class-scheduler_sync.R)

```r
save_workflow = function(skill_path, filename = "tricobbler-workflow.yaml", append = TRUE)
```

Extracts `self$manifest` + builds agent configs from `self$agents$as_list()` (using
`agent@config` when available). Calls `workflow_save()`.

### 4. Populate `agent@config` in as_agent dispatches (R/generic-as_agent.R)

- `as_agent_from_chat()`: set `agent@config <- extract_chat_config(chat)`
- `as_agent.function()`: accept optional `source` + `function_name` args, store as config
- `as_agent.character()`: store the original string + type in config

### 5. Extract function agents from adhoc/test-manifest-import-bids.R

Move to:
- `inst/skills/skill-creator/agents/tool_maker.R` — `tool_maker_fn()`
- `inst/skills/skill-creator/agents/validator.R` — `validate_skill_fn()`
- `inst/skills/skill-creator/agents/reviewer.R` — `review_skill_wrapper()` + embedded `make_chat_reviewer()`

### 6. Create workflow YAML

Write `inst/skills/skill-creator/tricobbler-workflow.yaml` with the full manifest
definition + agent definitions from the test script.

### 7. Simplify test script

Rewrite `adhoc/test-manifest-import-bids.R` to use `workflow_load()` + `Scheduler$new()`.

### 8. Add tests (tests/testthat/test-workflow-io.R)

- `workflow_save()` creates valid YAML with correct structure
- `workflow_load(file, NULL)` lists workflow names
- `workflow_load(file, name)` reconstructs Manifest correctly
- Agent reconstruction for each type
- Append/replace semantics
- Roundtrip preservation

### 9. Documentation & checks

- roxygen2 docs on all exported functions (`@export`, `@rdname workflow-io`)
- `devtools::document()` to update NAMESPACE
- `devtools::check()` clean
- `spelling::spell_check_package()` clean
- Update `_pkgdown.yml` reference section
