# TriCobbler: A Manifesto

> *Three cobblers with their wits combined equal Zhuge Liang.*

## The Problem

AI agents are entering scientific workflows. Researchers use LLMs to classify data, generate narratives, review code, and orchestrate analysis pipelines. The tools to build individual agents are maturing rapidly — model providers (OpenAI, Anthropic, Ollama), protocol standards (MCP), and language-specific bindings make it straightforward to create a single capable agent.

What remains unsolved is **orchestrating multiple agents into workflows that meet the standards of scientific computing**: reproducible, auditable, sandboxed, and validated before execution. Current multi-agent frameworks (LangGraph, AutoGen, CrewAI) are built for software engineering. They optimize for flexibility and developer experience. None address:

1. **Reproducibility** — Can you re-run this workflow and guarantee the same execution structure?
2. **Auditability** — Can you prove what each agent saw, produced, and was allowed to access?
3. **Sandboxing** — Can you restrict what Agent A knows about Agent B's output?
4. **Pre-execution validation** — Can you verify the workflow is structurally sound before making any LLM call?

These are not optional for regulated industries (pharma, clinical research) or for scientific credibility.

## The Position

TriCobbler is not an agent framework. It does not compete with LLM bindings, MCP servers, or IDE-based coding assistants. It is a **compliance and reproducibility layer** that sits between your agents and your scientific pipeline.

- Agents come from elsewhere (LLM libraries, MCP servers, plain functions).
- Tools come from elsewhere (MCP protocol, package APIs).
- Models come from elsewhere (OpenAI, Anthropic, Ollama, local inference).

TriCobbler provides what none of them do: the guarantees that the workflow composed from these parts is **valid, sandboxed, reproducible, and auditable**.

## Core Principles

### 1. Immutable Policy, Mutable Runtime

A workflow is defined as an immutable blueprint (Manifest) that specifies stages, agents, priorities, dependencies, failure routes, and access scopes. This blueprint is validated at construction time and serializable for version control. The runtime that executes the blueprint is ephemeral and disposable.

**Why this matters:** The same Manifest always produces the same execution structure. The blueprint is the reproducible artifact. The runtime is the auditable record of what happened.

### 2. Agent-Level Access Control

Each agent in a workflow has an explicit access scope governing what it can read from the shared execution context. Agents do not implicitly share state. Information flows between agents only through declared dependencies and access policies.

**Why this matters:** In multi-agent workflows involving untrusted or third-party agents, you must be able to prove that Agent X could not access Data Y. No other framework provides this guarantee.

### 3. Validated Before Executed

Workflows are checked for structural correctness before any agent runs: dependency completeness, priority consistency, circular reference detection, reachability. If the blueprint is invalid, it fails at construction — not after three expensive LLM calls.

**Why this matters:** Scientific pipelines run in batch, often unattended. Catching structural errors at definition time prevents wasted compute and ambiguous failures.

### 4. Dual-Layer Audit Trail

Every execution produces two records: a public log (human-readable, redacted through agent-specific formatting policies) and a private attachment store (full objects, access-controlled, lifecycle-tracked in a database). The execution history is queryable, not just grep-able.

**Why this matters:** Regulatory compliance requires demonstrating not just what happened, but what information was available to each decision-maker (human or AI) at each step.

### 5. Leverage, Don't Reinvent

TriCobbler defers to the ecosystem for everything outside its core mandate:

| Responsibility | Owned by |
|---|---|
| LLM communication | Language-specific libraries, model providers |
| Tool discovery and execution | MCP protocol |
| Skill and knowledge packaging | MCP servers |
| **Workflow validation** | **TriCobbler** |
| **Agent access scoping** | **TriCobbler** |
| **Execution sandboxing** | **TriCobbler** |
| **Audit trail persistence** | **TriCobbler** |
| **Policy-driven orchestration** | **TriCobbler** |

When industry standards mature, TriCobbler adopts them. The package maintains the smallest possible surface area that delivers its unique guarantees.

## What TriCobbler Is Not

- **Not an LLM wrapper.** Use your language's LLM bindings.
- **Not a tool or skill platform.** Use MCP servers.
- **Not a coding assistant.** Use VS Code, Claude Code, or any IDE agent.
- **Not a general-purpose workflow engine.** Use established pipeline tools for deterministic workflows.

TriCobbler exists because multi-agent AI workflows need the same rigor that scientific computing demands of every other tool: know what ran, know what it saw, know it was valid, and be able to do it again.
