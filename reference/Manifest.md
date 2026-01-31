# Validated Container Linking Master Policy to State Policies

Container that ties a `MasterPolicy` together with a list of
`StatePolicy` objects. The validator ensures that every stage defined in
the master policy is represented by at least one state.

## Usage

``` r
Manifest(master = MasterPolicy(), states = list())
```

## Arguments

- master:

  `MasterPolicy` object.

- states:

  List of `StatePolicy` objects.

## Details

### TriCobbler's Three-Tier Architecture

The `Manifest` class represents the validated blueprint layer (Tier 1)
in TriCobbler's three-tier design:

1.  **Policy Layer (Tier 1 - Immutable S7)**: Blueprint definitions

    - `Manifest`: Validated container linking `MasterPolicy` to
      `StatePolicy` list

    - `MasterPolicy`: Workflow version + allowed stages (macro-level
      phases)

    - `StatePolicy`: Individual state metadata (stage, description,
      parameters, priority)

2.  **Contract Layer (Tier 2 - Immutable S7)**: Execution agreements

    - `Contract`: Master agreement linking `Manifest` to `StageContract`
      list

    - `StageContract`: Per-stage execution spec (executor, tools,
      validators)

    - `ContractExecutor`: Callable function wrapping LLM/script
      execution logic

3.  **State Layer (Tier 3 - Mutable R6)**: Runtime tracking

    - `ContractState`: Orchestrates all stage states and workflow
      progression

    - `SubContractState`: Tracks one stage's execution (status, I/O,
      timing)

### Stages vs States: Critical Distinction

- **Stages** (symbolic vocabulary): Workflow phase names defined in
  `MasterPolicy@stages` (e.g., "triage", "planning", "executing")

- **States** (concrete implementations): `StatePolicy` objects that
  reference stages and add execution metadata (description, parameters,
  priority)

- **Multiple states per stage**: Enables different execution patterns:

  - **Parallel**: Multiple states with equal priority (concurrent
    execution)

  - **Sequential**: Different priorities create ordered execution chains

  - **Critical gates**: Critical states enforce sequential fail-fast
    semantics

- **Validation rule**: Every stage in `MasterPolicy@stages` MUST have at
  least one corresponding `StatePolicy` (enforced by validator)

### Validation Rules

The `Manifest` validator performs critical cross-checks:

1.  **Completeness**: Every `MasterPolicy` stage has at least one
    `StatePolicy`

    - Prevents "orphaned" stages with no implementation

    - Error message: "Missing stages: ..."

2.  **Critical priority uniqueness**: Critical states cannot share
    priorities

    - If `StatePolicy@critical = TRUE`, no other state in the same stage
      can have the same `priority` value

    - Prevents ambiguity about which critical state blocks
      lower-priority states

    - Error message: "Critical state ... cannot share its priority with
      ..."

### Immutability and Serialization

Once created, `Manifest` objects are immutable (S7 value semantics),
providing a stable reference for `Contract` creation. Manifests can be
serialized to/from YAML for version control:

- `manifest_write(manifest, file)`: Save to human-readable YAML

- `manifest_read(file)`: Load with full validation

- All validation rules apply on deserialization

### Manifest vs Contract

**Important**: `Manifest` is NOT a `Contract`. It's purely a
policy-level blueprint that defines WHAT the workflow should do (stages
and states), not HOW it executes. The actual execution contract (with
executors, validators, tools, SLAs) is defined separately in the
`Contract` class.

## Examples

``` r
# Create a valid manifest
mp <- MasterPolicy(name = "example", version = "1.0.0",
                   stages = c("idle", "triage"), parameters = list())
sp1 <- StatePolicy(name = "state1", stage = "idle", description = "idle state")
sp2 <- StatePolicy(name = "state2", stage = "triage", description = "triage state")
mf <- Manifest(master = mp, states = list(sp1, sp2))
mf
#> Manifest (S7 class) - `example` (1.0.0)
#> 
#> Master policy stages: 
#>   idle, triage
#> 
#> State `idle`
#>   - State: `state1` (priority: 100)
#> State `triage`
#>   - State: `state2` (priority: 100)
```
