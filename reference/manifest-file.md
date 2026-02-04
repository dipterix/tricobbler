# Read or Write Manifest from or to a YAML File

Serialize and read `Manifest` objects to and from YAML files. This
enables version control and sharing of workflow policy definitions. The
resulting YAML files are human-readable and can be edited manually,
though changes must still pass validation when read back.

`manifest_write()` serializes a `Manifest` object to a YAML file.

`manifest_read()` reads a YAML file back into a validated `Manifest`
object, reconstructing the `MasterPolicy` and `StatePolicy` objects and
enforcing all validation rules (e.g., every stage must have a
corresponding state).

## Usage

``` r
manifest_write(x, file, ...)

manifest_read(file, ...)
```

## Arguments

- x:

  a `Manifest` object to serialize (for `manifest_write`)

- file:

  Character. Path to the YAML file (input for `manifest_read`, output
  for `manifest_write`).

- ...:

  Additional arguments passed to
  [`yaml::read_yaml()`](https://yaml.r-lib.org/reference/read_yaml.html)
  or
  [`yaml::write_yaml()`](https://yaml.r-lib.org/reference/write_yaml.html).

## Value

- `manifest_write()`: Invisibly returns the path to the written file.

- `manifest_read()`: A validated `Manifest` object

## Examples

``` r
# Create a manifest
mp <- MasterPolicy(
  name = "demo-workflow",
  version = "1.0.0",
  stages = c("idle", "working"),
  parameters = list(timeout = 300)
)
sp1 <- StatePolicy(
  name = "init", stage = "idle",
  description = "Initial state", agent_id = "agent_init"
)
sp2 <- StatePolicy(
  name = "process", stage = "working",
  description = "Processing state", agent_id = "agent_process"
)
manifest <- Manifest(master = mp, states = list(sp1, sp2))

# Write to temporary file
tmp <- tempfile(fileext = ".yaml")
manifest_write(manifest, tmp)

# Verify file exists
file.exists(tmp)
#> [1] TRUE

# Read it back
manifest2 <- manifest_read(tmp)

# Verify the name matches
manifest2@name == "demo-workflow"
#> [1] TRUE

# Clean up
unlink(tmp)
```
