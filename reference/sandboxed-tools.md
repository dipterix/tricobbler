# Create file I/O tools in sand-box

Generate ellmer `ToolDef` objects that restrict all file operations to a
given root directory. Every path argument accepted by the returned tools
is *relative* to `root_path`; attempts to escape the sandbox (e.g. via
`..`) are rejected with an informative error that the `LLM` can act on.

`sandboxed_tools()` returns a named list of all four tools at once. The
individual helpers (`sandboxed_read_file`, `sandboxed_write_file`, etc.)
return a single `ToolDef` each and are useful when you only need a
subset.

## Usage

``` r
sandboxed_read_file(root_path, read_n_lines = 50L)

sandboxed_load_text(text, description, read_n_lines = 50L)

sandboxed_write_file(root_path)

sandboxed_walk_directory(root_path)

sandboxed_copy_file(root_path)

sandboxed_tools(root_path, read_n_lines = 50L)
```

## Arguments

- root_path:

  character, the directory to which every file operation is confined.
  The directory is created (recursively) if it does not already exist.

- read_n_lines:

  integer, default number of lines returned by the read tool when the
  caller does not specify `n_lines`.

- text:

  character, the text content to expose as a virtual file.

- description:

  character, a short human-readable label for the virtual file shown to
  the `LLM` in tool descriptions.

## Value

For `sandboxed_tools()`: a named list of four `ToolDef` objects
(`read_file`, `write_file`, `walk_directory`, `copy_file`). For the
individual helpers: a single `ToolDef`.

## Examples

``` r
root <- file.path(tempdir(), "sandbox-demo")
tools <- sandboxed_tools(root)
names(tools)
#> [1] "read_file"      "write_file"     "walk_directory" "copy_file"     

# Individual tool
read_tool <- sandboxed_read_file(root)
```
