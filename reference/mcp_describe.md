# Describe an R object for use in `MCP` tools

An S7 generic function used to describe an R object in a way that can be
safely serialized to JSON and sent to `MCP` clients. Unlike direct
serialization, this function handles special objects (environments,
external pointers, large objects) by generating human-readable
descriptions.

The maximum print size is controlled by the context cache setting
`tricobbler.mcp_describe.max_size` (default: 100 lines).

## Usage

``` r
mcp_describe(x, ...)
```

## Arguments

- x:

  The object to describe.

- ...:

  Additional arguments passed down to underlying methods. Unused
  arguments are silently ignored.

## Value

A character vector of lines describing the object. If the object is a
simple scalar, may return a single-element character vector. For
[`ellmer::Content`](https://ellmer.tidyverse.org/reference/Content.html)
objects, returns the object as-is.

## Examples

``` r
# Describe a data frame
mcp_describe(mtcars)
#> [1] "          mpg cyl disp  hp drat   wt  qsec vs am gear carb\nMazda RX4  21   6  160 110  3.9 2.62 16.46  0  1    4    4\n [ reached 'max' / getOption(\"max.print\") -- omitted 31 rows ]"

# Describe a function
mcp_describe(mean)
#> [1] "function (x, ...) "  "UseMethod(\"mean\")"

# Describe a matrix
mcp_describe(matrix(1:12, 3, 4))
#> [1] "     [,1] [,2] [,3] [,4]\n[1,]    1    4    7   10\n[2,]    2    5    8   11\n[3,]    3    6    9   12"
```
