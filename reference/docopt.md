# Parse Command-Line Arguments with Enhanced Error Messages

Wrapper around [`docopt`](https://rdrr.io/pkg/docopt/man/docopt.html)
that fixes path-parsing bugs (e.g. paths with parentheses) and prints
informative error messages including the full usage string so agents can
self-correct.

## Usage

``` r
docopt(doc, args = commandArgs(TRUE), ...)
```

## Arguments

- doc:

  character, usage specification string in
  [`docopt`](https://rdrr.io/pkg/docopt/man/docopt.html) format

- args:

  character vector, command-line arguments to parse (defaults to
  `commandArgs(TRUE)`)

- ...:

  additional arguments passed to
  [`docopt`](https://rdrr.io/pkg/docopt/man/docopt.html)

## Value

A named list of parsed arguments
