# OutputSchema

Describes the expected shape of a stage's output. It stores a named list
where each element is a character string describing the R type (e.g.,
"character", "numeric", "list").

## Usage

``` r
OutputSchema(schema = list())
```

## Arguments

- schema:

  Named list where each element is a single character string describing
  the R type of the corresponding output.
