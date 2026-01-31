# OutputValidator

Encapsulates a validation function for stage output. The function should
accept the stage output (and optionally the input) and return `TRUE` on
success or a character error message.

## Usage

``` r
OutputValidator(validate = function() NULL)
```

## Arguments

- validate:

  Function that validates the stage output; should return TRUE or an
  error message.
