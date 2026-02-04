# Describe an R object for use in `MCP` tools

A generic function used to describe an R object in a way that can be
safely serialized to JSON and sent to `MCP` clients. Unlike direct
serialization, this function handles special objects (environments,
external pointers, large objects) by generating human-readable
descriptions.

## Usage

``` r
mcp_describe(x, ..., max_print = 100)
```

## Arguments

- x:

  The object to describe.

- ...:

  Additional arguments passed down to underlying methods. Unused
  arguments are silently ignored.

- max_print:

  Maximum number of items to print for long objects. Default is 100.

## Value

A character vector of lines describing the object. If the object is a
simple scalar, may return a single-element character vector.

## Examples

``` r
# Describe a data frame
mcp_describe(mtcars)
#>  [1] "                   mpg cyl  disp  hp drat    wt  qsec vs am gear carb"
#>  [2] "Mazda RX4         21.0   6 160.0 110 3.90 2.620 16.46  0  1    4    4"
#>  [3] "Mazda RX4 Wag     21.0   6 160.0 110 3.90 2.875 17.02  0  1    4    4"
#>  [4] "Datsun 710        22.8   4 108.0  93 3.85 2.320 18.61  1  1    4    1"
#>  [5] "Hornet 4 Drive    21.4   6 258.0 110 3.08 3.215 19.44  1  0    3    1"
#>  [6] "Hornet Sportabout 18.7   8 360.0 175 3.15 3.440 17.02  0  0    3    2"
#>  [7] "Valiant           18.1   6 225.0 105 2.76 3.460 20.22  1  0    3    1"
#>  [8] "Duster 360        14.3   8 360.0 245 3.21 3.570 15.84  0  0    3    4"
#>  [9] "Merc 240D         24.4   4 146.7  62 3.69 3.190 20.00  1  0    4    2"
#> [10] "Merc 230          22.8   4 140.8  95 3.92 3.150 22.90  1  0    4    2"
#> [11] " [ reached 'max' / getOption(\"max.print\") -- omitted 23 rows ]"     

# Describe a function
mcp_describe(mean)
#> [1] "function (x, ...) "  "UseMethod(\"mean\")"

# Describe a matrix
mcp_describe(matrix(1:12, 3, 4))
#> [1] "     [,1] [,2] [,3] [,4]" "[1,]    1    4    7   10"
#> [3] "[2,]    2    5    8   11" "[3,]    3    6    9   12"
```
