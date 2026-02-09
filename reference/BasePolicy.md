# Abstract Base Class for Policy Objects

Internal abstract base class for all policy objects. It defines common
properties `name` and `description` that are shared by concrete
subclasses such as
[`MasterPolicy`](http://dipterix.org/tricobbler/reference/MasterPolicy.md)
and
[`StatePolicy`](http://dipterix.org/tricobbler/reference/StatePolicy.md).
This class is not exported and is intended for internal use only.

## Usage

``` r
BasePolicy(name = character(0), description = character(0))
```

## Arguments

- name:

  character, must be a non-blank single string

- description:

  character, human-readable description; multiple values are collapsed
  into a single space-separated string
