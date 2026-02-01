# S7 & R6 Documentation Reference

## Critical Rule: @param vs @field

### S7 Classes: Use `@param`

**Why:** S7 constructors are R functions. Properties become function parameters.

```r
#' @title Dog Class
#' @description Represents a dog with name and age
#' @param name character, the dog's name
#' @param age numeric, the dog's age in years
#' @export
Dog <- S7::new_class(
  name = "Dog",
  properties = list(
    name = S7::class_character,
    age = S7::class_numeric
  )
)

# Usage: Dog(name = "Lola", age = 11)
# Constructor IS a function with name and age as parameters
```

**Access:** Use `@` operator (e.g., `dog@name`)

### R6 Classes: Use `@field`

**Why:** R6 classes define fields and methods, not function parameters.

```r
#' @title AgentContext
#' @description Manages execution environment and storage
#' @field id character, unique identifier
#' @field rootpath character, root directory path
#' @export
AgentContext <- R6::R6Class(
  "AgentContext",
  public = list(
    id = NULL,
    rootpath = NULL,
    
    #' @description Initialize context
    #' @param id character, identifier
    #' @param rootpath character, root path
    initialize = function(id, rootpath) {
      self$id <- id
      self$rootpath <- rootpath
    }
  )
)

# Usage: AgentContext$new(id = "ctx1", rootpath = "/path")
# Fields are properties, initialize() is a method with @param
```

**Access:** Use `$` operator (e.g., `context$id`)

## Complete Documentation Template

### S7 Exported Class

```r
#' @title ClassName
#' @description Brief description
#' @param prop1 type, description
#' @param prop2 type, description
#' @examples
#' ClassName(prop1 = "value", prop2 = 123)
#' @export
```

### S7 Internal/Abstract Class

```r
#' @title BaseClassName
#' @description Brief description
#' @param prop1 type, description
#' @keywords internal
```

### R6 Class with Methods

```r
#' @title ClassName
#' @description Brief description of class
#' @export
ClassName <- R6::R6Class(
  "ClassName",
  public = list(
    #' @field field1 type, description of public field
    field1 = NULL,
    
    #' @field field2 type, description of public field
    field2 = NULL,
    
    #' @description Initialize object
    #' @param field1 type, parameter description
    #' @param field2 type, parameter description
    initialize = function(field1, field2) {
      self$field1 <- field1
      self$field2 <- field2
    },
    
    #' @description Brief description of what method does
    #' @param arg1 type, argument description
    #' @param arg2 type, argument description
    #' @returns Description of return value
    method_name = function(arg1, arg2) {
      # implementation
    }
  ),
  active = list(
    #' @field computed_field type, description of active binding
    computed_field = function() {
      # getter implementation
    }
  )
)
```

## Quick Reference

| Class System | Constructor | Document With | Access | Example |
|--------------|-------------|---------------|--------|---------|
| S7           | Function    | `@param`      | `@`    | `dog@name` |
| R6           | `$new()`    | `@field`      | `$`    | `context$id` |

## Common Mistakes to Avoid

❌ **WRONG:** Documenting S7 properties with `@field`
```r
#' @field name Character  # NO! S7 uses @param
```

❌ **WRONG:** Documenting R6 fields with `@param`
```r
#' @param id Character  # NO! R6 uses @field (unless for initialize() method)
```

❌ **WRONG:** Mixing `@` and `$` operators
```r
s7_obj$property  # NO! Use s7_obj@property
r6_obj@field     # NO! Use r6_obj$field
```

## References

- S7 Documentation: https://rconsortium.github.io/S7/
- S7 Classes & Objects: https://rconsortium.github.io/S7/articles/classes-objects.html
- roxygen2 Documentation: https://roxygen2.r-lib.org/
