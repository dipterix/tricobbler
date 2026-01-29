# Project Rules

- **Language:** Always use R-language in files with `R` or `r` extensions.
- **Dependency:** Only use base-R and packages imported from `DESCRIPTION` for writing the package code
- **Formatting:** Use 2 spaces for indentation.
- **Documentation:** Every exported functions/classes must be documented with `roxygen2` with clear title, description, params, examples. Do not include `set.seed()` in any place. Mark internal/abstract classes with @keywords internal.

## Good Practices

- To test if the package is running, use `devtools::load_all()` with R before calling the package functions
- Use `adhoc` directory for temporary test scripts
- For unit tests, put them in `test/testthat` folder. The unit-test files must follow `testthat` package syntax and I can run `devtools::test()` to test them all
