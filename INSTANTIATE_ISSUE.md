# instantiate GitHub Issue: include_paths Support Request

## Issue Status

- **Repository**: [wlandau/instantiate](https://github.com/wlandau/instantiate)
- **Issue**: [#33](https://github.com/wlandau/instantiate/issues/33)
- **Status**: Submitted
- **Checked for duplicates**: 2026-01-30 (No existing issues found)

---

## Issue Title

```
stan_package_model() does not set include_paths on returned CmdStanModel
```

---

## Issue Body

### Description

When using `#include` directives in Stan files within an R package built with `instantiate`, the `stan_package_model()` function returns a CmdStanModel object without `include_paths` set. This causes errors when cmdstanr internally calls `stanc` to parse the Stan file for variable information.

### Reproducible Example

**Package structure:**
```
mypkg/
├── src/stan/
│   ├── my_model.stan          # Contains: #include functions/helper.stan
│   └── functions/
│       └── helper.stan        # Helper functions
```

**my_model.stan:**
```stan
functions {
  #include functions/helper.stan
}
// ... rest of model
```

**R code:**
```r
# After installing the package
library(mypkg)

model <- instantiate::stan_package_model(
  name = "my_model",
  package = "mypkg"
)

# This fails with:
# Error: Include file 'functions/helper.stan' not found
```

### Root Cause

When `stan_package_model()` is called, it returns a CmdStanModel object with the compiled executable. However, when cmdstanr later needs to parse the original `.stan` file (e.g., for `$variables()` or preparing data), it calls `stanc` without the necessary `include_paths` argument. Since `#include` directives are relative to the Stan file's directory, the parser cannot resolve them.

### Current Workaround

We are currently using a workaround that accesses cmdstanr's private R6 field:

```r
model <- instantiate::stan_package_model(name = "my_model", package = "mypkg")

# Workaround: manually set include_paths
stan_dir <- system.file("bin", "stan", package = "mypkg")
model$.__enclos_env__$private$include_paths_ <- stan_dir
```

This workaround:
- Accesses cmdstanr's internal implementation
- May break with future cmdstanr versions
- Is not documented or officially supported

### Proposed Solution

Could `stan_package_model()` automatically set `include_paths` on the returned CmdStanModel object to the directory containing the compiled models (typically `bin/stan/`)?

Alternatively, accept an `include_paths` argument that gets passed to the CmdStanModel:

```r
model <- instantiate::stan_package_model(
  name = "my_model",
  package = "mypkg",
  include_paths = system.file("bin", "stan", package = "mypkg")  # New argument
)
```

### Environment

| Component | Version |
|-----------|---------|
| R | 4.5.1 |
| instantiate | 0.2.3 |
| cmdstanr | 0.9.0 |
| CmdStan | 2.38.0 |
| OS | macOS (Darwin 25.2.0, arm64) |

### Related

- This issue affects any package using `#include` directives with instantiate
- The compilation itself works correctly (include_paths is properly set during compilation)
- The issue only manifests when cmdstanr needs to re-parse the Stan file at runtime

---

## Notes

### Before Submitting

- [x] Searched existing issues - no duplicates found
- [x] Prepared reproducible example
- [x] Collected environment information
- [x] Submit issue to GitHub

### Workaround Location in clmstan

The workaround is implemented in:
- File: `R/clm_stan.R`
- Lines: 106-127

```r
# Set include_paths for #include directives
stan_dir <- system.file("bin", "stan", package = "clmstan")
tryCatch(
  {
    model$.__enclos_env__$private$include_paths_ <- stan_dir
  },
  error = function(e) {
    warning(
      "Failed to set include_paths on CmdStanModel. ",
      "This may be due to a cmdstanr version change. ",
      "If sampling fails, please report this issue to the clmstan maintainer.\n",
      "Original error: ", conditionMessage(e)
    )
  }
)
```

### Timeline

| Date | Action |
|------|--------|
| 2026-01-30 | Issue documented, workaround implemented |
| 2026-01-30 | [Issue #33](https://github.com/wlandau/instantiate/issues/33) submitted to GitHub |
| TBD | Response from maintainer |
