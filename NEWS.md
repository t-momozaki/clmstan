# clmstan 0.1.2

## Bug fixes / Compatibility

* Updated compatibility with upcoming changes to `loo_compare()` output
  structure in the `loo` package (> 2.9.0), which now returns a data frame
  instead of a matrix and includes additional diagnostic columns.


# clmstan 0.1.1

Patch release addressing CRAN maintainer feedback.

## Bug fixes

* Suppressed misleading error message during package installation when
  'cmdstanr' is not available. Stan model compilation is now silently
  skipped with an informational note instead of an error-like message.

* Removed `SystemRequirements: CmdStan` from DESCRIPTION. CmdStan is not
  required to install or check the package; it is only needed at runtime
  for model fitting.

# clmstan 0.1.0

Initial CRAN release.

## Features

### Link Functions

* 5 standard link functions: logit, probit, cloglog, loglog, cauchit
* 6 flexible link functions with shape parameters:
  - t-link (df)
  - Aranda-Ordaz (lambda)
  - Symmetric Power (r, base)
  - log-gamma (lambda)
  - GEV (xi)
  - AEP (theta1, theta2)

### Threshold Structures

* flexible: Standard ordered thresholds (default)
* equidistant: Equal spacing between thresholds
* symmetric: Symmetric thresholds around zero

### Prior Specification

* Distribution-based API: `prior(normal(0, 2.5), class = "b")`
* Support for normal, student_t, cauchy, gamma, and flat priors
* Legacy API via `clm_prior()` for backward compatibility

### Model Comparison

* LOO-CV via `loo()`
* WAIC via `waic()`

### Diagnostics

* `diagnostics()` for MCMC convergence checks
* `plot()` with multiple types: trace, dens, hist, areas, intervals, acf

### Prediction

* `predict()` for class predictions
* `fitted()` for probability predictions
* `posterior_predict()` for posterior predictive samples
