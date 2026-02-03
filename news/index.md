# Changelog

## clmstan 0.1.0

Initial CRAN release.

### Features

#### Link Functions

- 5 standard link functions: logit, probit, cloglog, loglog, cauchit
- 6 flexible link functions with shape parameters:
  - t-link (df)
  - Aranda-Ordaz (lambda)
  - Symmetric Power (r, base)
  - log-gamma (lambda)
  - GEV (xi)
  - AEP (theta1, theta2)

#### Threshold Structures

- flexible: Standard ordered thresholds (default)
- equidistant: Equal spacing between thresholds
- symmetric: Symmetric thresholds around zero

#### Prior Specification

- Distribution-based API: `prior(normal(0, 2.5), class = "b")`
- Support for normal, student_t, cauchy, gamma, and flat priors
- Legacy API via
  [`clm_prior()`](https://t-momozaki.github.io/clmstan/reference/clm_prior.md)
  for backward compatibility

#### Model Comparison

- LOO-CV via [`loo()`](https://mc-stan.org/loo/reference/loo.html)
- WAIC via [`waic()`](https://mc-stan.org/loo/reference/waic.html)

#### Diagnostics

- [`diagnostics()`](https://t-momozaki.github.io/clmstan/reference/diagnostics.md)
  for MCMC convergence checks
- [`plot()`](https://rdrr.io/r/graphics/plot.default.html) with multiple
  types: trace, dens, hist, areas, intervals, acf

#### Prediction

- [`predict()`](https://rdrr.io/r/stats/predict.html) for class
  predictions
- [`fitted()`](https://rdrr.io/r/stats/fitted.values.html) for
  probability predictions
- `posterior_predict()` for posterior predictive samples
