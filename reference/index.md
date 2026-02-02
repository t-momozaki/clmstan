# Package index

## Model Fitting

Main function for fitting cumulative link models

- [`clm_stan()`](https://t-momozaki.github.io/clmstan/reference/clm_stan.md)
  : Fit a Cumulative Link Model using CmdStanR
- [`clmstan-class`](https://t-momozaki.github.io/clmstan/reference/clmstan-class.md)
  : clmstan S3 Class

## Prior Specification

Functions for specifying prior distributions

- [`prior()`](https://t-momozaki.github.io/clmstan/reference/prior.md) :
  Specify Prior Distributions
- [`clm_prior()`](https://t-momozaki.github.io/clmstan/reference/clm_prior.md)
  : Prior Specification for clmstan
- [`normal()`](https://t-momozaki.github.io/clmstan/reference/normal.md)
  : Normal Distribution for Prior Specification
- [`gamma()`](https://t-momozaki.github.io/clmstan/reference/gamma.md) :
  Gamma Distribution for Prior Specification
- [`student_t()`](https://t-momozaki.github.io/clmstan/reference/student_t.md)
  : Student-t Distribution for Prior Specification
- [`cauchy()`](https://t-momozaki.github.io/clmstan/reference/cauchy.md)
  : Cauchy Distribution for Prior Specification
- [`flat()`](https://t-momozaki.github.io/clmstan/reference/flat.md) :
  Flat (Improper Uniform) Prior Distribution
- [`c(`*`<clm_prior_spec>`*`)`](https://t-momozaki.github.io/clmstan/reference/c.clm_prior_spec.md)
  : Combine Multiple Prior Specifications

## Model Information

Query available options

- [`supported_links()`](https://t-momozaki.github.io/clmstan/reference/supported_links.md)
  : Get supported link functions
- [`supported_thresholds()`](https://t-momozaki.github.io/clmstan/reference/supported_thresholds.md)
  : Get supported threshold structures
- [`link_functions`](https://t-momozaki.github.io/clmstan/reference/link_functions.md)
  : Available Link Functions

## S3 Methods

Methods for clmstan objects

- [`coef(`*`<clmstan>`*`)`](https://t-momozaki.github.io/clmstan/reference/coef.clmstan.md)
  : Extract coefficients from clmstan objects
- [`summary(`*`<clmstan>`*`)`](https://t-momozaki.github.io/clmstan/reference/summary.clmstan.md)
  : Summary method for clmstan objects
- [`print(`*`<clmstan>`*`)`](https://t-momozaki.github.io/clmstan/reference/print.clmstan.md)
  : Print method for clmstan objects
- [`print(`*`<summary.clmstan>`*`)`](https://t-momozaki.github.io/clmstan/reference/print.summary.clmstan.md)
  : Print method for summary.clmstan objects
- [`plot(`*`<clmstan>`*`)`](https://t-momozaki.github.io/clmstan/reference/plot.clmstan.md)
  : Plot method for clmstan objects

## Prediction

Prediction functions

- [`predict(`*`<clmstan>`*`)`](https://t-momozaki.github.io/clmstan/reference/predict.clmstan.md)
  : Predict method for clmstan objects
- [`fitted(`*`<clmstan>`*`)`](https://t-momozaki.github.io/clmstan/reference/fitted.clmstan.md)
  : Fitted values for clmstan objects
- [`posterior_predict.clmstan()`](https://t-momozaki.github.io/clmstan/reference/posterior_predict.clmstan.md)
  : Posterior predictive distribution for clmstan objects

## Model Comparison

Leave-one-out cross-validation and WAIC

- [`loo(`*`<clmstan>`*`)`](https://t-momozaki.github.io/clmstan/reference/loo.clmstan.md)
  : Leave-One-Out Cross-Validation for clmstan objects
- [`waic(`*`<clmstan>`*`)`](https://t-momozaki.github.io/clmstan/reference/waic.clmstan.md)
  : Widely Applicable Information Criterion for clmstan objects

## Diagnostics

MCMC convergence diagnostics

- [`diagnostics()`](https://t-momozaki.github.io/clmstan/reference/diagnostics.md)
  : MCMC Diagnostics for clmstan objects
- [`extract_acf()`](https://t-momozaki.github.io/clmstan/reference/extract_acf.md)
  : Extract ACF values from clmstan object

## Utilities

Utility functions

- [`is.clmstan()`](https://t-momozaki.github.io/clmstan/reference/is.clmstan.md)
  : Check if object is clmstan

## Print Methods

Print methods for prior objects

- [`print(`*`<clm_dist>`*`)`](https://t-momozaki.github.io/clmstan/reference/print.clm_dist.md)
  : Print method for clm_dist objects
- [`print(`*`<clm_prior>`*`)`](https://t-momozaki.github.io/clmstan/reference/print.clm_prior.md)
  : Print method for clm_prior objects
- [`print(`*`<clm_prior_list>`*`)`](https://t-momozaki.github.io/clmstan/reference/print.clm_prior_list.md)
  : Print method for clm_prior_list objects
- [`print(`*`<clm_prior_spec>`*`)`](https://t-momozaki.github.io/clmstan/reference/print.clm_prior_spec.md)
  : Print method for clm_prior_spec objects
