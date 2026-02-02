# Package index

## Model Fitting

Main function for fitting cumulative link models

- [`clm_stan()`](clm_stan.md) : Fit a Cumulative Link Model using
  CmdStanR
- [`clmstan-class`](clmstan-class.md) : clmstan S3 Class

## Prior Specification

Functions for specifying prior distributions

- [`prior()`](prior.md) : Specify Prior Distributions
- [`clm_prior()`](clm_prior.md) : Prior Specification for clmstan
- [`normal()`](normal.md) : Normal Distribution for Prior Specification
- [`gamma()`](gamma.md) : Gamma Distribution for Prior Specification
- [`student_t()`](student_t.md) : Student-t Distribution for Prior
  Specification
- [`cauchy()`](cauchy.md) : Cauchy Distribution for Prior Specification
- [`flat()`](flat.md) : Flat (Improper Uniform) Prior Distribution
- [`c(`*`<clm_prior_spec>`*`)`](c.clm_prior_spec.md) : Combine Multiple
  Prior Specifications

## Model Information

Query available options

- [`supported_links()`](supported_links.md) : Get supported link
  functions
- [`supported_thresholds()`](supported_thresholds.md) : Get supported
  threshold structures
- [`link_functions`](link_functions.md) : Available Link Functions

## S3 Methods

Methods for clmstan objects

- [`coef(`*`<clmstan>`*`)`](coef.clmstan.md) : Extract coefficients from
  clmstan objects
- [`summary(`*`<clmstan>`*`)`](summary.clmstan.md) : Summary method for
  clmstan objects
- [`print(`*`<clmstan>`*`)`](print.clmstan.md) : Print method for
  clmstan objects
- [`print(`*`<summary.clmstan>`*`)`](print.summary.clmstan.md) : Print
  method for summary.clmstan objects
- [`plot(`*`<clmstan>`*`)`](plot.clmstan.md) : Plot method for clmstan
  objects

## Prediction

Prediction functions

- [`predict(`*`<clmstan>`*`)`](predict.clmstan.md) : Predict method for
  clmstan objects
- [`fitted(`*`<clmstan>`*`)`](fitted.clmstan.md) : Fitted values for
  clmstan objects
- [`posterior_predict.clmstan()`](posterior_predict.clmstan.md) :
  Posterior predictive distribution for clmstan objects

## Model Comparison

Leave-one-out cross-validation and WAIC

- [`loo(`*`<clmstan>`*`)`](loo.clmstan.md) : Leave-One-Out
  Cross-Validation for clmstan objects
- [`waic(`*`<clmstan>`*`)`](waic.clmstan.md) : Widely Applicable
  Information Criterion for clmstan objects

## Diagnostics

MCMC convergence diagnostics

- [`diagnostics()`](diagnostics.md) : MCMC Diagnostics for clmstan
  objects
- [`extract_acf()`](extract_acf.md) : Extract ACF values from clmstan
  object

## Utilities

Utility functions

- [`is.clmstan()`](is.clmstan.md) : Check if object is clmstan

## Print Methods

Print methods for prior objects

- [`print(`*`<clm_dist>`*`)`](print.clm_dist.md) : Print method for
  clm_dist objects
- [`print(`*`<clm_prior>`*`)`](print.clm_prior.md) : Print method for
  clm_prior objects
- [`print(`*`<clm_prior_list>`*`)`](print.clm_prior_list.md) : Print
  method for clm_prior_list objects
- [`print(`*`<clm_prior_spec>`*`)`](print.clm_prior_spec.md) : Print
  method for clm_prior_spec objects
