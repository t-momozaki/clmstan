# Summary method for clmstan objects

Summary method for clmstan objects

## Usage

``` r
# S3 method for class 'clmstan'
summary(object, probs = c(0.025, 0.5, 0.975), digits = 3, ...)
```

## Arguments

- object:

  A clmstan object

- probs:

  Quantile probabilities for credible intervals

- digits:

  Number of significant digits for display

- ...:

  Additional arguments (ignored)

## Value

An object of class "summary.clmstan" containing:

- coefficients: Posterior summary for regression coefficients

- thresholds: Posterior summary for threshold parameters

- beta0: Posterior summary for intercept

- model_info: Model metadata (formula, link, threshold, K, N, P)
