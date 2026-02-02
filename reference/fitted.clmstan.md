# Fitted values for clmstan objects

Returns expected category probabilities for each observation. This is
equivalent to `predict(object, type = "probs", summary = TRUE)`.

## Usage

``` r
# S3 method for class 'clmstan'
fitted(
  object,
  newdata = NULL,
  summary = TRUE,
  robust = FALSE,
  probs = c(0.025, 0.975),
  ndraws = NULL,
  ...
)
```

## Arguments

- object:

  A `clmstan` object returned by
  [`clm_stan()`](https://t-momozaki.github.io/clmstan/reference/clm_stan.md).

- newdata:

  Optional data frame for prediction. If `NULL` (default), predictions
  are made for the original training data.

- summary:

  Logical. If `TRUE` (default), return summary statistics (mean, SD,
  quantiles). If `FALSE`, return raw posterior draws.

- robust:

  Logical. If `TRUE`, use median instead of mean for point estimates.
  Default is `FALSE`.

- probs:

  Numeric vector of probabilities for quantiles. Default is
  `c(0.025, 0.975)` for 95% credible intervals.

- ndraws:

  Number of posterior draws to use. If `NULL` (default), all available
  draws are used.

- ...:

  Additional arguments (currently ignored).

## Value

If `summary = TRUE` (default): A data frame with N rows and columns for
each category probability (`P[Y=1]`, `P[Y=2]`, etc.). If
`summary = FALSE`: An S x N x K array of probability draws.

## See also

[`predict.clmstan()`](https://t-momozaki.github.io/clmstan/reference/predict.clmstan.md),
[`posterior_predict.clmstan()`](https://t-momozaki.github.io/clmstan/reference/posterior_predict.clmstan.md)
