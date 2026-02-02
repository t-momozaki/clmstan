# Predict method for clmstan objects

Generates predictions from a fitted cumulative link model.

## Usage

``` r
# S3 method for class 'clmstan'
predict(
  object,
  newdata = NULL,
  type = c("class", "probs"),
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

- type:

  Type of prediction:

  - `"class"`: Predicted category (most likely class)

  - `"probs"`: Predicted probabilities for each category

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

Depending on `type` and `summary`:

- `type = "class"`, `summary = TRUE`: A data frame with columns
  `Estimate` (mean/median predicted class), `Est.Error` (SD), quantile
  columns, and `Class` (modal predicted category).

- `type = "class"`, `summary = FALSE`: An S x N integer matrix of
  predicted categories (1 to K), where S is the number of posterior
  draws and N is the number of observations.

- `type = "probs"`, `summary = TRUE`: A data frame with columns for each
  category probability (`P[Y=1]`, `P[Y=2]`, etc.).

- `type = "probs"`, `summary = FALSE`: An S x N x K array of predicted
  probabilities.

## See also

[`fitted.clmstan()`](https://t-momozaki.github.io/clmstan/reference/fitted.clmstan.md)
for expected probabilities,
[`posterior_predict.clmstan()`](https://t-momozaki.github.io/clmstan/reference/posterior_predict.clmstan.md)
for posterior predictive samples.
