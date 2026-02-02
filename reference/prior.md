# Specify Prior Distributions

Specify prior distributions for model parameters using distribution
functions.

## Usage

``` r
prior(prior, class = "b", coef = "")
```

## Arguments

- prior:

  A distribution object created by
  [`normal()`](https://t-momozaki.github.io/clmstan/reference/normal.md),
  [`gamma()`](https://t-momozaki.github.io/clmstan/reference/gamma.md),
  [`student_t()`](https://t-momozaki.github.io/clmstan/reference/student_t.md),
  or
  [`cauchy()`](https://t-momozaki.github.io/clmstan/reference/cauchy.md).

- class:

  The parameter class. Valid classes are:

  - `"b"`: Regression coefficients (beta)

  - `"Intercept"`: Cutpoints/thresholds (flexible)

  - `"c1"`: First cutpoint (equidistant)

  - `"d"`: Threshold interval (equidistant)

  - `"cpos"`: Positive cutpoints (symmetric)

  - `"df"`: Degrees of freedom (tlink)

  - `"lambda_ao"`: Lambda parameter (aranda_ordaz)

  - `"lambda_lg"`: Lambda parameter (log_gamma)

  - `"xi"`: Xi parameter (gev)

  - `"r"`: R parameter (sp)

  - `"theta1"`, `"theta2"`: Theta parameters (aep)

- coef:

  Optional coefficient name (for future extension).

## Value

An object of class `"clm_prior_spec"` representing the prior
specification.

## See also

[`normal()`](https://t-momozaki.github.io/clmstan/reference/normal.md),
[`gamma()`](https://t-momozaki.github.io/clmstan/reference/gamma.md),
[`student_t()`](https://t-momozaki.github.io/clmstan/reference/student_t.md),
[`cauchy()`](https://t-momozaki.github.io/clmstan/reference/cauchy.md),
[`clm_prior()`](https://t-momozaki.github.io/clmstan/reference/clm_prior.md)

## Examples

``` r
# Specify a normal prior for regression coefficients
prior(normal(0, 2.5), class = "b")
#> Prior: normal(0, 2.5)
#> Class: b

# Specify a gamma prior for degrees of freedom
prior(gamma(2, 0.1), class = "df")
#> Prior: gamma(2, 0.1)
#> Class: df

# Combine multiple priors
c(
  prior(normal(0, 2.5), class = "b"),
  prior(normal(0, 10), class = "Intercept")
)
#> Prior specifications:
#>   1. b: normal(0, 2.5)
#>   2. Intercept: normal(0, 10)
```
