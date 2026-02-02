# Combine Multiple Prior Specifications

Combines multiple
[`prior()`](https://t-momozaki.github.io/clmstan/reference/prior.md)
objects into a single prior specification list.

## Usage

``` r
# S3 method for class 'clm_prior_spec'
c(...)
```

## Arguments

- ...:

  [`prior()`](https://t-momozaki.github.io/clmstan/reference/prior.md)
  objects to combine.

## Value

An object of class `"clm_prior_list"` containing all prior
specifications.

## Examples

``` r
# Combine multiple priors
priors <- c(
  prior(normal(0, 2.5), class = "b"),
  prior(normal(0, 10), class = "Intercept"),
  prior(gamma(2, 0.1), class = "df")
)
print(priors)
#> Prior specifications:
#>   1. b: normal(0, 2.5)
#>   2. Intercept: normal(0, 10)
#>   3. df: gamma(2, 0.1)
```
