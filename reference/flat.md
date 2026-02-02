# Flat (Improper Uniform) Prior Distribution

Creates a flat (improper uniform) distribution object for use with
[`prior()`](https://t-momozaki.github.io/clmstan/reference/prior.md). A
flat prior assigns equal probability density to all values, which is
improper (does not integrate to 1) but can be used when the likelihood
provides sufficient information for identification.

## Usage

``` r
flat()
```

## Value

An object of class `"clm_dist"` representing a flat distribution.

## Note

Flat priors are supported for:

- Regression coefficients (class "b")

- Threshold classes ("Intercept", "c1", "cpos")

Using flat priors may lead to improper posteriors if the likelihood does
not provide sufficient information. For thresholds with ordered
constraints, Stan's internal transformation provides implicit
regularization.

## See also

[`prior()`](https://t-momozaki.github.io/clmstan/reference/prior.md),
[`normal()`](https://t-momozaki.github.io/clmstan/reference/normal.md),
[`student_t()`](https://t-momozaki.github.io/clmstan/reference/student_t.md),
[`cauchy()`](https://t-momozaki.github.io/clmstan/reference/cauchy.md)

## Examples

``` r
# Create a flat prior for regression coefficients
prior(flat(), class = "b")
#> Prior: flat()
#> Class: b

# Flat prior for thresholds (flexible)
prior(flat(), class = "Intercept")
#> Prior: flat()
#> Class: Intercept
```
