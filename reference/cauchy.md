# Cauchy Distribution for Prior Specification

Creates a Cauchy distribution object for use with [`prior()`](prior.md).

## Usage

``` r
cauchy(mu = 0, sigma = 1)
```

## Arguments

- mu:

  Location parameter. Default: 0

- sigma:

  Scale parameter. Must be positive. Default: 1

## Value

An object of class `"clm_dist"` representing a Cauchy distribution.

## See also

[`prior()`](prior.md), [`normal()`](normal.md), [`gamma()`](gamma.md),
[`student_t()`](student_t.md)

## Examples

``` r
# Create a Cauchy prior (weakly informative)
cauchy(0, 2.5)
#> cauchy(0, 2.5) 

# Use with prior()
prior(cauchy(0, 2.5), class = "b")
#> Prior: cauchy(0, 2.5)
#> Class: b
```
