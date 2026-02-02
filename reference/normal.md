# Normal Distribution for Prior Specification

Creates a normal distribution object for use with [`prior()`](prior.md).

## Usage

``` r
normal(mu = 0, sigma = 1)
```

## Arguments

- mu:

  Mean of the normal distribution. Default: 0

- sigma:

  Standard deviation of the normal distribution. Must be positive.
  Default: 1

## Value

An object of class `"clm_dist"` representing a normal distribution.

## See also

[`prior()`](prior.md), [`gamma()`](gamma.md),
[`student_t()`](student_t.md), [`cauchy()`](cauchy.md)

## Examples

``` r
# Create a normal prior
normal(0, 2.5)
#> normal(0, 2.5) 

# Use with prior()
prior(normal(0, 2.5), class = "b")
#> Prior: normal(0, 2.5)
#> Class: b
```
