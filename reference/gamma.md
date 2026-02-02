# Gamma Distribution for Prior Specification

Creates a gamma distribution object for use with [`prior()`](prior.md).

## Usage

``` r
gamma(alpha, beta)
```

## Arguments

- alpha:

  Shape parameter of the gamma distribution. Must be positive.

- beta:

  Rate parameter of the gamma distribution. Must be positive.

## Value

An object of class `"clm_dist"` representing a gamma distribution.

## Note

This function masks
[`base::gamma()`](https://rdrr.io/r/base/Special.html). To use the base
gamma function, use
[`base::gamma()`](https://rdrr.io/r/base/Special.html) explicitly.

## See also

[`prior()`](prior.md), [`normal()`](normal.md),
[`student_t()`](student_t.md), [`cauchy()`](cauchy.md)

## Examples

``` r
# Create a gamma prior
gamma(2, 0.1)
#> gamma(2, 0.1) 

# Use with prior() for degrees of freedom
prior(gamma(2, 0.1), class = "df")
#> Prior: gamma(2, 0.1)
#> Class: df
```
