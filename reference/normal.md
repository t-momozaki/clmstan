# Normal Distribution for Prior Specification

Creates a normal distribution object for use with
[`prior()`](https://t-momozaki.github.io/clmstan/reference/prior.md).

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

[`prior()`](https://t-momozaki.github.io/clmstan/reference/prior.md),
[`gamma()`](https://t-momozaki.github.io/clmstan/reference/gamma.md),
[`student_t()`](https://t-momozaki.github.io/clmstan/reference/student_t.md),
[`cauchy()`](https://t-momozaki.github.io/clmstan/reference/cauchy.md)

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
