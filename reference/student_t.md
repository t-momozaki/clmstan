# Student-t Distribution for Prior Specification

Creates a Student-t distribution object for use with
[`prior()`](prior.md).

## Usage

``` r
student_t(df = 3, mu = 0, sigma = 1)
```

## Arguments

- df:

  Degrees of freedom. Must be positive. Default: 3

- mu:

  Location parameter. Default: 0

- sigma:

  Scale parameter. Must be positive. Default: 1

## Value

An object of class `"clm_dist"` representing a Student-t distribution.

## See also

[`prior()`](prior.md), [`normal()`](normal.md), [`gamma()`](gamma.md),
[`cauchy()`](cauchy.md)

## Examples

``` r
# Create a Student-t prior with heavy tails
student_t(3, 0, 2.5)
#> student_t(3, 0, 2.5) 

# Use with prior()
prior(student_t(3, 0, 2.5), class = "b")
#> Prior: student_t(3, 0, 2.5)
#> Class: b
```
