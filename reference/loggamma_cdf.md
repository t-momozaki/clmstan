# Log-gamma link CDF

Based on the log-gamma distribution:

- lambda = 0: probit (normal)

- lambda \> 0: F(x) = pgamma(exp(x), shape = lambda, rate = lambda)

- lambda \< 0: F(x) = 1 - pgamma(exp(-x), shape = -lambda, rate =
  -lambda)

## Usage

``` r
loggamma_cdf(x, lambda)
```

## Arguments

- x:

  Numeric vector

- lambda:

  Shape parameter

## Value

CDF values

## Details

Reference: Prentice (1976) Biometrics
