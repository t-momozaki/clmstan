# Log-log CDF

F(x) = exp(-exp(-x))

## Usage

``` r
loglog_cdf(x)
```

## Arguments

- x:

  Numeric vector

## Value

CDF values

## Details

This corresponds to the Gumbel (minimum) distribution. It is the
reflection of cloglog: loglog(x) = 1 - cloglog(-x)
