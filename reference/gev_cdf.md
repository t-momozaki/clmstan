# Generalized Extreme Value (GEV) link CDF

F(x; xi) = exp(-(1 + xi \* x)^(-1/xi))

## Usage

``` r
gev_cdf(x, xi)
```

## Arguments

- x:

  Numeric vector

- xi:

  Shape parameter

## Value

CDF values

## Details

Special cases:

- xi = 0: Gumbel (equals loglog)

- xi \< 0: Weibull (bounded above)

- xi \> 0: Frechet (heavy tail)

Reference: Wang & Dey (2011)
