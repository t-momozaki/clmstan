# Unified CDF dispatcher for clmstan

Computes the cumulative distribution function (CDF) for any supported
link function. This is the R equivalent of `unified_F()` in Stan.

## Usage

``` r
clm_cdf(x, link, link_param = list())
```

## Arguments

- x:

  Numeric vector of values

- link:

  Link function name (character)

- link_param:

  List of link parameters (for flexible links)

## Value

CDF values F(x)
