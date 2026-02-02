# Aranda-Ordaz asymmetric link CDF

F(x; lambda) = 1 - (1 + exp(x))^(-lambda)

## Usage

``` r
aranda_ordaz_cdf(x, lambda)
```

## Arguments

- x:

  Numeric vector

- lambda:

  Shape parameter (lambda \> 0)

## Value

CDF values

## Details

Special cases:

- lambda = 1: logit

- lambda -\> 0: cloglog

Reference: Aranda-Ordaz (1981) Biometrika
