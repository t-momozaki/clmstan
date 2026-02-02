# Symmetric Power (SP) link CDF

The SP link applies a power transformation to a base CDF:

## Usage

``` r
sp_cdf(x, r, base, df = 8)
```

## Arguments

- x:

  Numeric vector

- r:

  Power parameter (r \> 0)

- base:

  Base distribution name

- df:

  Degrees of freedom (for tlink base)

## Value

CDF values

## Details

For r \<= 1: F_sp(x) = F_0(x/r)^r For r \> 1: F_sp(x) = 1 -
F_0(-r\*x)^(1/r)

Special case: r = 1 gives the base distribution F_0.

Reference: Li et al. (2019) Environmetrics
