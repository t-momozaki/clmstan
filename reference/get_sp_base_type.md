# Get base type number for SP link

Converts a base distribution name to the integer code used in Stan for
SP link.

## Usage

``` r
get_sp_base_type(base)
```

## Arguments

- base:

  A character string specifying the base distribution

## Value

An integer (1-6) representing the base type

## Details

Base type mapping (symmetric distributions only, per Li et al. 2019):

- 1: logit

- 2: probit

- 3: cauchit

- 4: tlink
