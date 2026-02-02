# Prepare link parameters for Stan

Fills in default values for link parameters and validates them.

## Usage

``` r
prepare_link_params(link, link_param = NULL)
```

## Arguments

- link:

  Link function name

- link_param:

  A list of user-specified link parameters

## Value

A list with all link parameters (df, lambda, xi, r, base_type, theta1,
theta2)
