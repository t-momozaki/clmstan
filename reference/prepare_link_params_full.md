# Prepare link parameters for full Stan model

Determines which parameters to estimate vs. fix, and sets up priors.

## Usage

``` r
prepare_link_params_full(link, link_param = NULL, link_prior = NULL)
```

## Arguments

- link:

  Link function name

- link_param:

  A list of user-specified link parameters

- link_prior:

  A list of custom prior specifications

## Value

A list with estimation flags, fixed values, and prior hyperparameters
