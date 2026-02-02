# Dispatch to appropriate data preparation function

Routes to the correct prepare function based on threshold structure and
whether link parameters are being estimated.

## Usage

``` r
prepare_stan_data_dispatch(
  formula,
  data,
  link,
  base,
  link_param,
  threshold,
  full,
  prior
)
```

## Arguments

- formula:

  A formula specifying the model

- data:

  A data frame containing the variables

- link:

  Link function name

- base:

  Base distribution for SP link

- link_param:

  A list of link parameters

- threshold:

  Threshold structure

- full:

  Whether to use full model (estimate link parameters)

- prior:

  A clm_prior object or NULL for default priors

## Value

A list suitable for passing to CmdStan
