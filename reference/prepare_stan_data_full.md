# Prepare data for full Stan model with link parameter inference

Prepare data for full Stan model with link parameter inference

## Usage

``` r
prepare_stan_data_full(
  formula,
  data,
  link = "logit",
  link_param = NULL,
  prior_beta_sd = 2.5,
  prior_c_sd = 10,
  link_prior = NULL
)
```

## Arguments

- formula:

  A formula specifying the model

- data:

  A data frame containing the variables

- link:

  Link function name

- link_param:

  A list of link parameters. Values can be:

  - numeric: Use as fixed value

  - "estimate": Estimate the parameter with default prior

- prior_beta_sd:

  Prior SD for regression coefficients (default: 2.5)

- prior_c_sd:

  Prior SD for cutpoints (default: 10)

- link_prior:

  A list of custom prior specifications for link parameters

## Value

A list suitable for passing to CmdStan (clm_full.stan)
