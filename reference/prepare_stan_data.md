# Prepare data for Stan model

Prepare data for Stan model

## Usage

``` r
prepare_stan_data(
  formula,
  data,
  link = "logit",
  link_param = NULL,
  prior_beta_sd = 2.5,
  prior_c_sd = 10
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

  A list of link parameters (for flexible links)

- prior_beta_sd:

  Prior SD for regression coefficients (default: 2.5)

- prior_c_sd:

  Prior SD for cutpoints (default: 10)

## Value

A list suitable for passing to CmdStan
