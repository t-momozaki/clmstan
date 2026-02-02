# Prepare data for equidistant threshold Stan model

Creates a Stan data list for cumulative link models with equidistant
(equally spaced) thresholds: c_k = c_1 + (k-1) \* d

## Usage

``` r
prepare_stan_data_equidistant(
  formula,
  data,
  link = "logit",
  link_param = NULL,
  prior_beta_sd = 2.5,
  prior_c1_mu = 0,
  prior_c1_sd = 10,
  prior_d_alpha = 2,
  prior_d_beta = 0.5
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

- prior_c1_mu:

  Prior mean for first threshold c1 (default: 0)

- prior_c1_sd:

  Prior SD for first threshold c1 (default: 10)

- prior_d_alpha:

  Gamma prior shape for interval d (default: 2)

- prior_d_beta:

  Gamma prior rate for interval d (default: 0.5)

## Value

A list suitable for passing to CmdStan (clm_equidistant.stan)
