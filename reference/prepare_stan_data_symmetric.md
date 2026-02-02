# Prepare data for symmetric threshold Stan model

Creates a Stan data list for cumulative link models with symmetric
thresholds centered at 0: `c[k] = -c[K-k]`

## Usage

``` r
prepare_stan_data_symmetric(
  formula,
  data,
  link = "logit",
  link_param = NULL,
  prior_beta_sd = 2.5,
  prior_cpos_sd = 5
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

- prior_cpos_sd:

  Prior SD for positive thresholds (default: 5)

## Value

A list suitable for passing to CmdStan (clm_symmetric.stan)

## Details

Examples:

- K=4 (3 thresholds): c = (-a, 0, a)

- K=5 (4 thresholds): c = (-b, -a, a, b)

- K=6 (5 thresholds): c = (-b, -a, 0, a, b)
