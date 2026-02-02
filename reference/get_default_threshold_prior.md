# Get default priors for threshold parameters

Get default priors for threshold parameters

## Usage

``` r
get_default_threshold_prior(threshold)
```

## Arguments

- threshold:

  A character string specifying the threshold structure

## Value

A list with default prior specifications

## Details

Default priors:

- `flexible`: c ~ normal(0, prior_c_sd)

- `equidistant`: c1 ~ normal(0, 10), d ~ gamma(2, 0.5)

- `symmetric`: c_pos ~ normal(0, 5) (truncated to positive)
