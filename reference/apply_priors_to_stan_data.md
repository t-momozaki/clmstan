# Apply user priors to Stan data

Merges user-specified priors with default values and updates the Stan
data list.

## Usage

``` r
apply_priors_to_stan_data(stan_data, prior, threshold, full = FALSE)
```

## Arguments

- stan_data:

  A list of Stan data prepared by prepare_stan_data\_\*()

- prior:

  A clm_prior object or NULL

- threshold:

  The threshold structure ("flexible", "equidistant", "symmetric")

- full:

  Whether this is a full model (with link parameter estimation)

## Value

Updated stan_data list with prior values
