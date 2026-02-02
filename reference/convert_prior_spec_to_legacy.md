# Convert Prior Specification to Internal Format

Internal function to convert distribution-based prior specifications to
the internal format used by apply_priors_to_stan_data().

## Usage

``` r
convert_prior_spec_to_legacy(prior)
```

## Arguments

- prior:

  A clm_prior_spec, clm_prior_list, or clm_prior object

## Value

A list with clm_prior class containing prior parameter values
