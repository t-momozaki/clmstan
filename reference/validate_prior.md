# Validate prior specification for a model

Checks that the prior specification is compatible with the model
settings.

## Usage

``` r
validate_prior(prior, threshold, link, link_param)
```

## Arguments

- prior:

  A clm_prior object or NULL

- threshold:

  The threshold structure

- link:

  The link function

- link_param:

  The link parameters

## Value

TRUE if valid, otherwise throws a warning
