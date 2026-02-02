# Create a clmstan object (internal constructor)

This is an internal constructor called by clm_stan(). Users should not
call this function directly.

## Usage

``` r
new_clmstan(
  fit,
  formula,
  data,
  link,
  base,
  threshold,
  link_param = NULL,
  full = FALSE,
  K,
  N,
  P
)
```

## Arguments

- fit:

  A CmdStanMCMC object

- formula:

  The model formula

- data:

  The original data

- link:

  The link function

- base:

  The base distribution (for SP link)

- threshold:

  The threshold structure

- link_param:

  Link parameter settings

- full:

  Whether full model was used

- K:

  Number of categories

- N:

  Number of observations

- P:

  Number of predictors

## Value

An object of class "clmstan"
