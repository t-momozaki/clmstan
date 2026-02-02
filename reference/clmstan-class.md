# clmstan S3 Class

The `clmstan` class represents a fitted cumulative link model. It
contains the CmdStanR fit object and additional metadata.

## Slots

- fit:

  The CmdStanMCMC object from cmdstanr

- formula:

  The model formula

- data:

  The original data frame

- link:

  The link function used

- base:

  The base distribution (for SP link)

- threshold:

  The threshold structure

- link_param:

  Link parameter settings (for flexible links)

- full:

  TRUE if link parameters were estimated (Bayesian inference), FALSE if
  they were fixed at user-specified values

- K:

  Number of response categories (cached from data)

- N:

  Number of observations (extracted from data for efficiency)

- P:

  Number of predictors (extracted from design matrix)
