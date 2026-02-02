# Compute category probabilities for all draws

Compute category probabilities for all draws

## Usage

``` r
compute_probs_all(draws, X, link, base_link_param, K, full = FALSE)
```

## Arguments

- draws:

  Output from extract_prediction_draws()

- X:

  Design matrix (N x P)

- link:

  Link function name

- base_link_param:

  Fixed link parameters

- K:

  Number of categories

- full:

  Whether link parameters were estimated

## Value

Array of S x N x K probabilities
