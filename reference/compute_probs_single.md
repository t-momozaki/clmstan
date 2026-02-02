# Compute category probabilities for a single draw

Compute category probabilities for a single draw

## Usage

``` r
compute_probs_single(c_vec, eta_vec, link, link_param, K)
```

## Arguments

- c_vec:

  Vector of K-1 cutpoints

- eta_vec:

  Vector of N linear predictors

- link:

  Link function name

- link_param:

  Link parameters (for this draw)

- K:

  Number of categories

## Value

Matrix of N x K probabilities
