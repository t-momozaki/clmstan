# Extract log-likelihood matrix from clmstan object

Extract log-likelihood matrix from clmstan object

## Usage

``` r
extract_log_lik(object)
```

## Arguments

- object:

  A clmstan object

## Value

An S x N matrix of log-likelihood values, where S is the number of
posterior samples and N is the number of observations.
