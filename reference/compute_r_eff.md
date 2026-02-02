# Compute relative effective sample size for log-likelihood

Compute relative effective sample size for log-likelihood

## Usage

``` r
compute_r_eff(object, log_lik, cores = 1)
```

## Arguments

- object:

  A clmstan object

- log_lik:

  Log-likelihood matrix (S x N)

- cores:

  Number of cores for parallel computation

## Value

A vector of relative effective sample sizes (length N)
