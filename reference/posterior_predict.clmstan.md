# Posterior predictive distribution for clmstan objects

Draws from the posterior predictive distribution. For each posterior
sample, a predicted category is sampled from the categorical
distribution with the predicted probabilities.

## Usage

``` r
posterior_predict.clmstan(object, newdata = NULL, ndraws = NULL, ...)
```

## Arguments

- object:

  A `clmstan` object returned by [`clm_stan()`](clm_stan.md).

- newdata:

  Optional data frame for prediction. If `NULL` (default), predictions
  are made for the original training data.

- ndraws:

  Number of posterior draws to use. If `NULL` (default), all available
  draws are used.

- ...:

  Additional arguments (currently ignored).

## Value

An integer matrix of dimension S x N containing predicted categories (1
to K), where S is the number of posterior draws and N is the number of
observations.

## See also

[`predict.clmstan()`](predict.clmstan.md),
[`fitted.clmstan()`](fitted.clmstan.md)
