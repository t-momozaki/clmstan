# Get draws for plotting with bayesplot

Returns draws for default parameters (excluding log_lik, y_rep, eta, raw
c).

## Usage

``` r
get_clmstan_draws(object, pars = NULL)
```

## Arguments

- object:

  A clmstan object

- pars:

  Character vector of parameter names (NULL for default)

## Value

A draws_array suitable for bayesplot
