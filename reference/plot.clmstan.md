# Plot method for clmstan objects

Produces diagnostic plots using the bayesplot package.

## Usage

``` r
# S3 method for class 'clmstan'
plot(
  x,
  type = c("trace", "dens", "hist", "areas", "intervals", "acf"),
  pars = NULL,
  ...
)
```

## Arguments

- x:

  A clmstan object

- type:

  Type of plot: "trace" (default), "dens", "hist", "areas", "intervals",
  or "acf" (autocorrelation).

- pars:

  Character vector of parameter names to plot. If NULL, plots beta,
  c_transformed (except first), and beta0.

- ...:

  Additional arguments passed to bayesplot functions. For "acf" type,
  you can use `lags` to control the number of lags.

## Value

A ggplot object

## Examples

``` r
if (FALSE) { # \dontrun{
fit <- clm_stan(rating ~ temp, data = wine)
plot(fit)                    # trace plots
plot(fit, type = "dens")     # density plots
plot(fit, type = "intervals") # credible intervals
plot(fit, type = "acf")      # autocorrelation plots
plot(fit, pars = "beta")     # only beta parameters
} # }
```
