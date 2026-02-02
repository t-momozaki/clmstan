# Extract ACF values from clmstan object

Computes autocorrelation function (ACF) values for MCMC chains and
returns them in a tidy data frame format.

## Usage

``` r
extract_acf(object, pars = NULL, lags = 20, ...)
```

## Arguments

- object:

  A clmstan object

- pars:

  Character vector of parameter names. If NULL (default), uses beta,
  c_transformed (except first), and beta0.

- lags:

  Maximum number of lags to compute. Default is 20.

- ...:

  Additional arguments (ignored)

## Value

A data frame with columns:

- parameter: Parameter name

- chain: Chain number

- lag: Lag value (0, 1, 2, ...)

- acf: Autocorrelation value

## Details

The ACF measures how correlated each draw is with previous draws in the
same chain. High autocorrelation at many lags indicates slow mixing and
the need for more samples or reparameterization.

Ideally, ACF should drop to near zero within a few lags. Persistent high
autocorrelation suggests the sampler is exploring the posterior slowly.

## Examples

``` r
if (FALSE) { # \dontrun{
fit <- clm_stan(rating ~ temp, data = wine)
acf_df <- extract_acf(fit)
head(acf_df)

# Plot ACF for specific parameters
library(ggplot2)
acf_df |>
  dplyr::filter(parameter == "beta[1]") |>
  ggplot(aes(x = lag, y = acf, color = factor(chain))) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed")
} # }
```
