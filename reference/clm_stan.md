# Fit a Cumulative Link Model using CmdStanR

Fit a Cumulative Link Model using CmdStanR

## Usage

``` r
clm_stan(
  formula,
  data,
  link = "logit",
  base = "logit",
  threshold = "flexible",
  link_param = NULL,
  prior = NULL,
  chains = 4,
  iter = 2000,
  warmup = 1000,
  ...
)
```

## Arguments

- formula:

  A formula specifying the model (response ~ predictors)

- data:

  A data frame containing the variables in the formula

- link:

  Link function. One of "logit" (default), "probit", "cloglog",
  "loglog", "cauchit", "tlink", "gev", "aep", "sp", "aranda_ordaz",
  "log_gamma"

- base:

  Base distribution for SP link. One of "logit" (default), "probit",
  "cloglog", "loglog", "cauchit", "tlink". Ignored for other link
  functions.

- threshold:

  Threshold structure. One of "flexible" (default), "equidistant",
  "symmetric"

- link_param:

  A list of link parameters. For flexible links, values can be:

  - Numeric: Use as fixed value (e.g., `list(df = 8)`)

  - "estimate": Estimate the parameter with Bayesian inference

- prior:

  Prior specification. Can be either:

  - A `clm_prior` object created by
    [`clm_prior()`](https://t-momozaki.github.io/clmstan/reference/clm_prior.md)

  - A distribution-based prior using
    [`prior()`](https://t-momozaki.github.io/clmstan/reference/prior.md)
    with distribution functions
    ([`normal()`](https://t-momozaki.github.io/clmstan/reference/normal.md),
    [`gamma()`](https://t-momozaki.github.io/clmstan/reference/gamma.md),
    [`student_t()`](https://t-momozaki.github.io/clmstan/reference/student_t.md),
    [`cauchy()`](https://t-momozaki.github.io/clmstan/reference/cauchy.md))

- chains:

  Number of MCMC chains (default: 4)

- iter:

  Total iterations per chain (default: 2000)

- warmup:

  Warmup iterations per chain (default: 1000)

- ...:

  Additional arguments passed to cmdstanr::sample()

## Value

An object of class "clmstan"

## Examples

``` r
if (FALSE) { # \dontrun{
# Fit a proportional odds model
library(ordinal)
data(wine)
fit <- clm_stan(rating ~ temp + contact, data = wine, link = "logit")
print(fit)

# Fit with t-link (fixed df)
fit_t <- clm_stan(rating ~ temp, data = wine, link = "tlink",
                  link_param = list(df = 8))

# Fit with GEV link (estimate xi)
fit_gev <- clm_stan(rating ~ temp, data = wine, link = "gev",
                    link_param = list(xi = "estimate"))
} # }
```
