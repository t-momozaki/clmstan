# Extract posterior summary for specified parameters

Extract posterior summary for specified parameters

## Usage

``` r
extract_posterior_summary(fit, pars, probs = c(0.025, 0.5, 0.975))
```

## Arguments

- fit:

  A CmdStanMCMC object

- pars:

  Character vector of parameter names

- probs:

  Quantile probabilities

## Value

A data frame with posterior summaries
