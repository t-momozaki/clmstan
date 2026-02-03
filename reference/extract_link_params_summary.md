# Extract posterior summary for link parameters

Extract posterior summary for link parameters

## Usage

``` r
extract_link_params_summary(object, probs = c(0.025, 0.5, 0.975))
```

## Arguments

- object:

  A clmstan object

- probs:

  Quantile probabilities

## Value

A data frame with posterior summaries, or NULL if no estimated params
