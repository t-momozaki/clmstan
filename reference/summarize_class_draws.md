# Summarize class prediction draws

Summarize class prediction draws

## Usage

``` r
summarize_class_draws(class_draws, robust = FALSE, probs = c(0.025, 0.975))
```

## Arguments

- class_draws:

  S x N matrix of predicted classes

- robust:

  Use median instead of mean

- probs:

  Quantile probabilities

## Value

Data frame with summary statistics
