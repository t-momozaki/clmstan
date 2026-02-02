# Extract posterior draws needed for prediction

Extract posterior draws needed for prediction

## Usage

``` r
extract_prediction_draws(object, ndraws = NULL)
```

## Arguments

- object:

  A clmstan object

- ndraws:

  Number of draws to extract

## Value

List with c (cutpoints), beta (coefficients), link_params (if full
model)
