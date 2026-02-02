# Extract coefficients from clmstan objects

Returns posterior point estimates (mean or median) for all model
parameters.

## Usage

``` r
# S3 method for class 'clmstan'
coef(object, type = c("mean", "median"), ...)
```

## Arguments

- object:

  A clmstan object

- type:

  Type of point estimate: "mean" (default) or "median"

- ...:

  Additional arguments (ignored)

## Value

A named numeric vector with:

- Threshold coefficients (e.g., "1\|2", "2\|3", ...)

- Regression coefficients (variable names from formula)

## Examples

``` r
if (FALSE) { # \dontrun{
fit <- clm_stan(rating ~ temp, data = wine)
coef(fit)
coef(fit, type = "median")
} # }
```
