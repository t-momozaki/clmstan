# Widely Applicable Information Criterion for clmstan objects

Computes the Widely Applicable Information Criterion (WAIC) for a fitted
cumulative link model.

## Usage

``` r
# S3 method for class 'clmstan'
waic(x, ...)
```

## Arguments

- x:

  A `clmstan` object returned by
  [`clm_stan`](https://t-momozaki.github.io/clmstan/reference/clm_stan.md).

- ...:

  Additional arguments (currently ignored).

## Value

An object of class `c("waic", "loo")` containing:

- `estimates`: A matrix with columns `Estimate` and `SE` for
  `elpd_waic`, `p_waic`, and `waic`.

- `pointwise`: A matrix with pointwise contributions.

## Details

WAIC is an alternative to LOO-CV that is asymptotically equivalent to
leave-one-out cross-validation. However, LOO-CV with PSIS is generally
preferred because:

- It provides useful diagnostics (Pareto k values)

- It is more robust in finite samples

- It has been shown to be more reliable in practice

For most purposes,
[`loo.clmstan`](https://t-momozaki.github.io/clmstan/reference/loo.clmstan.md)
is recommended over WAIC.

## See also

[`loo.clmstan`](https://t-momozaki.github.io/clmstan/reference/loo.clmstan.md)
for LOO-CV (recommended),
[`waic`](https://mc-stan.org/loo/reference/waic.html) for details on
WAIC computation,
[`loo_compare`](https://mc-stan.org/loo/reference/loo_compare.html) for
model comparison.

## Examples

``` r
if (FALSE) { # \dontrun{
fit <- clm_stan(rating ~ temp, data = wine)
waic_result <- waic(fit)
print(waic_result)
} # }
```
