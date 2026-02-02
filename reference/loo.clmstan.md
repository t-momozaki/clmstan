# Leave-One-Out Cross-Validation for clmstan objects

Computes approximate leave-one-out cross-validation (LOO-CV) for a
fitted cumulative link model using Pareto smoothed importance sampling
(PSIS).

## Usage

``` r
# S3 method for class 'clmstan'
loo(x, ..., r_eff = NULL, cores = getOption("mc.cores", 1), save_psis = FALSE)
```

## Arguments

- x:

  A `clmstan` object returned by
  [`clm_stan`](https://t-momozaki.github.io/clmstan/reference/clm_stan.md).

- ...:

  Additional arguments passed to
  [`loo`](https://mc-stan.org/loo/reference/loo.html).

- r_eff:

  A vector of relative effective sample sizes for each observation, or
  `NULL` (default) to compute them automatically using
  [`relative_eff`](https://mc-stan.org/loo/reference/relative_eff.html).
  Set to `NA` to skip r_eff computation (faster but diagnostics may be
  over-optimistic).

- cores:

  The number of cores to use for parallel computation. Defaults to
  `getOption("mc.cores", 1)`.

- save_psis:

  If `TRUE`, the PSIS object is saved in the returned object. This is
  required for some downstream functions like `E_loo()`. Default is
  `FALSE`.

## Value

An object of class `c("psis_loo", "loo")` containing:

- `estimates`: A matrix with columns `Estimate` and `SE` for `elpd_loo`,
  `p_loo`, and `looic`.

- `pointwise`: A matrix with pointwise contributions.

- `diagnostics`: A list with Pareto k values and effective sample sizes
  for each observation.

## Details

The function extracts the log-likelihood matrix (`log_lik`) computed in
the generated quantities block of the Stan model and passes it to
[`loo`](https://mc-stan.org/loo/reference/loo.html).

**Pareto k diagnostics:** Observations with high Pareto k values (k \>
0.7) indicate potential problems with the LOO approximation for those
observations. Use
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) on the returned
object to visualize the Pareto k values.

**Model comparison:** Use
[`loo_compare`](https://mc-stan.org/loo/reference/loo_compare.html) to
compare multiple models. Models with higher `elpd_loo` are preferred.

## See also

[`waic.clmstan`](https://t-momozaki.github.io/clmstan/reference/waic.clmstan.md)
for WAIC computation,
[`loo`](https://mc-stan.org/loo/reference/loo.html) for details on the
LOO algorithm,
[`loo_compare`](https://mc-stan.org/loo/reference/loo_compare.html) for
model comparison.

## Examples

``` r
if (FALSE) { # \dontrun{
fit <- clm_stan(rating ~ temp, data = wine)
loo_result <- loo(fit)
print(loo_result)
plot(loo_result)

# Compare two models
fit1 <- clm_stan(rating ~ temp, data = wine, link = "logit")
fit2 <- clm_stan(rating ~ temp, data = wine, link = "probit")
loo::loo_compare(loo(fit1), loo(fit2))
} # }
```
