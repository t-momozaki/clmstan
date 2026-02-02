# clmstan: Cumulative Link Models with CmdStanR

Fit cumulative link models (CLMs) for ordinal categorical data using
CmdStanR. The package supports various link functions including standard
links (logit, probit, cloglog, loglog, cauchit) and flexible parametric
links (GEV, AEP, Symmetric Power, Aranda-Ordaz, log-gamma).

Models are pre-compiled using the instantiate package for fast execution
without runtime compilation.

## Main functions

- [`clm_stan()`](https://t-momozaki.github.io/clmstan/reference/clm_stan.md) -
  Fit a cumulative link model

- [`supported_links()`](https://t-momozaki.github.io/clmstan/reference/supported_links.md) -
  List available link functions

## Methods

- [`print.clmstan()`](https://t-momozaki.github.io/clmstan/reference/print.clmstan.md) -
  Print summary

- [`summary.clmstan()`](https://t-momozaki.github.io/clmstan/reference/summary.clmstan.md) -
  Detailed summary

- [`coef.clmstan()`](https://t-momozaki.github.io/clmstan/reference/coef.clmstan.md) -
  Extract coefficients

- [`predict.clmstan()`](https://t-momozaki.github.io/clmstan/reference/predict.clmstan.md) -
  Predict categories or probabilities

- [`plot.clmstan()`](https://t-momozaki.github.io/clmstan/reference/plot.clmstan.md) -
  Diagnostic plots

- [`loo.clmstan()`](https://t-momozaki.github.io/clmstan/reference/loo.clmstan.md) -
  Leave-one-out cross-validation

## References

**Flexible Link Functions:**

Aranda-Ordaz, F. J. (1981). On two families of transformations to
additivity for binary response data. *Biometrika*, 68(2), 357-363.

Li, D., Wang, X., & Dey, D. K. (2019). Power link functions in an
ordinal regression model with Gaussian process priors. *Environmetrics*,
30(6), e2564.

Prentice, R. L. (1976). A generalization of the probit and logit methods
for dose response curves. *Biometrics*, 32(4), 761-768.

Wang, X. & Dey, D. K. (2011). Generalized extreme value regression for
ordinal response data. *Environmental and Ecological Statistics*, 18(4),
619-634.

Naranjo, L., Pérez, C. J., & Martín, J. (2015). Bayesian analysis of
some models that use the asymmetric exponential power distribution.
*Statistics and Computing*, 25(3), 497-514.

## See also

Useful links:

- [instantiate package](https://wlandau.github.io/instantiate/)

- [cmdstanr](https://mc-stan.org/cmdstanr/)

## Author

**Maintainer**: Tomotaka Momozaki <momozaki.stat@gmail.com>
