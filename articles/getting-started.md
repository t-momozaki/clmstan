# Getting Started with clmstan

## Introduction

**clmstan** is an R package for fitting Cumulative Link Models (CLMs)
using Bayesian inference via CmdStan. It provides: - **11 link
functions**: 5 standard + 6 flexible parametric links - **3 threshold
structures**: flexible, equidistant, symmetric - **Bayesian
estimation**: Full posterior inference with MCMC - **Pre-compiled Stan
models**: Fast execution via the instantiate package

### Comparison with ordinal::clm()

| Feature                       | ordinal::clm()     | clmstan                      |
|-------------------------------|--------------------|------------------------------|
| Inference                     | Maximum likelihood | Bayesian (MCMC)              |
| Uncertainty                   | Asymptotic SE      | Posterior distribution       |
| Link functions                | 5 standard         | 11 (5 standard + 6 flexible) |
| Estimation of link parameters | No                 | Yes                          |

## Installation

Before using clmstan, you need to install CmdStan:

``` r
# Step 1: Install cmdstanr
install.packages("cmdstanr",
                 repos = c("https://stan-dev.r-universe.dev",
                           getOption("repos")))

# Step 2: Install CmdStan (only needed once)
library(cmdstanr)
install_cmdstan()

# Step 3: Install clmstan
# devtools::install_github("t-momozaki/clmstan")
```

## Quick Start

Fit a cumulative link model in just a few lines:

``` r
library(clmstan)
library(ordinal)
data(wine)

# Fit a model with logit link
fit <- clm_stan(rating ~ temp + contact, data = wine,
                chains = 4, iter = 2000, warmup = 1000)

# View results
print(fit)
summary(fit)
```

The `wine` dataset contains ratings (1-5) of 72 wine samples with two
predictors: temperature (cold/warm) and contact (yes/no).

## For ordinal::clm() Users

If you’re familiar with the ordinal package, the transition to clmstan
is straightforward:

### API Correspondence

| ordinal::clm()           | clmstan::clm_stan()      |
|--------------------------|--------------------------|
| `clm(y ~ x, data)`       | `clm_stan(y ~ x, data)`  |
| `link = "logit"`         | `link = "logit"`         |
| `threshold = "flexible"` | `threshold = "flexible"` |
| `summary(fit)`           | `summary(fit)`           |
| `coef(fit)`              | `coef(fit)`              |

### Example Comparison

``` r
library(ordinal)
library(clmstan)
data(wine)

# ordinal::clm (frequentist)
fit_freq <- clm(rating ~ temp + contact, data = wine, link = "logit")

# clmstan::clm_stan (Bayesian)
fit_bayes <- clm_stan(rating ~ temp + contact, data = wine, link = "logit",
                      chains = 4, iter = 2000, warmup = 1000)

# Compare coefficients
coef(fit_freq)
coef(fit_bayes)
```

### Key Differences

1.  **Posterior distributions**: clmstan provides full posterior
    samples, enabling Bayesian inference (credible intervals, posterior
    predictive checks)
2.  **Reproducibility**: Set
    [`set.seed()`](https://rdrr.io/r/base/Random.html) before
    [`clm_stan()`](../reference/clm_stan.md) for reproducible results
3.  **Computation time**: MCMC sampling is slower than MLE, but provides
    richer inference

## Link Functions

clmstan supports 11 link functions, more than any other CLM package.

### Standard Links (5)

``` r
# View available link functions
supported_links("standard")

# Logit (default) - proportional odds model
fit_logit <- clm_stan(rating ~ temp, data = wine, link = "logit",
                      chains = 2, iter = 1000, warmup = 500)

# Probit - normal distribution
fit_probit <- clm_stan(rating ~ temp, data = wine, link = "probit",
                       chains = 2, iter = 1000, warmup = 500)

# Complementary log-log - asymmetric (right-skewed)
fit_cloglog <- clm_stan(rating ~ temp, data = wine, link = "cloglog",
                        chains = 2, iter = 1000, warmup = 500)

# Log-log - asymmetric (left-skewed)
fit_loglog <- clm_stan(rating ~ temp, data = wine, link = "loglog",
                       chains = 2, iter = 1000, warmup = 500)

# Cauchit - heavy tails
fit_cauchit <- clm_stan(rating ~ temp, data = wine, link = "cauchit",
                        chains = 2, iter = 1000, warmup = 500)
```

### Flexible Links (6)

Flexible links have parameters that can be fixed or estimated. For
theoretical background, see Wang & Dey (2011) for GEV, Jiang & Dey
(2015) for SP, and Naranjo et al. (2015) for AEP.

``` r
# View flexible link functions
supported_links("flexible")

# t-link with fixed df (heavier tails than logit)
fit_t8 <- clm_stan(rating ~ temp, data = wine, link = "tlink",
                   link_param = list(df = 8),
                   chains = 2, iter = 1000, warmup = 500)

# t-link with estimated df (data-driven tail behavior)
fit_t_est <- clm_stan(rating ~ temp, data = wine, link = "tlink",
                      link_param = list(df = "estimate"),
                      chains = 2, iter = 1000, warmup = 500)

# GEV link with estimated shape parameter
fit_gev <- clm_stan(rating ~ temp, data = wine, link = "gev",
                    link_param = list(xi = "estimate"),
                    chains = 2, iter = 1000, warmup = 500)

# Aranda-Ordaz link (asymmetric)
fit_ao <- clm_stan(rating ~ temp, data = wine, link = "aranda_ordaz",
                   link_param = list(lambda = "estimate"),
                   chains = 2, iter = 1000, warmup = 500)

# Symmetric Power link (skewness adjustment)
fit_sp <- clm_stan(rating ~ temp, data = wine, link = "sp",
                   base = "logit",
                   link_param = list(r = "estimate"),
                   chains = 2, iter = 1000, warmup = 500)

# Log-gamma link
fit_lg <- clm_stan(rating ~ temp, data = wine, link = "log_gamma",
                   link_param = list(lambda = "estimate"),
                   chains = 2, iter = 1000, warmup = 500)

# AEP link (asymmetric exponential power)
fit_aep <- clm_stan(rating ~ temp, data = wine, link = "aep",
                    link_param = list(theta1 = "estimate", theta2 = "estimate"),
                    chains = 2, iter = 1000, warmup = 500)
```

### Link Function Summary

| Link         | Parameters          | Special Cases                  |
|--------------|---------------------|--------------------------------|
| tlink        | df \> 0             | df = Inf -\> probit            |
| aranda_ordaz | lambda \> 0         | lambda = 1 -\> logit           |
| gev          | xi (real)           | xi = 0 -\> loglog              |
| sp           | r \> 0, base        | r = 1 -\> base distribution    |
| log_gamma    | lambda (real)       | lambda = 0 -\> probit          |
| aep          | theta1, theta2 \> 0 | Both = 2 -\> similar to probit |

## Threshold Structures

clmstan supports three threshold parameterizations:

``` r
# View available threshold structures
supported_thresholds()
```

### Flexible (Default)

Each threshold is freely estimated (K-1 parameters for K categories):

``` r
fit_flex <- clm_stan(rating ~ temp, data = wine, threshold = "flexible",
                     chains = 2, iter = 1000, warmup = 500)
```

### Equidistant

Thresholds are equally spaced (2 parameters: start + interval):

``` r
# Useful for Likert scales with assumed equal intervals
fit_equi <- clm_stan(rating ~ temp, data = wine, threshold = "equidistant",
                     chains = 2, iter = 1000, warmup = 500)
```

### Symmetric

Thresholds are symmetric around zero (useful for scales with a neutral
center):

``` r
fit_sym <- clm_stan(rating ~ temp, data = wine, threshold = "symmetric",
                    chains = 2, iter = 1000, warmup = 500)
```

## Prior Specification

clmstan uses weakly informative default priors, but you can customize
them.

### Default Priors

| Parameter           | Default Prior   |
|---------------------|-----------------|
| Coefficients (beta) | normal(0, 2.5)  |
| Thresholds (c)      | normal(0, 10)   |
| t-link df           | gamma(2, 0.1)   |
| GEV xi              | normal(0, 2)    |
| SP r                | gamma(0.5, 0.5) |
| AEP theta1, theta2  | gamma(2, 1)     |

### Using prior() Function (Recommended)

The [`prior()`](../reference/prior.md) function provides a brms-like
interface:

``` r
# Single prior
fit <- clm_stan(rating ~ temp, data = wine,
                prior = prior(normal(0, 1), class = "b"),
                chains = 2, iter = 1000, warmup = 500)

# Multiple priors
fit <- clm_stan(rating ~ temp, data = wine,
                prior = c(
                  prior(normal(0, 1), class = "b"),
                  prior(normal(0, 5), class = "Intercept")
                ),
                chains = 2, iter = 1000, warmup = 500)

# Prior for link parameters
fit_gev <- clm_stan(rating ~ temp, data = wine, link = "gev",
                    link_param = list(xi = "estimate"),
                    prior = prior(normal(0, 0.5), class = "xi"),
                    chains = 2, iter = 1000, warmup = 500)
```

### Available Distributions

``` r
normal(mu, sigma)      # Normal distribution
gamma(alpha, beta)     # Gamma distribution (shape, rate)
student_t(df, mu, sigma)  # Student-t distribution
cauchy(mu, sigma)      # Cauchy distribution
flat()                 # Flat (improper) prior
```

### Prior Classes

| Class                  | Description                     | Compatible Distributions        |
|------------------------|---------------------------------|---------------------------------|
| `"b"`                  | Regression coefficients         | normal, student_t, cauchy, flat |
| `"Intercept"`          | Thresholds (flexible)           | normal, student_t, cauchy, flat |
| `"c1"`                 | First threshold (equidistant)   | normal, student_t, cauchy, flat |
| `"d"`                  | Interval (equidistant)          | gamma                           |
| `"cpos"`               | Positive thresholds (symmetric) | normal, student_t, cauchy, flat |
| `"df"`                 | t-link degrees of freedom       | gamma                           |
| `"xi"`                 | GEV shape parameter             | normal, student_t, cauchy       |
| `"r"`                  | SP skewness parameter           | gamma                           |
| `"theta1"`, `"theta2"` | AEP shape parameters            | gamma                           |

### Legacy API (clm_prior)

For backward compatibility:

``` r
fit <- clm_stan(rating ~ temp, data = wine,
                prior = clm_prior(beta_sd = 1, c_sd = 5),
                chains = 2, iter = 1000, warmup = 500)
```

## Interpreting Results

### Basic Methods

``` r
fit <- clm_stan(rating ~ temp + contact, data = wine,
                chains = 4, iter = 2000, warmup = 1000)

# Quick overview
print(fit)

# Detailed summary with credible intervals
summary(fit)
summary(fit, probs = c(0.025, 0.5, 0.975))

# Extract point estimates
coef(fit)                    # Posterior mean (default)
coef(fit, type = "median")   # Posterior median
```

### Diagnostic Plots

``` r
# Trace plots (check mixing)
plot(fit, type = "trace")

# Posterior density
plot(fit, type = "dens")

# Posterior intervals
plot(fit, type = "intervals")

# Autocorrelation (check for high autocorrelation)
plot(fit, type = "acf")

# Select specific parameters
plot(fit, type = "trace", pars = c("beta[1]", "beta[2]"))
```

### Convergence Diagnostics

``` r
# Quick summary
diagnostics(fit)

# Detailed output
diagnostics(fit, detail = TRUE)
```

Key diagnostics to check: - **Rhat**: Should be \< 1.01 (values \> 1.01
indicate poor convergence) - **ESS (bulk/tail)**: Should be \> 400 (low
values suggest inefficient sampling) - **Divergences**: Should be 0
(divergences indicate sampling problems)

## Prediction

### Category Prediction

``` r
fit <- clm_stan(rating ~ temp + contact, data = wine,
                chains = 4, iter = 2000, warmup = 1000)

# Predict most likely category
pred_class <- predict(fit, type = "class")
head(pred_class)

# Prediction for new data
newdata <- data.frame(
  temp = factor("warm", levels = levels(wine$temp)),
  contact = factor("yes", levels = levels(wine$contact))
)
predict(fit, newdata = newdata, type = "class")
```

### Probability Prediction

``` r
# Predicted probabilities for each category
pred_probs <- predict(fit, type = "probs")
head(pred_probs)

# Alternative: fitted values
fitted_vals <- fitted(fit)
head(fitted_vals)
```

### Posterior Predictive Distribution

``` r
# Draw from posterior predictive distribution
y_rep <- posterior_predict(fit)
dim(y_rep)  # draws x observations

# Uncertainty in predictions
pred_draws <- predict(fit, type = "class", summary = FALSE)
```

## Model Comparison

Use LOO-CV (Leave-One-Out Cross-Validation) for model comparison:

``` r
# Fit competing models
fit1 <- clm_stan(rating ~ temp, data = wine, link = "logit",
                 chains = 4, iter = 2000, warmup = 1000)
fit2 <- clm_stan(rating ~ temp + contact, data = wine, link = "logit",
                 chains = 4, iter = 2000, warmup = 1000)
fit3 <- clm_stan(rating ~ temp + contact, data = wine, link = "probit",
                 chains = 4, iter = 2000, warmup = 1000)

# Compute LOO-CV
loo1 <- loo(fit1)
loo2 <- loo(fit2)
loo3 <- loo(fit3)

# View results
print(loo1)

# Pareto k diagnostic plot
plot(loo1)

# Compare models
loo::loo_compare(loo1, loo2, loo3)

# WAIC is also available
waic(fit1)
```

**Interpreting LOO results:** - Lower `elpd_loo` (expected log pointwise
predictive density) indicates better fit - Pareto k values \> 0.7
suggest unreliable LOO estimates for those observations

## Practical Workflow

Here’s a recommended workflow for ordinal data analysis:

``` r
library(clmstan)
library(ordinal)
data(wine)

# Step 1: Fit baseline model
set.seed(123)
fit_base <- clm_stan(rating ~ temp + contact, data = wine,
                     link = "logit", threshold = "flexible",
                     chains = 4, iter = 2000, warmup = 1000)

# Step 2: Check convergence
diagnostics(fit_base)
plot(fit_base, type = "trace")

# Step 3: Review results
summary(fit_base)

# Step 4: Try alternative link functions
fit_probit <- clm_stan(rating ~ temp + contact, data = wine,
                       link = "probit",
                       chains = 4, iter = 2000, warmup = 1000)

fit_gev <- clm_stan(rating ~ temp + contact, data = wine,
                    link = "gev", link_param = list(xi = "estimate"),
                    chains = 4, iter = 2000, warmup = 1000)

# Step 5: Compare models
loo_base <- loo(fit_base)
loo_probit <- loo(fit_probit)
loo_gev <- loo(fit_gev)
loo::loo_compare(loo_base, loo_probit, loo_gev)

# Step 6: Final model interpretation
summary(fit_base)
plot(fit_base, type = "intervals")
```

## Troubleshooting

### CmdStan Not Found

``` r
# Check CmdStan installation
cmdstanr::cmdstan_path()
cmdstanr::cmdstan_version()

# If not found, install CmdStan:
cmdstanr::install_cmdstan()
```

### Convergence Issues

If you see Rhat \> 1.01 or low ESS:

``` r
# Increase iterations and warmup
fit <- clm_stan(rating ~ temp, data = wine,
                chains = 4,
                iter = 4000,
                warmup = 2000)

# Increase adapt_delta (helps with divergences)
fit <- clm_stan(rating ~ temp, data = wine,
                chains = 4,
                iter = 2000,
                warmup = 1000,
                adapt_delta = 0.95)

# Increase max_treedepth
fit <- clm_stan(rating ~ temp, data = wine,
                chains = 4,
                iter = 2000,
                warmup = 1000,
                max_treedepth = 12)
```

### Divergent Transitions

Divergent transitions indicate that the sampler had difficulty exploring
the posterior:

1.  **Increase `adapt_delta`**: Try 0.95, 0.99, or even 0.999
2.  **Reparameterize**: Try different threshold structures
3.  **Simplify priors**: Use more informative priors
4.  **Check data**: Look for separation or outliers

``` r
# High adapt_delta
fit <- clm_stan(rating ~ temp, data = wine,
                chains = 4,
                iter = 2000,
                warmup = 1000,
                adapt_delta = 0.99)
```

### Memory Issues

For large datasets:

``` r
# Reduce number of chains
fit <- clm_stan(rating ~ temp, data = wine,
                chains = 2,
                iter = 2000,
                warmup = 1000)

# Run chains in parallel (default)
fit <- clm_stan(rating ~ temp, data = wine,
                chains = 4,
                parallel_chains = 4)
```

## References and Resources

### Package Resources

- [clmstan GitHub Repository](https://github.com/t-momozaki/clmstan)
- [Stan User’s Guide](https://mc-stan.org/users/documentation/)
- [cmdstanr Documentation](https://mc-stan.org/cmdstanr/)

### Related Packages

- [ordinal](https://cran.r-project.org/package=ordinal): Frequentist
  CLMs
- [brms](https://paul-buerkner.github.io/brms/): Bayesian regression
  (rstan-based)

### Statistical References

- Agresti, A. (2010). *Analysis of Ordinal Categorical Data* (2nd ed.)
- Christensen, R.H.B. (2019). *ordinal: Regression Models for Ordinal
  Data*
- Jiang, X., & Dey, D.K. (2015). Symmetric power link with ordinal
  response model. In S.K. Upadhyay et al. (Eds.), *Current trends in
  Bayesian methodology with applications* (pp. 335-346). CRC Press.
- Naranjo, L., Perez, C.J., & Martin, J. (2015). Bayesian analysis of
  some models that use the asymmetric exponential power distribution.
  *Statistics and Computing*, 25(3), 497-514.
- Wang, X., & Dey, D.K. (2011). Generalized extreme value regression for
  ordinal response data. *Environmental and Ecological Statistics*,
  18(4), 619-634.
