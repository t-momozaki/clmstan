# clmstan

Cumulative Link Models with CmdStanR

## Overview

`clmstan` fits cumulative link models (CLMs) for ordinal categorical
data using CmdStanR. It supports 11 link functions including standard
links (logit, probit, cloglog) and flexible parametric links (GEV, AEP,
Symmetric Power).

Models are pre-compiled using the `instantiate` package for fast
execution without runtime compilation.

## Installation

### Prerequisites

This package requires:

1.  **CmdStan** - Stan’s command-line interface
2.  **cmdstanr** - R interface to CmdStan (not on CRAN)

### Step 1: Install cmdstanr

``` r
# Install cmdstanr from r-universe (recommended)
install.packages("cmdstanr",
                 repos = c("https://stan-dev.r-universe.dev",
                           getOption("repos")))
```

### Step 2: Install CmdStan

``` r
library(cmdstanr)
install_cmdstan()  # Only needed once
```

### Step 3: Install clmstan

``` r
# From CRAN (when available)
install.packages("clmstan")

# From GitHub (development version)
# install.packages("devtools")
devtools::install_github("t-momozaki/clmstan")
```

**Note**: During package installation, Stan models are compiled
automatically. This may take a few minutes on first install.

## Quick Start

``` r
library(clmstan)

# Example data
set.seed(123)
n <- 100
x <- rnorm(n)
latent <- 1.0 * x + rlogis(n)
y <- cut(latent, breaks = c(-Inf, -1, 0, 1, Inf), labels = 1:4)
data <- data.frame(y = y, x = x)

# Fit a cumulative link model with logit link
fit <- clm_stan(y ~ x, data = data, link = "logit",
                chains = 4, iter = 2000, warmup = 1000)

# View results
fit$fit$summary(variables = c("beta", "c_transformed", "beta0"))
```

## Supported Link Functions

### Standard Links (5)

| Link    | Distribution | Use Case                                  |
|---------|--------------|-------------------------------------------|
| logit   | Logistic     | Default, proportional odds                |
| probit  | Normal       | Symmetric, latent variable interpretation |
| cloglog | Gumbel (max) | Asymmetric, proportional hazards          |
| loglog  | Gumbel (min) | Asymmetric                                |
| cauchit | Cauchy       | Heavy tails                               |

### Flexible Links with Parameters (6)

| Link         | Parameter      | Description                                |
|--------------|----------------|--------------------------------------------|
| tlink        | df \> 0        | t-distribution, adjustable tail weight     |
| aranda_ordaz | lambda \> 0    | Generalized asymmetric link                |
| sp           | r \> 0, base   | Symmetric Power, adjustable skewness       |
| log_gamma    | lambda         | Continuous symmetric/asymmetric adjustment |
| gev          | xi             | Generalized Extreme Value                  |
| aep          | theta1, theta2 | Asymmetric Exponential Power               |

### Using Flexible Links

``` r
# Fixed parameter
fit_t <- clm_stan(y ~ x, data = data, link = "tlink",
                  link_param = list(df = 8))

# Estimate parameter with Bayesian inference
fit_gev <- clm_stan(y ~ x, data = data, link = "gev",
                    link_param = list(xi = "estimate"))
```

## Threshold Structures

| Structure   | Description                      |
|-------------|----------------------------------|
| flexible    | Free thresholds (default)        |
| equidistant | Equal spacing between thresholds |
| symmetric   | Symmetric around center          |

``` r
# Equidistant thresholds
fit_equi <- clm_stan(y ~ x, data = data, threshold = "equidistant")
```

## Prior Specification

### Default Priors

clmstan uses weakly informative default priors:

| Parameter                   | Default Prior  |
|-----------------------------|----------------|
| Regression coefficients (β) | normal(0, 2.5) |
| Thresholds (c)              | normal(0, 10)  |
| Equidistant spacing (d)     | gamma(2, 0.5)  |

For link parameters estimated via Bayesian inference:

| Link         | Parameter      | Default Prior   |
|--------------|----------------|-----------------|
| tlink        | df             | gamma(2, 0.1)   |
| aranda_ordaz | lambda         | gamma(0.5, 0.5) |
| sp           | r              | gamma(0.5, 0.5) |
| log_gamma    | lambda         | normal(0, 1)    |
| gev          | xi             | normal(0, 2)    |
| aep          | theta1, theta2 | gamma(2, 1)     |

### Custom Priors

Use the [`prior()`](reference/prior.md) function with distribution
helpers:

``` r
# Tighter prior on regression coefficients
fit <- clm_stan(y ~ x, data = data,
                prior = prior(normal(0, 1), class = "b"))

# Multiple priors
fit <- clm_stan(y ~ x, data = data,
                prior = c(
                  prior(normal(0, 1), class = "b"),
                  prior(normal(0, 5), class = "Intercept")
                ))
```

### Prior for Link Parameters

When estimating link parameters, you can specify custom priors:

``` r
# Custom prior for t-link df parameter
fit <- clm_stan(y ~ x, data = data, link = "tlink",
                link_param = list(df = "estimate"),
                prior = prior(gamma(3, 0.2), class = "df"))

# Custom prior for GEV xi parameter
fit <- clm_stan(y ~ x, data = data, link = "gev",
                link_param = list(xi = "estimate"),
                prior = prior(normal(0, 0.5), class = "xi"))
```

### Available Distribution Functions

| Function                      | Parameters          | Example                       |
|-------------------------------|---------------------|-------------------------------|
| `normal(mu, sigma)`           | mean, SD            | `normal(0, 2.5)`              |
| `gamma(alpha, beta)`          | shape, rate         | `gamma(2, 0.1)`               |
| `student_t(df, mu, sigma)`    | df, location, scale | `student_t(3, 0, 2.5)`        |
| `cauchy(mu, sigma)`           | location, scale     | `cauchy(0, 2.5)`              |
| [`flat()`](reference/flat.md) | none                | [`flat()`](reference/flat.md) |

**Note:** [`flat()`](reference/flat.md) creates an improper uniform
prior. Use with caution as it may lead to improper posteriors if the
data does not provide sufficient information. For thresholds with
ordered constraints, Stan’s internal transformation provides implicit
regularization.

### Prior Classes

| Class              | Description                     | Compatible Distributions        |
|--------------------|---------------------------------|---------------------------------|
| `b`                | Regression coefficients         | normal, student_t, cauchy, flat |
| `Intercept`        | Thresholds (flexible)           | normal, student_t, cauchy, flat |
| `c1`               | First threshold (equidistant)   | normal, student_t, cauchy, flat |
| `cpos`             | Positive thresholds (symmetric) | normal, student_t, cauchy, flat |
| `d`                | Equidistant spacing             | gamma                           |
| `df`               | t-link degrees of freedom       | gamma                           |
| `lambda_ao`        | Aranda-Ordaz lambda             | gamma                           |
| `r`                | Symmetric Power r               | gamma                           |
| `lambda_lg`        | Log-gamma lambda                | normal, student_t, cauchy       |
| `xi`               | GEV xi                          | normal, student_t, cauchy       |
| `theta1`, `theta2` | AEP shape parameters            | gamma                           |

## License

MIT
