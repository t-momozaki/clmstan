# Comparison tests with ordinal::clm()
#
# NOTE: These tests require the package to be installed (not just loaded).
#
# IMPORTANT: clmstan uses c1 = 0 identification constraint, while ordinal::clm
# estimates all thresholds freely. Therefore, we:
# - Compare beta coefficients directly (should match)
# - Transform ordinal::clm thresholds to c1=0 parameterization before comparison

# Helper to check if Stan models are compiled (package must be installed)
stan_models_compiled <- function() {
  model_path <- system.file("bin", "stan", "clm_base", package = "clmstan")
  nzchar(model_path) && file.exists(model_path)
}

# Helper to transform ordinal::clm thresholds to c1=0 parameterization
# ordinal::clm estimates all thresholds freely, while clmstan uses c1=0 constraint
# This function subtracts the first threshold from all thresholds
transform_thresholds_c1_zero <- function(coefs, K) {
  thresh_names <- paste0(1:(K - 1), "|", 2:K)
  thresh_vals <- unname(coefs[thresh_names])
  transformed <- thresh_vals - thresh_vals[1]  # Subtract first threshold
  names(transformed) <- thresh_names
  transformed
}

test_that("clmstan matches ordinal::clm for logit link", {
  skip_if_not(instantiate::stan_cmdstan_exists(), "CmdStan not available")
  skip_if_not(stan_models_compiled(), "Stan models not compiled (install package first)")
  skip_if_not_installed("ordinal")

  set.seed(42)
  n <- 300
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  eta <- 1.0 * x1 - 0.5 * x2
  latent <- eta + rlogis(n)
  y <- cut(latent, breaks = c(-Inf, -1, 0.5, 2, Inf), labels = 1:4)
  data <- data.frame(y = y, x1 = x1, x2 = x2)

  fit_clm <- ordinal::clm(y ~ x1 + x2, data = data, link = "logit")
  fit_clmstan <- clm_stan(
    y ~ x1 + x2,
    data = data,
    link = "logit",
    chains = 4,
    iter = 2000,
    warmup = 1000,
    refresh = 0
  )

  coef_clm <- coef(fit_clm)
  coef_clmstan <- coef(fit_clmstan, type = "mean")

  # Transform ordinal::clm thresholds to c1=0 parameterization
  thresh_clm_transformed <- transform_thresholds_c1_zero(coef_clm, K = 4)

  # Compare thresholds directly (both now use c1=0)
  thresh_names <- c("1|2", "2|3", "3|4")
  for (tname in thresh_names) {
    expect_equal(
      unname(coef_clmstan[tname]),
      unname(thresh_clm_transformed[tname]),
      tolerance = 0.20,
      label = paste("Threshold", tname)
    )
  }

  # Compare coefficients (should match directly)
  expect_equal(
    unname(coef_clmstan["x1"]),
    unname(coef_clm["x1"]),
    tolerance = 0.15,
    label = "Beta x1"
  )
  expect_equal(
    unname(coef_clmstan["x2"]),
    unname(coef_clm["x2"]),
    tolerance = 0.15,
    label = "Beta x2"
  )
})

test_that("clmstan matches ordinal::clm for probit link", {
  skip_if_not(instantiate::stan_cmdstan_exists(), "CmdStan not available")
  skip_if_not(stan_models_compiled(), "Stan models not compiled (install package first)")
  skip_if_not_installed("ordinal")

  set.seed(123)
  n <- 250
  x <- rnorm(n)
  latent <- 0.8 * x + rnorm(n)
  y <- cut(latent, breaks = c(-Inf, -0.5, 0.5, Inf), labels = 1:3)
  data <- data.frame(y = y, x = x)

  fit_clm <- ordinal::clm(y ~ x, data = data, link = "probit")
  fit_clmstan <- clm_stan(
    y ~ x,
    data = data,
    link = "probit",
    chains = 4,
    iter = 2000,
    warmup = 1000,
    refresh = 0
  )

  coef_clm <- coef(fit_clm)
  coef_clmstan <- coef(fit_clmstan, type = "mean")

  # Compare beta coefficient (should match directly)
  expect_equal(
    unname(coef_clmstan["x"]),
    unname(coef_clm["x"]),
    tolerance = 0.15
  )

  # Transform ordinal::clm thresholds to c1=0 parameterization
  thresh_clm_transformed <- transform_thresholds_c1_zero(coef_clm, K = 3)

  # Compare thresholds directly
  for (tname in c("1|2", "2|3")) {
    expect_equal(
      unname(coef_clmstan[tname]),
      unname(thresh_clm_transformed[tname]),
      tolerance = 0.15,
      label = paste("Threshold", tname)
    )
  }
})

test_that("clmstan matches ordinal::clm for cloglog link", {
  skip_if_not(instantiate::stan_cmdstan_exists(), "CmdStan not available")
  skip_if_not(stan_models_compiled(), "Stan models not compiled (install package first)")
  skip_if_not_installed("ordinal")

  set.seed(234)
  n <- 250
  x <- rnorm(n)
  # Gumbel (maximum) errors for cloglog
  latent <- 0.6 * x + (-log(-log(runif(n))))
  y <- cut(latent, breaks = c(-Inf, 0, 1, Inf), labels = 1:3)
  data <- data.frame(y = y, x = x)

  fit_clm <- ordinal::clm(y ~ x, data = data, link = "cloglog")
  fit_clmstan <- clm_stan(
    y ~ x,
    data = data,
    link = "cloglog",
    chains = 4,
    iter = 2000,
    warmup = 1000,
    refresh = 0
  )

  coef_clm <- coef(fit_clm)
  coef_clmstan <- coef(fit_clmstan, type = "mean")

  expect_equal(
    unname(coef_clmstan["x"]),
    unname(coef_clm["x"]),
    tolerance = 0.20
  )
})

test_that("clmstan matches ordinal::clm for loglog link", {
  skip_if_not(instantiate::stan_cmdstan_exists(), "CmdStan not available")
  skip_if_not(stan_models_compiled(), "Stan models not compiled (install package first)")
  skip_if_not_installed("ordinal")

  set.seed(345)
  n <- 250
  x <- rnorm(n)
  # Gumbel (minimum) errors for loglog
  latent <- 0.7 * x + log(-log(runif(n)))
  y <- cut(latent, breaks = c(-Inf, -1, 0, Inf), labels = 1:3)
  data <- data.frame(y = y, x = x)

  fit_clm <- ordinal::clm(y ~ x, data = data, link = "loglog")
  fit_clmstan <- clm_stan(
    y ~ x,
    data = data,
    link = "loglog",
    chains = 4,
    iter = 2000,
    warmup = 1000,
    refresh = 0
  )

  coef_clm <- coef(fit_clm)
  coef_clmstan <- coef(fit_clmstan, type = "mean")

  expect_equal(
    unname(coef_clmstan["x"]),
    unname(coef_clm["x"]),
    tolerance = 0.20
  )
})

test_that("clmstan matches ordinal::clm for cauchit link", {
  skip_if_not(instantiate::stan_cmdstan_exists(), "CmdStan not available")
  skip_if_not(stan_models_compiled(), "Stan models not compiled (install package first)")
  skip_if_not_installed("ordinal")

  set.seed(456)
  n <- 300
  x <- rnorm(n)
  latent <- 0.5 * x + rcauchy(n)
  y <- cut(latent, breaks = c(-Inf, -1, 1, Inf), labels = 1:3)
  data <- data.frame(y = y, x = x)

  fit_clm <- ordinal::clm(y ~ x, data = data, link = "cauchit")
  fit_clmstan <- clm_stan(
    y ~ x,
    data = data,
    link = "cauchit",
    chains = 4,
    iter = 2000,
    warmup = 1000,
    refresh = 0
  )

  coef_clm <- coef(fit_clm)
  coef_clmstan <- coef(fit_clmstan, type = "mean")

  # Cauchit can have more variability due to heavy tails

  expect_equal(
    unname(coef_clmstan["x"]),
    unname(coef_clm["x"]),
    tolerance = 0.25
  )
})

test_that("clmstan matches ordinal::clm on wine dataset", {
  skip_if_not(instantiate::stan_cmdstan_exists(), "CmdStan not available")
  skip_if_not(stan_models_compiled(), "Stan models not compiled (install package first)")
  skip_if_not_installed("ordinal")

  data(wine, package = "ordinal")

  fit_clm <- ordinal::clm(rating ~ temp + contact, data = wine, link = "logit")
  fit_clmstan <- clm_stan(
    rating ~ temp + contact,
    data = wine,
    link = "logit",
    chains = 4,
    iter = 2000,
    warmup = 1000,
    refresh = 0
  )

  coef_clm <- coef(fit_clm)
  coef_clmstan <- coef(fit_clmstan, type = "mean")

  expect_equal(
    unname(coef_clmstan["tempwarm"]),
    unname(coef_clm["tempwarm"]),
    tolerance = 0.2
  )

  expect_equal(
    unname(coef_clmstan["contactyes"]),
    unname(coef_clm["contactyes"]),
    tolerance = 0.2
  )

  # Compare standard errors (informative check)
  se_clm <- sqrt(diag(vcov(fit_clm)))
  summ <- summary(fit_clmstan)
  sd_tempwarm <- summ$coefficients$sd[summ$coefficients$variable == "tempwarm"]

  # Posterior SD should be comparable to frequentist SE
  expect_true(
    sd_tempwarm / se_clm["tempwarm"] > 0.5 &&
    sd_tempwarm / se_clm["tempwarm"] < 2.0,
    label = "Posterior SD comparable to SE"
  )
})

test_that("clmstan matches ordinal::clm on intercept-only model", {
  skip_if_not(instantiate::stan_cmdstan_exists(), "CmdStan not available")
  skip_if_not(stan_models_compiled(), "Stan models not compiled (install package first)")
  skip_if_not_installed("ordinal")

  set.seed(567)
  n <- 200
  probs <- c(0.2, 0.3, 0.3, 0.2)
  y <- factor(sample(1:4, n, replace = TRUE, prob = probs))
  data <- data.frame(y = y)

  fit_clm <- ordinal::clm(y ~ 1, data = data, link = "logit")
  fit_clmstan <- clm_stan(
    y ~ 1,
    data = data,
    link = "logit",
    chains = 4,
    iter = 2000,
    warmup = 1000,
    refresh = 0
  )

  coef_clm <- coef(fit_clm)
  coef_clmstan <- coef(fit_clmstan, type = "mean")

  # Transform ordinal::clm thresholds to c1=0 parameterization
  thresh_clm_transformed <- transform_thresholds_c1_zero(coef_clm, K = 4)

  # Compare thresholds directly
  thresh_names <- c("1|2", "2|3", "3|4")
  for (tname in thresh_names) {
    expect_equal(
      unname(coef_clmstan[tname]),
      unname(thresh_clm_transformed[tname]),
      tolerance = 0.20,
      label = paste("Intercept-only threshold", tname)
    )
  }
})
