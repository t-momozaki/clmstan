# Comparison tests with brms
#
# NOTE: These tests require the package to be installed (not just loaded).

# Helper to check if Stan models are compiled (package must be installed)
stan_models_compiled <- function() {
  model_path <- system.file("bin", "stan", "clm_base", package = "clmstan")
  nzchar(model_path) && file.exists(model_path)
}

test_that("clmstan matches brms cumulative for logit link", {
  skip_if_not(instantiate::stan_cmdstan_exists(), "CmdStan not available")
  skip_if_not(stan_models_compiled(), "Stan models not compiled (install package first)")
  skip_if_not_installed("brms")

  set.seed(123)
  n <- 200
  x <- rnorm(n)
  latent <- 0.8 * x + rlogis(n)
  y <- ordered(cut(latent, breaks = c(-Inf, -0.5, 0.5, Inf), labels = 1:3))
  data <- data.frame(y = y, x = x)

  fit_brms <- brms::brm(
    y ~ x,
    family = brms::cumulative("logit"),
    data = data,
    chains = 2,
    iter = 1500,
    warmup = 750,
    refresh = 0,
    silent = 2
  )

  fit_clmstan <- clm_stan(
    y ~ x,
    data = data,
    link = "logit",
    chains = 4,
    iter = 2000,
    warmup = 1000,
    refresh = 0
  )

  brms_beta <- brms::fixef(fit_brms)["x", "Estimate"]
  clmstan_beta <- coef(fit_clmstan, type = "mean")["x"]

  expect_equal(
    unname(clmstan_beta),
    brms_beta,
    tolerance = 0.15
  )
})

test_that("clmstan matches brms cumulative for probit link", {
  skip_if_not(instantiate::stan_cmdstan_exists(), "CmdStan not available")
  skip_if_not(stan_models_compiled(), "Stan models not compiled (install package first)")
  skip_if_not_installed("brms")

  set.seed(234)
  n <- 200
  x <- rnorm(n)
  latent <- 0.6 * x + rnorm(n)
  y <- ordered(cut(latent, breaks = c(-Inf, -0.3, 0.3, Inf), labels = 1:3))
  data <- data.frame(y = y, x = x)

  fit_brms <- brms::brm(
    y ~ x,
    family = brms::cumulative("probit"),
    data = data,
    chains = 2,
    iter = 1500,
    warmup = 750,
    refresh = 0,
    silent = 2
  )

  fit_clmstan <- clm_stan(
    y ~ x,
    data = data,
    link = "probit",
    chains = 4,
    iter = 2000,
    warmup = 1000,
    refresh = 0
  )

  brms_beta <- brms::fixef(fit_brms)["x", "Estimate"]
  clmstan_beta <- coef(fit_clmstan, type = "mean")["x"]

  expect_equal(
    unname(clmstan_beta),
    brms_beta,
    tolerance = 0.15
  )
})

test_that("clmstan matches brms cumulative with multiple predictors", {
  skip_if_not(instantiate::stan_cmdstan_exists(), "CmdStan not available")
  skip_if_not(stan_models_compiled(), "Stan models not compiled (install package first)")
  skip_if_not_installed("brms")

  set.seed(345)
  n <- 250
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  latent <- 0.7 * x1 - 0.4 * x2 + rlogis(n)
  y <- ordered(cut(latent, breaks = c(-Inf, -0.5, 0.5, 1.5, Inf), labels = 1:4))
  data <- data.frame(y = y, x1 = x1, x2 = x2)

  fit_brms <- brms::brm(
    y ~ x1 + x2,
    family = brms::cumulative("logit"),
    data = data,
    chains = 2,
    iter = 1500,
    warmup = 750,
    refresh = 0,
    silent = 2
  )

  fit_clmstan <- clm_stan(
    y ~ x1 + x2,
    data = data,
    link = "logit",
    chains = 4,
    iter = 2000,
    warmup = 1000,
    refresh = 0
  )

  brms_coefs <- brms::fixef(fit_brms)[, "Estimate"]
  clmstan_coefs <- coef(fit_clmstan, type = "mean")

  expect_equal(
    unname(clmstan_coefs["x1"]),
    unname(brms_coefs["x1"]),
    tolerance = 0.20
  )

  expect_equal(
    unname(clmstan_coefs["x2"]),
    unname(brms_coefs["x2"]),
    tolerance = 0.20
  )
})

test_that("clmstan and brms produce similar posterior uncertainty", {
  skip_if_not(instantiate::stan_cmdstan_exists(), "CmdStan not available")
  skip_if_not(stan_models_compiled(), "Stan models not compiled (install package first)")
  skip_if_not_installed("brms")

  set.seed(456)
  n <- 200
  x <- rnorm(n)
  latent <- 1.0 * x + rlogis(n)
  y <- ordered(cut(latent, breaks = c(-Inf, 0, 1, Inf), labels = 1:3))
  data <- data.frame(y = y, x = x)

  fit_brms <- brms::brm(
    y ~ x,
    family = brms::cumulative("logit"),
    data = data,
    chains = 2,
    iter = 1500,
    warmup = 750,
    refresh = 0,
    silent = 2
  )

  fit_clmstan <- clm_stan(
    y ~ x,
    data = data,
    link = "logit",
    chains = 4,
    iter = 2000,
    warmup = 1000,
    refresh = 0
  )

  brms_sd <- brms::fixef(fit_brms)["x", "Est.Error"]
  summ <- summary(fit_clmstan)
  clmstan_sd <- summ$coefficients$sd[summ$coefficients$variable == "x"]

  # Posterior SDs should be in the same ballpark (within factor of 2)
  expect_true(
    clmstan_sd / brms_sd > 0.5 && clmstan_sd / brms_sd < 2.0,
    label = "Posterior SDs comparable between packages"
  )
})
