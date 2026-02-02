# Tests for parameter recovery with simulation
#
# These tests verify that clmstan can recover true parameters from
# simulated ordinal data. They use longer chains for accuracy.
#
# NOTE: These tests require the package to be installed (not just loaded).
# Run with: R CMD INSTALL . && Rscript validation/run-validation.R

# Helper to check if Stan models are compiled (package must be installed)
stan_models_compiled <- function() {
  model_path <- system.file("bin", "stan", "clm_base", package = "clmstan")
  nzchar(model_path) && file.exists(model_path)
}

# =============================================================================
# Standard Link Functions - Parameter Recovery
# =============================================================================

test_that("clmstan recovers true parameters with logit link", {
  skip_if_not(instantiate::stan_cmdstan_exists(), "CmdStan not available")
  skip_if_not(stan_models_compiled(), "Stan models not compiled (install package first)")

  set.seed(12345)
  true_beta <- c(1.2, -0.7)
  true_thresholds <- c(0, 1.0, 2.5)

  sim <- simulate_ordinal_data(
    n = 300,
    K = 4,
    beta = true_beta,
    thresholds = true_thresholds,
    link = "logit",
    seed = 12345
  )

  fit <- clm_stan(
    y ~ x1 + x2,
    data = sim$data,
    link = "logit",
    chains = 4,
    iter = 2000,
    warmup = 1000,
    refresh = 0
  )

  coefs <- coef(fit, type = "mean")

  # Test point estimates
  expect_equal(unname(coefs["x1"]), true_beta[1], tolerance = 0.25,
               label = "beta[1] recovery")
  expect_equal(unname(coefs["x2"]), true_beta[2], tolerance = 0.25,
               label = "beta[2] recovery")
  expect_equal(unname(coefs["2|3"]), true_thresholds[2], tolerance = 0.25,
               label = "threshold[2] recovery")
  expect_equal(unname(coefs["3|4"]), true_thresholds[3], tolerance = 0.30,
               label = "threshold[3] recovery")

  # Test 95% CI coverage for beta
  summ <- summary(fit)
  beta1_row <- summ$coefficients[summ$coefficients$variable == "x1", ]
  expect_true(
    true_beta[1] >= beta1_row$`2.5%` && true_beta[1] <= beta1_row$`97.5%`,
    label = "95% CI for beta[1] covers true value"
  )

  beta2_row <- summ$coefficients[summ$coefficients$variable == "x2", ]
  expect_true(
    true_beta[2] >= beta2_row$`2.5%` && true_beta[2] <= beta2_row$`97.5%`,
    label = "95% CI for beta[2] covers true value"
  )

  # Test convergence
  diag_result <- diagnostics(fit)
  expect_false(diag_result$issues, label = "No convergence issues")
})

test_that("clmstan recovers true parameters with probit link", {
  skip_if_not(instantiate::stan_cmdstan_exists(), "CmdStan not available")
  skip_if_not(stan_models_compiled(), "Stan models not compiled (install package first)")

  set.seed(23456)
  true_beta <- c(0.8)
  true_thresholds <- c(0, 0.8, 1.6)

  sim <- simulate_ordinal_data(
    n = 300,
    K = 4,
    beta = true_beta,
    thresholds = true_thresholds,
    link = "probit",
    seed = 23456
  )

  fit <- clm_stan(
    y ~ x1,
    data = sim$data,
    link = "probit",
    chains = 4,
    iter = 2000,
    warmup = 1000,
    refresh = 0
  )

  coefs <- coef(fit, type = "mean")

  expect_equal(unname(coefs["x1"]), true_beta[1], tolerance = 0.25,
               label = "beta recovery with probit")

  # Check convergence
  diag_result <- diagnostics(fit)
  expect_false(diag_result$issues)
})

test_that("clmstan recovers true parameters with cloglog link", {
  skip_if_not(instantiate::stan_cmdstan_exists(), "CmdStan not available")
  skip_if_not(stan_models_compiled(), "Stan models not compiled (install package first)")

  set.seed(34567)
  true_beta <- c(0.6)
  true_thresholds <- c(0, 0.7, 1.5)

  sim <- simulate_ordinal_data(
    n = 300,
    K = 4,
    beta = true_beta,
    thresholds = true_thresholds,
    link = "cloglog",
    seed = 34567
  )

  fit <- clm_stan(
    y ~ x1,
    data = sim$data,
    link = "cloglog",
    chains = 4,
    iter = 2000,
    warmup = 1000,
    refresh = 0
  )

  coefs <- coef(fit, type = "mean")

  expect_equal(unname(coefs["x1"]), true_beta[1], tolerance = 0.30,
               label = "beta recovery with cloglog")

  diag_result <- diagnostics(fit)
  expect_false(diag_result$issues)
})

# =============================================================================
# Flexible Link Functions - Fixed Parameters
# =============================================================================

test_that("clmstan recovers true parameters with GEV link (fixed xi)", {
  skip_if_not(instantiate::stan_cmdstan_exists(), "CmdStan not available")
  skip_if_not(stan_models_compiled(), "Stan models not compiled (install package first)")

  set.seed(45678)
  true_beta <- c(0.9)
  true_thresholds <- c(0, 0.9, 2.0)
  true_xi <- 0.2  # Frechet-type (heavy right tail)

  sim <- simulate_ordinal_data(
    n = 400,
    K = 4,
    beta = true_beta,
    thresholds = true_thresholds,
    link = "gev",
    link_param = list(xi = true_xi),
    seed = 45678
  )

  fit <- clm_stan(
    y ~ x1,
    data = sim$data,
    link = "gev",
    link_param = list(xi = true_xi),
    chains = 4,
    iter = 2000,
    warmup = 1000,
    refresh = 0
  )

  coefs <- coef(fit, type = "mean")

  expect_equal(unname(coefs["x1"]), true_beta[1], tolerance = 0.30,
               label = "beta recovery with GEV")
  expect_equal(unname(coefs["2|3"]), true_thresholds[2], tolerance = 0.30,
               label = "threshold[2] recovery with GEV")

  diag_result <- diagnostics(fit)
  expect_false(diag_result$issues)
})

test_that("clmstan recovers true parameters with AEP link (fixed theta)", {
  skip_if_not(instantiate::stan_cmdstan_exists(), "CmdStan not available")
  skip_if_not(stan_models_compiled(), "Stan models not compiled (install package first)")

  set.seed(56789)
  true_beta <- c(1.0)
  true_thresholds <- c(0, 1.0, 2.2)
  true_theta1 <- 2
  true_theta2 <- 2

  sim <- simulate_ordinal_data(
    n = 400,
    K = 4,
    beta = true_beta,
    thresholds = true_thresholds,
    link = "aep",
    link_param = list(theta1 = true_theta1, theta2 = true_theta2),
    seed = 56789
  )

  fit <- clm_stan(
    y ~ x1,
    data = sim$data,
    link = "aep",
    link_param = list(theta1 = true_theta1, theta2 = true_theta2),
    chains = 4,
    iter = 2000,
    warmup = 1000,
    refresh = 0
  )

  coefs <- coef(fit, type = "mean")

  expect_equal(unname(coefs["x1"]), true_beta[1], tolerance = 0.30,
               label = "beta recovery with AEP")

  diag_result <- diagnostics(fit)
  expect_false(diag_result$issues)
})

# =============================================================================
# Flexible Link Functions - Estimated Parameters
# =============================================================================

test_that("clmstan estimates xi parameter in GEV link", {
  skip_if_not(instantiate::stan_cmdstan_exists(), "CmdStan not available")
  skip_if_not(stan_models_compiled(), "Stan models not compiled (install package first)")

  set.seed(67890)
  true_beta <- c(1.0)
  true_thresholds <- c(0, 1.0, 2.2)
  true_xi <- 0.15

  sim <- simulate_ordinal_data(
    n = 500,
    K = 4,
    beta = true_beta,
    thresholds = true_thresholds,
    link = "gev",
    link_param = list(xi = true_xi),
    seed = 67890
  )

  fit <- clm_stan(
    y ~ x1,
    data = sim$data,
    link = "gev",
    link_param = list(xi = "estimate"),
    chains = 4,
    iter = 3000,
    warmup = 1500,
    refresh = 0
  )

  # Extract xi estimate from Stan fit
  xi_draws <- fit$fit$draws(variables = "xi", format = "matrix")
  xi_mean <- mean(xi_draws)
  xi_q025 <- quantile(xi_draws, 0.025)
  xi_q975 <- quantile(xi_draws, 0.975)

  # Test xi recovery
  expect_equal(xi_mean, true_xi, tolerance = 0.15,
               label = "xi recovery")

  # Check 95% CI covers true value
  expect_true(
    true_xi >= xi_q025 && true_xi <= xi_q975,
    label = "95% CI for xi covers true value"
  )

  # Beta should still be recovered (with slightly wider tolerance)
  coefs <- coef(fit, type = "mean")
  expect_equal(unname(coefs["x1"]), true_beta[1], tolerance = 0.35,
               label = "beta recovery with estimated xi")
})

test_that("clmstan estimates theta parameters in AEP link", {
  skip_if_not(instantiate::stan_cmdstan_exists(), "CmdStan not available")
  skip_if_not(stan_models_compiled(), "Stan models not compiled (install package first)")

  set.seed(78901)
  true_beta <- c(0.8)
  true_thresholds <- c(0, 0.9, 1.8)
  true_theta1 <- 1.5
  true_theta2 <- 2.5  # Asymmetric

  sim <- simulate_ordinal_data(
    n = 500,
    K = 4,
    beta = true_beta,
    thresholds = true_thresholds,
    link = "aep",
    link_param = list(theta1 = true_theta1, theta2 = true_theta2),
    seed = 78901
  )

  fit <- clm_stan(
    y ~ x1,
    data = sim$data,
    link = "aep",
    link_param = list(theta1 = "estimate", theta2 = "estimate"),
    chains = 4,
    iter = 3000,
    warmup = 1500,
    refresh = 0
  )

  # Extract theta estimates
  theta1_draws <- fit$fit$draws(variables = "theta1", format = "matrix")
  theta2_draws <- fit$fit$draws(variables = "theta2", format = "matrix")

  theta1_mean <- mean(theta1_draws)
  theta2_mean <- mean(theta2_draws)

  # Test theta recovery (wider tolerance for shape parameters)
  expect_equal(theta1_mean, true_theta1, tolerance = 0.4,
               label = "theta1 recovery")
  expect_equal(theta2_mean, true_theta2, tolerance = 0.4,
               label = "theta2 recovery")

  # Beta should still be recovered
  coefs <- coef(fit, type = "mean")
  expect_equal(unname(coefs["x1"]), true_beta[1], tolerance = 0.40,
               label = "beta recovery with estimated theta")
})

# =============================================================================
# Intercept-only Model
# =============================================================================

test_that("clmstan recovers thresholds in intercept-only model", {
  skip_if_not(instantiate::stan_cmdstan_exists(), "CmdStan not available")
  skip_if_not(stan_models_compiled(), "Stan models not compiled (install package first)")

  set.seed(89012)
  true_thresholds <- c(0, 1.5, 3.0)

  sim <- simulate_ordinal_data(
    n = 300,
    K = 4,
    beta = numeric(0),  # No predictors
    thresholds = true_thresholds,
    link = "logit",
    seed = 89012
  )

  fit <- clm_stan(
    y ~ 1,
    data = sim$data,
    link = "logit",
    chains = 4,
    iter = 2000,
    warmup = 1000,
    refresh = 0
  )

  coefs <- coef(fit, type = "mean")

  # Check thresholds (note: first threshold is always 0 after transformation)
  expect_equal(unname(coefs["1|2"]), 0, tolerance = 1e-10,
               label = "first threshold = 0")
  expect_equal(unname(coefs["2|3"]), true_thresholds[2], tolerance = 0.25,
               label = "threshold[2] recovery")
  expect_equal(unname(coefs["3|4"]), true_thresholds[3], tolerance = 0.30,
               label = "threshold[3] recovery")
})

# =============================================================================
# Multiple Predictors
# =============================================================================

test_that("clmstan recovers parameters with multiple predictors", {
  skip_if_not(instantiate::stan_cmdstan_exists(), "CmdStan not available")
  skip_if_not(stan_models_compiled(), "Stan models not compiled (install package first)")

  set.seed(90123)
  true_beta <- c(1.0, -0.5, 0.3)
  true_thresholds <- c(0, 0.8, 1.8)

  sim <- simulate_ordinal_data(
    n = 400,
    K = 4,
    beta = true_beta,
    thresholds = true_thresholds,
    link = "logit",
    seed = 90123
  )

  fit <- clm_stan(
    y ~ x1 + x2 + x3,
    data = sim$data,
    link = "logit",
    chains = 4,
    iter = 2000,
    warmup = 1000,
    refresh = 0
  )

  coefs <- coef(fit, type = "mean")

  expect_equal(unname(coefs["x1"]), true_beta[1], tolerance = 0.25)
  expect_equal(unname(coefs["x2"]), true_beta[2], tolerance = 0.25)
  expect_equal(unname(coefs["x3"]), true_beta[3], tolerance = 0.25)

  diag_result <- diagnostics(fit)
  expect_false(diag_result$issues)
})

# =============================================================================
# Coverage Test (Optional - Long Running)
# =============================================================================

test_that("clmstan achieves nominal 95% CI coverage", {
  skip_if_not(instantiate::stan_cmdstan_exists(), "CmdStan not available")
  skip("Long-running coverage test - run manually with skip = FALSE")

  n_reps <- 50
  n <- 200
  K <- 3
  true_beta <- c(1.0)
  true_thresholds <- c(0, 1.5)

  coverage_beta <- logical(n_reps)

  for (rep in seq_len(n_reps)) {
    sim <- simulate_ordinal_data(
      n = n,
      K = K,
      beta = true_beta,
      thresholds = true_thresholds,
      link = "logit",
      seed = 1000 + rep
    )

    fit <- clm_stan(
      y ~ x1,
      data = sim$data,
      link = "logit",
      chains = 2,
      iter = 1500,
      warmup = 750,
      refresh = 0
    )

    # Check if 95% CI covers true beta
    beta_draws <- fit$fit$draws(variables = "beta[1]", format = "matrix")
    q025 <- quantile(beta_draws, 0.025)
    q975 <- quantile(beta_draws, 0.975)
    coverage_beta[rep] <- (true_beta[1] >= q025 && true_beta[1] <= q975)
  }

  # Expected: 95% coverage, accept 85-98% given Monte Carlo variability
  coverage_rate <- mean(coverage_beta)
  expect_true(
    coverage_rate >= 0.85 && coverage_rate <= 0.98,
    label = sprintf("Coverage rate %.2f in [0.85, 0.98]", coverage_rate)
  )
})
