# Tests for link function sampling
#
# These tests verify that all 11 link functions can run MCMC sampling
# without errors. They use minimal iterations for speed.
#
# NOTE: These tests require compiled Stan models.
# - Skipped in devtools::test() (development mode)
# - Run in R CMD check (after package build/install)

# Helper to check if Stan models are compiled
stan_models_compiled <- function() {
  model_path <- system.file("bin", "stan", "clm_base", package = "clmstan")
  nzchar(model_path) && file.exists(model_path)
}

# Helper function to generate simple ordinal test data
# This is a simplified version that doesn't depend on clm_cdf
generate_test_data <- function(n = 50, K = 3, link = "logit",
                                link_param = list(), seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  # Generate simple data using logistic latent variable
  x1 <- rnorm(n)
  latent <- 0.5 * x1 + rlogis(n)

  # Create thresholds
  thresholds <- seq(-1, 1, length.out = K - 1)

  # Create ordinal response
  y <- cut(latent, breaks = c(-Inf, thresholds, Inf), labels = 1:K)

  data <- data.frame(y = y, x1 = x1)

  list(
    data = data,
    true_params = list(
      beta = c(0.5),
      thresholds = c(0, thresholds[-1] - thresholds[1]),
      link = link,
      link_param = link_param
    )
  )
}

# =============================================================================
# Standard Link Functions (5)
# =============================================================================

test_that("logit link runs without error", {
  skip_on_cran()
  skip_if_not(instantiate::stan_cmdstan_exists(), "CmdStan not available")
  skip_if_not(stan_models_compiled(), "Stan models not compiled")

  sim <- generate_test_data(n = 50, K = 3, link = "logit", seed = 101)

  expect_no_error({
    fit <- clm_stan(
      y ~ x1,
      data = sim$data,
      link = "logit",
      chains = 1,
      iter = 100,
      warmup = 50,
      refresh = 0
    )
  })
})

test_that("probit link runs without error", {
  skip_on_cran()
  skip_if_not(instantiate::stan_cmdstan_exists(), "CmdStan not available")
  skip_if_not(stan_models_compiled(), "Stan models not compiled")

  sim <- generate_test_data(n = 50, K = 3, link = "probit", seed = 102)

  expect_no_error({
    fit <- clm_stan(
      y ~ x1,
      data = sim$data,
      link = "probit",
      chains = 1,
      iter = 100,
      warmup = 50,
      refresh = 0
    )
  })
})

test_that("cloglog link runs without error", {
  skip_on_cran()
  skip_if_not(instantiate::stan_cmdstan_exists(), "CmdStan not available")
  skip_if_not(stan_models_compiled(), "Stan models not compiled")

  sim <- generate_test_data(n = 50, K = 3, link = "cloglog", seed = 103)

  expect_no_error({
    fit <- clm_stan(
      y ~ x1,
      data = sim$data,
      link = "cloglog",
      chains = 1,
      iter = 100,
      warmup = 50,
      refresh = 0
    )
  })
})

test_that("loglog link runs without error", {
  skip_on_cran()
  skip_if_not(instantiate::stan_cmdstan_exists(), "CmdStan not available")
  skip_if_not(stan_models_compiled(), "Stan models not compiled")

  sim <- generate_test_data(n = 50, K = 3, link = "loglog", seed = 104)

  expect_no_error({
    fit <- clm_stan(
      y ~ x1,
      data = sim$data,
      link = "loglog",
      chains = 1,
      iter = 100,
      warmup = 50,
      refresh = 0
    )
  })
})

test_that("cauchit link runs without error", {
  skip_on_cran()
  skip_if_not(instantiate::stan_cmdstan_exists(), "CmdStan not available")
  skip_if_not(stan_models_compiled(), "Stan models not compiled")

  sim <- generate_test_data(n = 50, K = 3, link = "cauchit", seed = 105)

  expect_no_error({
    fit <- clm_stan(
      y ~ x1,
      data = sim$data,
      link = "cauchit",
      chains = 1,
      iter = 100,
      warmup = 50,
      refresh = 0
    )
  })
})

# =============================================================================
# Flexible Link Functions (6) - Fixed Parameters
# =============================================================================

test_that("tlink with fixed df runs without error", {
  skip_on_cran()
  skip_if_not(instantiate::stan_cmdstan_exists(), "CmdStan not available")
  skip_if_not(stan_models_compiled(), "Stan models not compiled")

  sim <- generate_test_data(
    n = 50, K = 3,
    link = "tlink",
    link_param = list(df = 8),
    seed = 106
  )

  expect_no_error({
    fit <- clm_stan(
      y ~ x1,
      data = sim$data,
      link = "tlink",
      link_param = list(df = 8),
      chains = 1,
      iter = 100,
      warmup = 50,
      refresh = 0
    )
  })
})

test_that("aranda_ordaz with fixed lambda runs without error", {
  skip_on_cran()
  skip_if_not(instantiate::stan_cmdstan_exists(), "CmdStan not available")
  skip_if_not(stan_models_compiled(), "Stan models not compiled")

  sim <- generate_test_data(
    n = 50, K = 3,
    link = "aranda_ordaz",
    link_param = list(lambda = 1),
    seed = 107
  )

  expect_no_error({
    fit <- clm_stan(
      y ~ x1,
      data = sim$data,
      link = "aranda_ordaz",
      link_param = list(lambda = 1),
      chains = 1,
      iter = 100,
      warmup = 50,
      refresh = 0
    )
  })
})

test_that("sp with fixed r runs without error", {
  skip_on_cran()
  skip_if_not(instantiate::stan_cmdstan_exists(), "CmdStan not available")
  skip_if_not(stan_models_compiled(), "Stan models not compiled")

  sim <- generate_test_data(
    n = 50, K = 3,
    link = "sp",
    link_param = list(r = 1, base = "logit"),
    seed = 108
  )

  expect_no_error({
    fit <- clm_stan(
      y ~ x1,
      data = sim$data,
      link = "sp",
      link_param = list(r = 1),
      base = "logit",
      chains = 1,
      iter = 100,
      warmup = 50,
      refresh = 0
    )
  })
})

test_that("log_gamma with fixed lambda runs without error", {
  skip_on_cran()
  skip_if_not(instantiate::stan_cmdstan_exists(), "CmdStan not available")
  skip_if_not(stan_models_compiled(), "Stan models not compiled")

  sim <- generate_test_data(
    n = 50, K = 3,
    link = "log_gamma",
    link_param = list(lambda = 0.5),
    seed = 109
  )

  expect_no_error({
    fit <- clm_stan(
      y ~ x1,
      data = sim$data,
      link = "log_gamma",
      link_param = list(lambda = 0.5),
      chains = 1,
      iter = 100,
      warmup = 50,
      refresh = 0
    )
  })
})

test_that("gev with fixed xi runs without error", {
  skip_on_cran()
  skip_if_not(instantiate::stan_cmdstan_exists(), "CmdStan not available")
  skip_if_not(stan_models_compiled(), "Stan models not compiled")

  sim <- generate_test_data(
    n = 50, K = 3,
    link = "gev",
    link_param = list(xi = 0.2),
    seed = 110
  )

  expect_no_error({
    fit <- clm_stan(
      y ~ x1,
      data = sim$data,
      link = "gev",
      link_param = list(xi = 0.2),
      chains = 1,
      iter = 100,
      warmup = 50,
      refresh = 0
    )
  })
})

test_that("aep with fixed theta1, theta2 runs without error", {
  skip_on_cran()
  skip_if_not(instantiate::stan_cmdstan_exists(), "CmdStan not available")
  skip_if_not(stan_models_compiled(), "Stan models not compiled")

  sim <- generate_test_data(
    n = 50, K = 3,
    link = "aep",
    link_param = list(theta1 = 2, theta2 = 2),
    seed = 111
  )

  expect_no_error({
    fit <- clm_stan(
      y ~ x1,
      data = sim$data,
      link = "aep",
      link_param = list(theta1 = 2, theta2 = 2),
      chains = 1,
      iter = 100,
      warmup = 50,
      refresh = 0
    )
  })
})

# =============================================================================
# Flexible Link Functions - Estimated Parameters (Full Model)
# =============================================================================

test_that("gev with estimated xi runs without error", {
  skip_on_cran()
  skip_if_not(instantiate::stan_cmdstan_exists(), "CmdStan not available")
  skip_if_not(stan_models_compiled(), "Stan models not compiled")

  # Use data generated with xi = 0.2
  sim <- generate_test_data(
    n = 50, K = 3,
    link = "gev",
    link_param = list(xi = 0.2),
    seed = 120
  )

  expect_no_error({
    fit <- clm_stan(
      y ~ x1,
      data = sim$data,
      link = "gev",
      link_param = list(xi = "estimate"),
      chains = 1,
      iter = 100,
      warmup = 50,
      refresh = 0
    )
  })
})

test_that("aep with estimated theta1, theta2 runs without error", {
  skip_on_cran()
  skip_if_not(instantiate::stan_cmdstan_exists(), "CmdStan not available")
  skip_if_not(stan_models_compiled(), "Stan models not compiled")

  sim <- generate_test_data(
    n = 50, K = 3,
    link = "aep",
    link_param = list(theta1 = 2, theta2 = 2),
    seed = 121
  )

  expect_no_error({
    fit <- clm_stan(
      y ~ x1,
      data = sim$data,
      link = "aep",
      link_param = list(theta1 = "estimate", theta2 = "estimate"),
      chains = 1,
      iter = 100,
      warmup = 50,
      refresh = 0
    )
  })
})

# =============================================================================
# Different Threshold Structures
# =============================================================================

test_that("logit with equidistant thresholds runs without error", {
  skip_on_cran()
  skip_if_not(instantiate::stan_cmdstan_exists(), "CmdStan not available")
  skip_if_not(stan_models_compiled(), "Stan models not compiled")

  sim <- generate_test_data(n = 50, K = 4, link = "logit", seed = 130)

  expect_no_error({
    fit <- clm_stan(
      y ~ x1,
      data = sim$data,
      link = "logit",
      threshold = "equidistant",
      chains = 1,
      iter = 100,
      warmup = 50,
      refresh = 0
    )
  })
})

test_that("logit with symmetric thresholds runs without error", {
  skip_on_cran()
  skip_if_not(instantiate::stan_cmdstan_exists(), "CmdStan not available")
  skip_if_not(stan_models_compiled(), "Stan models not compiled")

  # Symmetric thresholds work best with odd K
  sim <- generate_test_data(n = 50, K = 5, link = "logit", seed = 131)

  expect_no_error({
    fit <- clm_stan(
      y ~ x1,
      data = sim$data,
      link = "logit",
      threshold = "symmetric",
      chains = 1,
      iter = 100,
      warmup = 50,
      refresh = 0
    )
  })
})
