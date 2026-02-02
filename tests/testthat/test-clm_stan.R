# Tests for clm_stan main function
#
# These tests verify the core functionality of clm_stan(), including:
# - Basic model fitting and object creation
# - Warmup parameter auto-adjustment (consistent with rstan/brms)
# - Input validation (iter > warmup)
# - Edge cases (odd iteration numbers, etc.)
#
# NOTE: These tests require compiled Stan models.
# - Skipped in devtools::test() (development mode)
# - Run in R CMD check (after package build/install)

# Helper to check if Stan models are compiled
stan_models_compiled <- function() {
  model_path <- system.file("bin", "stan", "clm_base", package = "clmstan")
  nzchar(model_path) && file.exists(model_path)
}

# =============================================================================
# Basic functionality tests
# =============================================================================

test_that("clm_stan returns valid clmstan object", {
  skip_on_cran()
  skip_if_not(instantiate::stan_cmdstan_exists(), "CmdStan not available")
  skip_if_not(stan_models_compiled(), "Stan models not compiled")

  data(wine, package = "ordinal")

  fit <- clm_stan(rating ~ temp, data = wine,
                  chains = 1, iter = 200, refresh = 0)

  # Check class
  expect_s3_class(fit, "clmstan")

  # Check basic methods work
  expect_no_error(print(fit))
  expect_no_error(summary(fit))
  expect_no_error(coef(fit))
})

# =============================================================================
# Warmup parameter auto-adjustment tests
# =============================================================================

test_that("warmup parameter auto-adjustment works", {
  skip_on_cran()
  skip_if_not(instantiate::stan_cmdstan_exists(), "CmdStan not available")
  skip_if_not(stan_models_compiled(), "Stan models not compiled")

  data(wine, package = "ordinal")

  # Test 1: warmup = NULL (default) uses floor(iter/2)
  fit <- clm_stan(rating ~ temp, data = wine,
                  chains = 1, iter = 200,  # warmup未指定
                  refresh = 0)

  # iter_sampling = 200 - floor(200/2) = 200 - 100 = 100
  expect_equal(fit$metadata()$iter_sampling, 100)
  expect_equal(fit$metadata()$iter_warmup, 100)

  # Test 2: explicit warmup is respected
  fit2 <- clm_stan(rating ~ temp, data = wine,
                   chains = 1, iter = 200, warmup = 50,
                   refresh = 0)

  # iter_sampling = 200 - 50 = 150
  expect_equal(fit2$metadata()$iter_sampling, 150)
  expect_equal(fit2$metadata()$iter_warmup, 50)
})

test_that("odd iter values work correctly with auto-adjustment", {
  skip_on_cran()
  skip_if_not(instantiate::stan_cmdstan_exists(), "CmdStan not available")
  skip_if_not(stan_models_compiled(), "Stan models not compiled")

  data(wine, package = "ordinal")

  # iter = 201 (odd number) → warmup = floor(201/2) = 100
  fit <- clm_stan(rating ~ temp, data = wine,
                  chains = 1, iter = 201, refresh = 0)

  expect_equal(fit$metadata()$iter_warmup, 100)
  expect_equal(fit$metadata()$iter_sampling, 101)
})

# =============================================================================
# Input validation tests
# =============================================================================

test_that("iter <= warmup validation works", {
  data(wine, package = "ordinal")

  # iter < warmup should error
  expect_error(
    clm_stan(rating ~ temp, data = wine,
             chains = 1, iter = 100, warmup = 200),
    regexp = "iter.*must be greater than.*warmup"
  )

  # iter == warmup should error
  expect_error(
    clm_stan(rating ~ temp, data = wine,
             chains = 1, iter = 100, warmup = 100),
    regexp = "iter.*must be greater than.*warmup"
  )
})
