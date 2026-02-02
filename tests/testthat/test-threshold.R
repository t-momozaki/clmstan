# =============================================================================
# Tests for threshold structure functions
# =============================================================================

test_that("supported_thresholds returns expected values", {
  thresholds <- supported_thresholds()
  expect_type(thresholds, "character")
  expect_equal(thresholds, c("flexible", "equidistant", "symmetric"))
})

test_that("validate_threshold accepts valid thresholds", {
  for (threshold in supported_thresholds()) {
    expect_true(validate_threshold(threshold))
  }
})

test_that("validate_threshold rejects invalid thresholds", {
  expect_error(validate_threshold("invalid_threshold"))
  expect_error(validate_threshold(""))
  expect_error(validate_threshold("FLEXIBLE"))  # case sensitive
})

test_that("validate_threshold rejects symmetric for K < 3", {
  expect_error(validate_threshold("symmetric", K = 2))
  expect_true(validate_threshold("symmetric", K = 3))
  expect_true(validate_threshold("symmetric", K = 5))
})

test_that("validate_threshold warns for equidistant with K = 2", {
  expect_warning(validate_threshold("equidistant", K = 2))
  expect_true(suppressWarnings(validate_threshold("equidistant", K = 2)))
  expect_true(validate_threshold("equidistant", K = 3))
})

test_that("validate_threshold accepts flexible for any K >= 2", {
  expect_true(validate_threshold("flexible", K = 2))
  expect_true(validate_threshold("flexible", K = 3))
  expect_true(validate_threshold("flexible", K = 10))
})

test_that("get_threshold_params returns correct structure", {
  # flexible
  flex_params <- get_threshold_params("flexible")
  expect_equal(flex_params$params, "c")
  expect_equal(flex_params$stan_type, "ordered[K-1]")
  expect_equal(flex_params$n_params(5), 4)  # K=5 -> 4 thresholds

  # equidistant
  equi_params <- get_threshold_params("equidistant")
  expect_equal(equi_params$params, c("c1", "d"))
  expect_equal(equi_params$n_params(5), 2)  # Always 2 params

  # symmetric
  sym_params <- get_threshold_params("symmetric")
  expect_equal(sym_params$params, "c_pos")
  expect_equal(sym_params$n_params(4), 1)   # K=4 -> (K-1)/2 = 1
  expect_equal(sym_params$n_params(5), 2)   # K=5 -> (K-1)/2 = 2
  expect_equal(sym_params$n_params(6), 2)   # K=6 -> (K-1)/2 = 2
})

test_that("get_default_threshold_prior returns priors", {
  # flexible
  flex_prior <- get_default_threshold_prior("flexible")
  expect_true("c" %in% names(flex_prior))
  expect_equal(flex_prior$c$family, "normal")

  # equidistant
  equi_prior <- get_default_threshold_prior("equidistant")
  expect_true("c1" %in% names(equi_prior))
  expect_true("d" %in% names(equi_prior))
  expect_equal(equi_prior$c1$family, "normal")
  expect_equal(equi_prior$d$family, "gamma")

  # symmetric
  sym_prior <- get_default_threshold_prior("symmetric")
  expect_true("c_pos" %in% names(sym_prior))
  expect_equal(sym_prior$c_pos$family, "normal")
})

test_that("get_model_name returns correct model names", {
  # Base models (fixed link parameters)
  expect_equal(get_model_name("flexible", FALSE), "clm_base")
  expect_equal(get_model_name("equidistant", FALSE), "clm_equidistant")
  expect_equal(get_model_name("symmetric", FALSE), "clm_symmetric")

  # Full models (estimated link parameters)
  expect_equal(get_model_name("flexible", TRUE), "clm_full")
  expect_equal(get_model_name("equidistant", TRUE), "clm_equidistant_full")
  expect_equal(get_model_name("symmetric", TRUE), "clm_symmetric_full")
})

# Tests for prepare_data functions

test_that("prepare_stan_data_equidistant creates correct structure", {
  skip_if_not_installed("clmstan")

  # Create mock data
  data <- data.frame(
    y = factor(c(1, 2, 3, 2, 1, 3, 2, 3, 1, 2)),
    x = rnorm(10)
  )

  stan_data <- prepare_stan_data_equidistant(y ~ x, data)

  expect_equal(stan_data$K, 3)
  expect_equal(stan_data$N, 10)
  expect_equal(stan_data$P, 1)
  expect_true("prior_c1_mu" %in% names(stan_data))
  expect_true("prior_c1_sd" %in% names(stan_data))
  expect_true("prior_d_alpha" %in% names(stan_data))
  expect_true("prior_d_beta" %in% names(stan_data))
})

test_that("prepare_stan_data_symmetric creates correct structure", {
  skip_if_not_installed("clmstan")

  # Create mock data
  data <- data.frame(
    y = factor(c(1, 2, 3, 2, 1, 3, 2, 3, 1, 2)),
    x = rnorm(10)
  )

  stan_data <- prepare_stan_data_symmetric(y ~ x, data)

  expect_equal(stan_data$K, 3)
  expect_equal(stan_data$N, 10)
  expect_equal(stan_data$P, 1)
  expect_true("prior_cpos_sd" %in% names(stan_data))
})

test_that("prepare_stan_data_symmetric rejects K < 3", {
  skip_if_not_installed("clmstan")

  # Create mock data with only 2 categories
  data <- data.frame(
    y = factor(c(1, 2, 1, 2, 1, 2, 1, 2, 1, 2)),
    x = rnorm(10)
  )

  expect_error(prepare_stan_data_symmetric(y ~ x, data))
})
