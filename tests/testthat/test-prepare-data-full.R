# =============================================================================
# Tests for full model data preparation functions
# =============================================================================

# Helper function to create test data
create_test_data <- function(n = 100, K = 4) {
  set.seed(42)
  data.frame(
    y = factor(sample(1:K, n, replace = TRUE)),
    x1 = rnorm(n),
    x2 = rnorm(n)
  )
}

# =============================================================================
# Tests for needs_full_model()
# =============================================================================

test_that("needs_full_model returns FALSE for NULL", {
  expect_false(needs_full_model(NULL))
})

test_that("needs_full_model returns FALSE for all fixed values", {
  expect_false(needs_full_model(list(theta1 = 2, theta2 = 2)))
  expect_false(needs_full_model(list(df = 8)))
  expect_false(needs_full_model(list(r = 1, base = "logit")))
  expect_false(needs_full_model(list(xi = 0)))
})

test_that("needs_full_model returns TRUE when any parameter is 'estimate'", {
  expect_true(needs_full_model(list(theta1 = "estimate")))
  expect_true(needs_full_model(list(theta1 = "estimate", theta2 = 2)))
  expect_true(needs_full_model(list(df = "estimate")))
  expect_true(needs_full_model(list(r = "estimate", base = "logit")))
  expect_true(needs_full_model(list(xi = "estimate")))
})

test_that("needs_full_model handles mixed fixed and estimate", {
  expect_true(needs_full_model(list(theta1 = 2, theta2 = "estimate")))
  expect_true(needs_full_model(list(r = 1, base = "probit", df = "estimate")))
})

# =============================================================================
# Tests for prepare_link_params_full()
# =============================================================================

test_that("prepare_link_params_full returns default values when link_param is NULL", {
  result <- prepare_link_params_full("aep", NULL, NULL)

  # All estimate flags should be 0
  expect_equal(result$estimate_df, 0L)
  expect_equal(result$estimate_lambda, 0L)
  expect_equal(result$estimate_xi, 0L)
  expect_equal(result$estimate_r, 0L)
  expect_equal(result$estimate_theta1, 0L)
  expect_equal(result$estimate_theta2, 0L)

  # Fixed values should be defaults
  expect_equal(result$df_fixed, 8)
  expect_equal(result$lambda_fixed, 1)
  expect_equal(result$xi_fixed, 0)
  expect_equal(result$r_fixed, 1)
  expect_equal(result$theta1_fixed, 2)
  expect_equal(result$theta2_fixed, 2)
})

test_that("prepare_link_params_full sets estimation flag for 'estimate' parameters", {
  result <- prepare_link_params_full("aep", list(theta1 = "estimate"), NULL)
  expect_equal(result$estimate_theta1, 1L)
  expect_equal(result$estimate_theta2, 0L)

  result2 <- prepare_link_params_full("aep", list(theta1 = "estimate", theta2 = "estimate"), NULL)
  expect_equal(result2$estimate_theta1, 1L)
  expect_equal(result2$estimate_theta2, 1L)
})

test_that("prepare_link_params_full sets fixed values for numeric parameters", {
  result <- prepare_link_params_full("aep", list(theta1 = 3, theta2 = 4), NULL)
  expect_equal(result$estimate_theta1, 0L)
  expect_equal(result$estimate_theta2, 0L)
  expect_equal(result$theta1_fixed, 3)
  expect_equal(result$theta2_fixed, 4)
})

test_that("prepare_link_params_full handles SP link base parameter", {
  result <- prepare_link_params_full("sp", list(r = "estimate", base = "probit"), NULL)
  expect_equal(result$estimate_r, 1L)
  expect_equal(result$base_type, 2L)  # probit = 2
})

test_that("prepare_link_params_full applies custom priors", {
  link_prior <- list(theta1 = list(alpha = 3, beta = 0.5))
  result <- prepare_link_params_full("aep", list(theta1 = "estimate"), link_prior)

  expect_equal(result$prior_theta1_alpha, 3)
  expect_equal(result$prior_theta1_beta, 0.5)
  # theta2 should have defaults
  expect_equal(result$prior_theta2_alpha, 2)
  expect_equal(result$prior_theta2_beta, 1)
})

test_that("prepare_link_params_full has correct default priors", {
  result <- prepare_link_params_full("aep", NULL, NULL)

  # Check default prior hyperparameters
  expect_equal(result$prior_df_alpha, 2)
  expect_equal(result$prior_df_beta, 0.1)
  expect_equal(result$prior_lambda_ao_alpha, 0.5)
  expect_equal(result$prior_lambda_ao_beta, 0.5)
  expect_equal(result$prior_lambda_lg_mu, 0)
  expect_equal(result$prior_lambda_lg_sd, 1)
  expect_equal(result$prior_xi_mu, 0)
  expect_equal(result$prior_xi_sd, 2)
  expect_equal(result$prior_r_alpha, 0.5)
  expect_equal(result$prior_r_beta, 0.5)
  expect_equal(result$prior_theta1_alpha, 2)
  expect_equal(result$prior_theta1_beta, 1)
  expect_equal(result$prior_theta2_alpha, 2)
  expect_equal(result$prior_theta2_beta, 1)
})

# =============================================================================
# Tests for validate_full_params()
# =============================================================================

# Helper to create a complete params list for validate_full_params
create_full_params <- function(
    estimate_df = 0L, estimate_lambda = 0L, estimate_xi = 0L,
    estimate_r = 0L, estimate_theta1 = 0L, estimate_theta2 = 0L,
    base_type = 1L
) {
  list(
    estimate_df = estimate_df,
    estimate_lambda = estimate_lambda,
    estimate_xi = estimate_xi,
    estimate_r = estimate_r,
    estimate_theta1 = estimate_theta1,
    estimate_theta2 = estimate_theta2,
    base_type = base_type
  )
}

test_that("validate_full_params warns for irrelevant df estimation", {
  params <- create_full_params(estimate_df = 1L)
  expect_warning(validate_full_params("logit", params), "does not use df")
  expect_warning(validate_full_params("aep", params), "does not use df")
})

test_that("validate_full_params does not warn for valid df estimation", {
  params <- create_full_params(estimate_df = 1L)
  expect_silent(validate_full_params("tlink", params))

  # SP with tlink base
  params_sp <- create_full_params(estimate_df = 1L, base_type = 6L)
  expect_silent(validate_full_params("sp", params_sp))
})

test_that("validate_full_params warns for irrelevant lambda estimation", {
  params <- create_full_params(estimate_lambda = 1L)
  expect_warning(validate_full_params("logit", params), "does not use lambda")
  expect_warning(validate_full_params("aep", params), "does not use lambda")
})

test_that("validate_full_params does not warn for valid lambda estimation", {
  params <- create_full_params(estimate_lambda = 1L)
  expect_silent(validate_full_params("aranda_ordaz", params))
  expect_silent(validate_full_params("log_gamma", params))
})

test_that("validate_full_params warns for irrelevant xi estimation", {
  params <- create_full_params(estimate_xi = 1L)
  expect_warning(validate_full_params("logit", params), "does not use xi")
  expect_warning(validate_full_params("sp", params), "does not use xi")
})

test_that("validate_full_params does not warn for valid xi estimation", {
  params <- create_full_params(estimate_xi = 1L)
  expect_silent(validate_full_params("gev", params))
})

test_that("validate_full_params warns for irrelevant r estimation", {
  params <- create_full_params(estimate_r = 1L)
  expect_warning(validate_full_params("logit", params), "does not use r")
  expect_warning(validate_full_params("aep", params), "does not use r")
})

test_that("validate_full_params does not warn for valid r estimation", {
  params <- create_full_params(estimate_r = 1L)
  expect_silent(validate_full_params("sp", params))
})

test_that("validate_full_params warns for irrelevant theta estimation", {
  params <- create_full_params(estimate_theta1 = 1L)
  expect_warning(validate_full_params("logit", params), "does not use theta")
  expect_warning(validate_full_params("sp", params), "does not use theta")
})

test_that("validate_full_params does not warn for valid theta estimation", {
  params <- create_full_params(estimate_theta1 = 1L, estimate_theta2 = 1L)
  expect_silent(validate_full_params("aep", params))
})

# =============================================================================
# Tests for prepare_stan_data_full()
# =============================================================================

test_that("prepare_stan_data_full returns correct structure", {
  data <- create_test_data()
  result <- prepare_stan_data_full(y ~ x1 + x2, data, link = "aep",
                                    link_param = list(theta1 = "estimate", theta2 = 2))

  # Basic data fields
  expect_true("K" %in% names(result))
  expect_true("N" %in% names(result))
  expect_true("P" %in% names(result))
  expect_true("y" %in% names(result))
  expect_true("X" %in% names(result))
  expect_true("link_type" %in% names(result))

  # Estimation flags
  expect_true("estimate_df" %in% names(result))
  expect_true("estimate_theta1" %in% names(result))
  expect_true("estimate_theta2" %in% names(result))

  # Fixed values
  expect_true("df_fixed" %in% names(result))
  expect_true("theta1_fixed" %in% names(result))
  expect_true("theta2_fixed" %in% names(result))

  # Prior hyperparameters
  expect_true("prior_beta_sd" %in% names(result))
  expect_true("prior_c_sd" %in% names(result))
  expect_true("prior_theta1_alpha" %in% names(result))
})

test_that("prepare_stan_data_full sets correct estimation flags", {
  data <- create_test_data()

  # AEP with theta1 estimated, theta2 fixed
  result <- prepare_stan_data_full(y ~ x1 + x2, data, link = "aep",
                                    link_param = list(theta1 = "estimate", theta2 = 3))

  expect_equal(result$estimate_theta1, 1L)
  expect_equal(result$estimate_theta2, 0L)
  expect_equal(result$theta2_fixed, 3)
  expect_equal(result$link_type, 11L)  # aep = 11
})

test_that("prepare_stan_data_full handles GEV link", {
  data <- create_test_data()

  result <- prepare_stan_data_full(y ~ x1, data, link = "gev",
                                    link_param = list(xi = "estimate"))

  expect_equal(result$estimate_xi, 1L)
  expect_equal(result$link_type, 10L)  # gev = 10
})

test_that("prepare_stan_data_full handles SP link with base", {
  data <- create_test_data()

  result <- prepare_stan_data_full(y ~ x1, data, link = "sp",
                                    link_param = list(r = "estimate", base = "probit"))

  expect_equal(result$estimate_r, 1L)
  expect_equal(result$base_type, 2L)  # probit = 2
  expect_equal(result$link_type, 8L)  # sp = 8
})

test_that("prepare_stan_data_full applies custom priors", {
  data <- create_test_data()

  result <- prepare_stan_data_full(y ~ x1, data, link = "aep",
                                    link_param = list(theta1 = "estimate"),
                                    link_prior = list(theta1 = list(alpha = 5, beta = 2)))

  expect_equal(result$prior_theta1_alpha, 5)
  expect_equal(result$prior_theta1_beta, 2)
})

test_that("prepare_stan_data_full rejects K < 2", {
  data <- data.frame(y = factor(rep(1, 10)), x1 = rnorm(10))
  expect_error(prepare_stan_data_full(y ~ x1, data), "at least 2 categories")
})

test_that("prepare_stan_data_full handles intercept-only model", {
  data <- create_test_data()
  result <- prepare_stan_data_full(y ~ 1, data, link = "logit")

  expect_equal(result$P, 0)
  expect_equal(ncol(result$X), 0)
})
