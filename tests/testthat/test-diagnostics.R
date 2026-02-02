# =============================================================================
# Tests for convergence diagnostics
# =============================================================================
#
# Terminology:
#   ESS (Effective Sample Size): Number of independent samples after accounting
#       for autocorrelation. ESS_bulk for distribution center, ESS_tail for tails.
#   Rhat: Convergence diagnostic comparing within-chain and between-chain variance.
#       Values < 1.01 indicate good convergence.
#   E-BFMI (Energy Bayesian Fraction of Missing Information): HMC-specific
#       diagnostic. Values < 0.3 suggest sampling problems.
#
# =============================================================================

# =============================================================================
# Helper function to create mock clmstan objects for diagnostics testing
# =============================================================================

# Create a mock clmstan object for testing diagnostics
create_mock_clmstan_for_diagnostics <- function(
    P = 2, K = 4, S = 400, chains = 4, seed = 42,
    add_divergences = 0, add_max_treedepth = 0, low_ebfmi = NULL) {

  iter_sampling <- S / chains
  set.seed(seed)

  # Generate fake parameter draws
  n_beta <- P
  n_c <- K - 1  # c_transformed has K-1 elements

  # Create draws data
  beta_draws <- matrix(rnorm(S * n_beta), nrow = S, ncol = n_beta)
  colnames(beta_draws) <- paste0("beta[", 1:n_beta, "]")

  c_draws <- matrix(rnorm(S * n_c), nrow = S, ncol = n_c)
  colnames(c_draws) <- paste0("c_transformed[", 1:n_c, "]")

  beta0_draws <- matrix(rnorm(S), nrow = S, ncol = 1)
  colnames(beta0_draws) <- "beta0"

  all_draws <- cbind(beta_draws, c_draws, beta0_draws)

  # Create draws_df format using posterior package
  draws_df <- as.data.frame(all_draws)
  draws_df$.chain <- rep(1:chains, each = iter_sampling)
  draws_df$.iteration <- rep(1:iter_sampling, chains)
  draws_df$.draw <- 1:S
  draws_df <- posterior::as_draws_df(draws_df)

  # Create draws_array format using posterior package
  draws_array <- posterior::as_draws_array(draws_df)

  # Create E-BFMI values
  ebfmi_values <- rep(0.8, chains)  # Good values by default

  if (!is.null(low_ebfmi)) {
    ebfmi_values[low_ebfmi] <- 0.2  # Low value
  }

  mock_fit <- list(
    draws = function(variables, format = "matrix") {
      # Filter to requested variables
      if (is.null(variables)) {
        requested <- colnames(all_draws)
      } else {
        # Handle both exact and prefix matching
        requested <- c()
        for (v in variables) {
          if (v %in% colnames(all_draws)) {
            requested <- c(requested, v)
          } else {
            # Prefix match (e.g., "beta" matches "beta[1]", "beta[2]")
            matches <- grep(paste0("^", v, "(\\[|$)"), colnames(all_draws),
                            value = TRUE)
            requested <- c(requested, matches)
          }
        }
      }

      if (format == "draws_df") {
        # Use posterior::subset_draws to maintain proper draws format
        result <- posterior::subset_draws(draws_df, variable = requested)
        return(result)
      } else if (format == "draws_array") {
        result <- posterior::subset_draws(draws_array, variable = requested)
        return(result)
      } else {
        return(all_draws[, requested, drop = FALSE])
      }
    },
    num_chains = function() chains,
    metadata = function() list(iter_sampling = iter_sampling),
    diagnostic_summary = function() {
      list(
        num_divergent = rep(add_divergences / chains, chains),
        num_max_treedepth = rep(add_max_treedepth / chains, chains),
        ebfmi = ebfmi_values
      )
    }
  )

  structure(
    list(
      fit = mock_fit,
      formula = y ~ x1 + x2,
      data = data.frame(
        y = factor(1:K),
        x1 = rnorm(K),
        x2 = rnorm(K)
      ),
      N = 100,
      K = K,
      P = P,
      threshold = "flexible"
    ),
    class = "clmstan"
  )
}

# =============================================================================
# Tests for summary() with ess_tail
# =============================================================================

test_that("extract_posterior_summary includes ess_tail column", {
  mock_obj <- create_mock_clmstan_for_diagnostics()

  # Call extract_posterior_summary directly
  summary_df <- extract_posterior_summary(mock_obj$fit, "beta", c(0.025, 0.5, 0.975))

  expect_true("ess_tail" %in% names(summary_df))
  expect_true("ess_bulk" %in% names(summary_df))
  expect_true(all(summary_df$ess_tail > 0))
})

# =============================================================================
# Tests for diagnostics()
# =============================================================================

test_that("diagnostics generic dispatches correctly", {
  mock_obj <- create_mock_clmstan_for_diagnostics()

  # Should work with both diagnostics() and diagnostics.clmstan()
  expect_no_error(diagnostics(mock_obj))
})

test_that("diagnostics validates input class", {
  expect_error(
    diagnostics.clmstan(list(a = 1)),
    "'object' must be a clmstan object"
  )
})

test_that("diagnostics returns correct structure", {
  mock_obj <- create_mock_clmstan_for_diagnostics()

  result <- diagnostics(mock_obj)

  expect_type(result, "list")
  expect_true("hmc" %in% names(result))
  expect_true("convergence" %in% names(result))
  expect_true("issues" %in% names(result))
})

test_that("diagnostics detects no issues when all OK", {
  # Use larger S to ensure ESS is above threshold
  mock_obj <- create_mock_clmstan_for_diagnostics(
    S = 2000, chains = 4,
    add_divergences = 0,
    add_max_treedepth = 0
  )

  # Use lower ESS threshold since mock data has limited ESS
  result <- suppressMessages(diagnostics(mock_obj, ess_threshold = 100))

  expect_false(result$issues)
})

test_that("diagnostics detects divergences", {
  mock_obj <- create_mock_clmstan_for_diagnostics(add_divergences = 10)

  result <- suppressMessages(diagnostics(mock_obj))

  expect_true(result$issues)
  expect_equal(sum(result$hmc$num_divergent), 10)
})

test_that("diagnostics detects max treedepth issues", {
  mock_obj <- create_mock_clmstan_for_diagnostics(add_max_treedepth = 5)

  result <- suppressMessages(diagnostics(mock_obj))

  expect_true(result$issues)
  expect_equal(sum(result$hmc$num_max_treedepth), 5)
})

test_that("diagnostics detects low E-BFMI", {
  mock_obj <- create_mock_clmstan_for_diagnostics(low_ebfmi = c(1, 2))

  result <- suppressMessages(diagnostics(mock_obj))

  expect_true(result$issues)
})

test_that("diagnostics detail mode shows all parameters", {
  mock_obj <- create_mock_clmstan_for_diagnostics()

  # Capture output with detail = TRUE
  output <- capture.output(diagnostics(mock_obj, detail = TRUE))

  expect_true(any(grepl("Full parameter diagnostics", output)))
})

test_that("diagnostics convergence summary has correct columns", {
  mock_obj <- create_mock_clmstan_for_diagnostics()

  result <- suppressMessages(diagnostics(mock_obj))

  expect_true("rhat" %in% names(result$convergence))
  expect_true("ess_bulk" %in% names(result$convergence))
  expect_true("ess_tail" %in% names(result$convergence))
})

# =============================================================================
# Tests for plot() with ACF type
# =============================================================================

test_that("plot accepts acf type", {
  skip_if_not_installed("bayesplot")

  mock_obj <- create_mock_clmstan_for_diagnostics()

  # Should not error
  expect_no_error(plot(mock_obj, type = "acf"))
})

test_that("plot type argument validates correctly", {
  mock_obj <- create_mock_clmstan_for_diagnostics()

  expect_error(
    plot(mock_obj, type = "invalid_type"),
    "'arg' should be one of"
  )
})

# =============================================================================
# Tests for extract_acf()
# =============================================================================

test_that("extract_acf validates input class", {
  expect_error(
    extract_acf(list(a = 1)),
    "'object' must be a clmstan object"
  )
})

test_that("extract_acf returns correct structure", {
  mock_obj <- create_mock_clmstan_for_diagnostics()

  result <- extract_acf(mock_obj, lags = 10)

  expect_true(is.data.frame(result))
  expect_equal(names(result), c("parameter", "chain", "lag", "acf"))
})

test_that("extract_acf returns correct number of rows", {
  mock_obj <- create_mock_clmstan_for_diagnostics(P = 2, K = 4, chains = 4)

  lags <- 10
  result <- extract_acf(mock_obj, lags = lags)

  # 5 params: 2 beta + 2 c_transformed + 1 beta0
  n_params <- 5
  n_chains <- 4
  n_lags <- lags + 1  # 0 to lags inclusive

  expected_rows <- n_params * n_chains * n_lags
  expect_equal(nrow(result), expected_rows)
})

test_that("extract_acf lag 0 has ACF = 1", {
  mock_obj <- create_mock_clmstan_for_diagnostics()

  result <- extract_acf(mock_obj, lags = 5)

  lag0 <- result[result$lag == 0, ]
  expect_true(all(abs(lag0$acf - 1) < 1e-10))
})

test_that("extract_acf respects lags parameter", {
  mock_obj <- create_mock_clmstan_for_diagnostics()

  result5 <- extract_acf(mock_obj, lags = 5)
  result10 <- extract_acf(mock_obj, lags = 10)

  expect_equal(max(result5$lag), 5)
  expect_equal(max(result10$lag), 10)
})

test_that("extract_acf returns values in valid range", {
  mock_obj <- create_mock_clmstan_for_diagnostics()

  result <- extract_acf(mock_obj, lags = 20)

  # ACF values should be between -1 and 1
  expect_true(all(result$acf >= -1 & result$acf <= 1))
})
