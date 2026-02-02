# =============================================================================
# Tests for loo.clmstan() and waic.clmstan() methods
# =============================================================================

# =============================================================================
# Helper function to create mock clmstan objects
# =============================================================================

# Create a mock clmstan object for testing loo/waic
# Args: N = observations, S = total draws, chains = chain count, seed = RNG seed
create_mock_clmstan_for_loo <- function(N = 20, S = 400, chains = 4, seed = 42) {
  iter_sampling <- S / chains

  # Generate fake log_lik values (S x N matrix)
  set.seed(seed)
  log_lik <- matrix(rnorm(S * N, mean = -2, sd = 0.5), nrow = S, ncol = N)
  colnames(log_lik) <- paste0("log_lik[", 1:N, "]")

  mock_fit <- list(
    draws = function(variables, format = "matrix") {
      if ("log_lik" %in% variables || variables == "log_lik") {
        return(log_lik)
      }
      stop("Unknown variable: ", variables)
    },
    num_chains = function() chains,
    metadata = function() list(iter_sampling = iter_sampling)
  )

  structure(
    list(
      fit = mock_fit,
      formula = y ~ x,
      N = N,
      K = 3,
      P = 1
    ),
    class = "clmstan"
  )
}

# =============================================================================
# Tests for extract_log_lik()
# =============================================================================

test_that("extract_log_lik validates missing fit object", {
  mock_obj <- list(fit = NULL, N = 10)
  class(mock_obj) <- "clmstan"

  expect_error(
    extract_log_lik(mock_obj),
    "No Stan fit object found"
  )
})

test_that("extract_log_lik validates dimensions", {
  # Create mock fit with mismatched dimensions
  mock_fit <- list(
    draws = function(variables, format = "matrix") {
      matrix(rnorm(100 * 5), nrow = 100, ncol = 5)  # 5 observations
    }
  )
  mock_obj <- list(fit = mock_fit, N = 10)  # But object says 10
  class(mock_obj) <- "clmstan"

  expect_error(
    extract_log_lik(mock_obj),
    "log_lik dimension mismatch"
  )
})

test_that("extract_log_lik returns correct matrix", {
  mock_obj <- create_mock_clmstan_for_loo(N = 15, S = 200)
  log_lik <- extract_log_lik(mock_obj)

  expect_true(is.matrix(log_lik))
  expect_equal(nrow(log_lik), 200)
  expect_equal(ncol(log_lik), 15)
})

# =============================================================================
# Tests for compute_r_eff()
# =============================================================================

test_that("compute_r_eff handles chain structure correctly", {
  skip_if_not_installed("loo")

  mock_obj <- create_mock_clmstan_for_loo(N = 10, S = 400, chains = 4)
  log_lik <- extract_log_lik(mock_obj)

  r_eff <- compute_r_eff(mock_obj, log_lik, cores = 1)

  # r_eff should be a vector of length N
  expect_equal(length(r_eff), 10)

  # All values should be positive
  expect_true(all(r_eff > 0))
})

test_that("compute_r_eff warns when chain structure is ambiguous", {
  skip_if_not_installed("loo")

  # Create object where S is not divisible by chains
  log_lik_bad <- matrix(rnorm(101 * 10), nrow = 101, ncol = 10)

  mock_fit <- list(
    draws = function(variables, format = "matrix") log_lik_bad,
    num_chains = function() 4,
    metadata = function() list(iter_sampling = 25)  # 4*25=100, not 101
  )

  mock_obj <- structure(
    list(fit = mock_fit, N = 10),
    class = "clmstan"
  )

  expect_warning(
    r_eff <- compute_r_eff(mock_obj, log_lik_bad),
    "Cannot determine chain structure"
  )

  # Should fall back to r_eff = 1
  expect_equal(r_eff, rep(1, 10))
})

# =============================================================================
# Tests for loo.clmstan()
# =============================================================================

test_that("loo.clmstan returns loo object with correct structure", {
  skip_if_not_installed("loo")

  mock_obj <- create_mock_clmstan_for_loo(N = 20)

  # Use r_eff = NA to skip computation (faster test)
  result <- loo(mock_obj, r_eff = NA)

  # Check class
  expect_s3_class(result, "loo")
  expect_s3_class(result, "psis_loo")

  # Check required components
  expect_true("estimates" %in% names(result))
  expect_true("pointwise" %in% names(result))
  expect_true("diagnostics" %in% names(result))

  # Check estimates structure
  expect_true(all(c("elpd_loo", "p_loo", "looic") %in% rownames(result$estimates)))
  expect_equal(ncol(result$estimates), 2)  # Estimate and SE

  # Check pointwise has correct number of observations
  expect_equal(nrow(result$pointwise), 20)

  # Check diagnostics
  expect_true("pareto_k" %in% names(result$diagnostics))
  expect_equal(length(result$diagnostics$pareto_k), 20)
})

test_that("loo.clmstan computes r_eff when not provided", {
  skip_if_not_installed("loo")

  mock_obj <- create_mock_clmstan_for_loo(N = 10, S = 200, chains = 4)

  # Capture message about r_eff computation
  expect_message(
    result <- loo(mock_obj, r_eff = NULL),
    "Computing r_eff"
  )

  expect_s3_class(result, "loo")
})

test_that("loo.clmstan accepts user-provided r_eff", {
  skip_if_not_installed("loo")

  mock_obj <- create_mock_clmstan_for_loo(N = 10)

  # Provide custom r_eff
  custom_r_eff <- rep(0.8, 10)
  result <- loo(mock_obj, r_eff = custom_r_eff)

  expect_s3_class(result, "loo")
})

test_that("loo.clmstan validates input class", {
  skip_if_not_installed("loo")

  expect_error(
    loo.clmstan(list(a = 1)),
    "'x' must be a clmstan object"
  )
})

test_that("loo.clmstan adds model attributes", {
  skip_if_not_installed("loo")

  mock_obj <- create_mock_clmstan_for_loo(N = 15)
  result <- loo(mock_obj, r_eff = NA)

  expect_equal(attr(result, "nobs"), 15)
  expect_true(!is.null(attr(result, "model_name")))
})

# =============================================================================
# Tests for waic.clmstan()
# =============================================================================

test_that("waic.clmstan returns waic object with correct structure", {
  skip_if_not_installed("loo")

  mock_obj <- create_mock_clmstan_for_loo(N = 20)
  result <- waic(mock_obj)

  # Check class
  expect_s3_class(result, "loo")
  expect_s3_class(result, "waic")

  # Check required components
  expect_true("estimates" %in% names(result))
  expect_true("pointwise" %in% names(result))

  # Check estimates structure
  expect_true(all(c("elpd_waic", "p_waic", "waic") %in% rownames(result$estimates)))

  # Check pointwise has correct number of observations
  expect_equal(nrow(result$pointwise), 20)
})

test_that("waic.clmstan validates input class", {
  skip_if_not_installed("loo")

  expect_error(
    waic.clmstan(list(a = 1)),
    "'x' must be a clmstan object"
  )
})

test_that("waic.clmstan adds model attributes", {
  skip_if_not_installed("loo")

  mock_obj <- create_mock_clmstan_for_loo(N = 15)
  result <- waic(mock_obj)

  expect_equal(attr(result, "nobs"), 15)
})

# =============================================================================
# Integration tests: loo_compare compatibility
# =============================================================================

test_that("loo results can be used with loo_compare", {
  skip_if_not_installed("loo")

  mock_obj1 <- create_mock_clmstan_for_loo(N = 20, seed = 42)
  mock_obj2 <- create_mock_clmstan_for_loo(N = 20, seed = 123)

  loo1 <- loo(mock_obj1, r_eff = NA)
  loo2 <- loo(mock_obj2, r_eff = NA)

  # loo_compare should work without error
  comparison <- loo::loo_compare(loo1, loo2)

  expect_true(is.matrix(comparison))
  expect_equal(nrow(comparison), 2)
})

test_that("waic results can be used with loo_compare", {
  skip_if_not_installed("loo")

  mock_obj1 <- create_mock_clmstan_for_loo(N = 20, seed = 42)
  mock_obj2 <- create_mock_clmstan_for_loo(N = 20, seed = 123)

  waic1 <- waic(mock_obj1)
  waic2 <- waic(mock_obj2)

  # loo_compare should work with waic objects
  comparison <- loo::loo_compare(waic1, waic2)

  expect_true(is.matrix(comparison))
  expect_equal(nrow(comparison), 2)
})
