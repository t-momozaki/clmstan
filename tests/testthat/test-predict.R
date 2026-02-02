# =============================================================================
# Tests for prediction methods in predict.R
# =============================================================================

# =============================================================================
# Helper function tests (no fit object required)
# =============================================================================

test_that("validate_prediction_input rejects non-clmstan objects", {
  expect_error(
    validate_prediction_input(list(class = "other"), NULL, NULL),
    "must be a clmstan object"
  )
})

test_that("validate_prediction_input rejects non-data.frame newdata", {
  mock_obj <- structure(
    list(
      formula = y ~ x,
      data = data.frame(y = factor(1:3), x = 1:3)
    ),
    class = "clmstan"
  )

  expect_error(
    validate_prediction_input(mock_obj, "not a data frame", NULL),
    "must be a data frame"
  )
})

test_that("validate_prediction_input detects missing variables", {
  mock_obj <- structure(
    list(
      formula = y ~ x1 + x2,
      data = data.frame(y = factor(1:3), x1 = 1:3, x2 = 4:6)
    ),
    class = "clmstan"
  )

  newdata <- data.frame(x1 = 1:2)  # Missing x2

  expect_error(
    validate_prediction_input(mock_obj, newdata, NULL),
    "Variables missing from newdata: x2"
  )
})

test_that("validate_prediction_input detects unknown factor levels", {
  mock_obj <- structure(
    list(
      formula = y ~ group,
      data = data.frame(
        y = factor(1:3),
        group = factor(c("A", "B", "A"), levels = c("A", "B"))
      )
    ),
    class = "clmstan"
  )

  newdata <- data.frame(group = factor(c("A", "C")))  # C is unknown

  expect_error(
    validate_prediction_input(mock_obj, newdata, NULL),
    "Unknown factor levels"
  )
})

test_that("validate_prediction_input accepts valid inputs", {
  mock_obj <- structure(
    list(
      formula = y ~ x,
      data = data.frame(y = factor(1:3), x = 1:3)
    ),
    class = "clmstan"
  )

  # NULL newdata should be valid
  expect_true(validate_prediction_input(mock_obj, NULL, NULL))

  # Valid newdata should be accepted
  newdata <- data.frame(x = 4:5)
  expect_true(validate_prediction_input(mock_obj, newdata, NULL))
})

test_that("validate_prediction_input rejects invalid ndraws", {
  mock_obj <- structure(
    list(
      formula = y ~ x,
      data = data.frame(y = factor(1:3), x = 1:3)
    ),
    class = "clmstan"
  )

  expect_error(validate_prediction_input(mock_obj, NULL, 0), "positive integer")
  expect_error(validate_prediction_input(mock_obj, NULL, -1), "positive integer")
  expect_error(validate_prediction_input(mock_obj, NULL, c(1, 2)), "positive integer")
})

# =============================================================================
# compute_probs_single tests
# =============================================================================

test_that("compute_probs_single returns valid probabilities", {
  # Simple case: K=3 categories, N=2 observations
  c_vec <- c(-1, 1)  # K-1 = 2 cutpoints
  eta_vec <- c(0, 0.5)  # N=2 observations
  K <- 3

  probs <- compute_probs_single(c_vec, eta_vec, "logit", list(), K)

  # Check dimensions
  expect_equal(dim(probs), c(2, 3))

  # Check probabilities sum to 1
  expect_equal(rowSums(probs), c(1, 1), tolerance = 1e-10)

  # Check all probabilities are in [0, 1]
  expect_true(all(probs >= 0))
  expect_true(all(probs <= 1))
})

test_that("compute_probs_single works with different links", {
  c_vec <- c(-1, 0, 1)  # K-1 = 3 cutpoints
  eta_vec <- c(0)  # N=1 observation
  K <- 4

  for (link in c("logit", "probit", "cloglog", "loglog", "cauchit")) {
    probs <- compute_probs_single(c_vec, eta_vec, link, list(), K)

    expect_equal(dim(probs), c(1, 4), info = link)
    expect_equal(sum(probs), 1, tolerance = 1e-10, info = link)
    expect_true(all(probs >= 0), info = link)
  }
})

test_that("compute_probs_single handles flexible links", {
  c_vec <- c(-1, 1)
  eta_vec <- c(0)
  K <- 3

  # tlink
  probs <- compute_probs_single(c_vec, eta_vec, "tlink", list(df = 8), K)
  expect_equal(sum(probs), 1, tolerance = 1e-10)

  # aranda_ordaz
  probs <- compute_probs_single(c_vec, eta_vec, "aranda_ordaz", list(lambda = 1), K)
  expect_equal(sum(probs), 1, tolerance = 1e-10)

  # sp
  probs <- compute_probs_single(c_vec, eta_vec, "sp",
                                 list(r = 1, base = "logit"), K)
  expect_equal(sum(probs), 1, tolerance = 1e-10)

  # log_gamma
  probs <- compute_probs_single(c_vec, eta_vec, "log_gamma", list(lambda = 0), K)
  expect_equal(sum(probs), 1, tolerance = 1e-10)

  # gev
  probs <- compute_probs_single(c_vec, eta_vec, "gev", list(xi = 0), K)
  expect_equal(sum(probs), 1, tolerance = 1e-10)

  # aep
  probs <- compute_probs_single(c_vec, eta_vec, "aep",
                                 list(theta1 = 2, theta2 = 2), K)
  expect_equal(sum(probs), 1, tolerance = 1e-10)
})

test_that("compute_probs_single gives sensible results for extreme eta", {
  c_vec <- c(-1, 0, 1)
  K <- 4

  # In CLM: P(Y <= k) = F(c_k - eta)
  # Very negative eta -> c_k - eta is large -> F() close to 1 -> high P(Y=1)
  probs_neg <- compute_probs_single(c_vec, c(-10), "logit", list(), K)
  expect_true(probs_neg[1, 1] > 0.9)

  # Very positive eta -> c_k - eta is small -> F() close to 0 -> high P(Y=K)
  probs_pos <- compute_probs_single(c_vec, c(10), "logit", list(), K)
  expect_true(probs_pos[1, 4] > 0.9)
})

# =============================================================================
# get_base_link_params tests
# =============================================================================

test_that("get_base_link_params extracts fixed parameters", {
  mock_obj <- list(
    link = "tlink",
    link_param = list(df = 8),
    base = NA
  )

  params <- get_base_link_params(mock_obj)
  expect_equal(params$df, 8)
})

test_that("get_base_link_params ignores estimated parameters", {
  mock_obj <- list(
    link = "gev",
    link_param = list(xi = "estimate"),
    base = NA
  )

  params <- get_base_link_params(mock_obj)
  expect_null(params$xi)
})

test_that("get_base_link_params handles SP link base", {
  mock_obj <- list(
    link = "sp",
    link_param = list(r = 1),
    base = "logit"
  )

  params <- get_base_link_params(mock_obj)
  expect_equal(params$r, 1)
  expect_equal(params$base, "logit")
})

test_that("get_base_link_params returns empty list for standard links", {
  mock_obj <- list(
    link = "logit",
    link_param = NULL,
    base = NA
  )

  params <- get_base_link_params(mock_obj)
  expect_equal(length(params), 0)
})

# =============================================================================
# summarize_class_draws tests
# =============================================================================

test_that("summarize_class_draws returns correct structure", {
  # S=100 draws, N=5 observations
  set.seed(123)
  class_draws <- matrix(sample(1:3, 500, replace = TRUE), nrow = 100, ncol = 5)

  result <- summarize_class_draws(class_draws)

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 5)
  expect_true("Estimate" %in% names(result))
  expect_true("Est.Error" %in% names(result))
  expect_true("Class" %in% names(result))
  expect_true("Q2.5" %in% names(result))
  expect_true("Q97.5" %in% names(result))
})

test_that("summarize_class_draws robust option uses median", {
  set.seed(123)
  class_draws <- matrix(sample(1:3, 500, replace = TRUE), nrow = 100, ncol = 5)

  result_mean <- summarize_class_draws(class_draws, robust = FALSE)
  result_median <- summarize_class_draws(class_draws, robust = TRUE)

  # Median and mean may differ
  expect_true(is.data.frame(result_median))
  expect_equal(nrow(result_median), 5)
})

test_that("summarize_class_draws modal class is most frequent", {
  # Create draws where class 2 is always most frequent
  class_draws <- matrix(c(
    rep(2, 60), rep(1, 20), rep(3, 20),  # Observation 1: mode = 2
    rep(1, 50), rep(2, 30), rep(3, 20)   # Observation 2: mode = 1
  ), nrow = 100, ncol = 2)

  result <- summarize_class_draws(class_draws)

  expect_equal(result$Class[1], 2)
  expect_equal(result$Class[2], 1)
})

# =============================================================================
# summarize_probs_draws tests
# =============================================================================

test_that("summarize_probs_draws returns correct structure", {
  # S=100 draws, N=5 observations, K=3 categories
  set.seed(123)
  probs_array <- array(runif(100 * 5 * 3), dim = c(100, 5, 3))
  # Normalize to valid probabilities
  for (s in 1:100) {
    for (n in 1:5) {
      probs_array[s, n, ] <- probs_array[s, n, ] / sum(probs_array[s, n, ])
    }
  }

  result <- summarize_probs_draws(probs_array)

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 5)
  expect_equal(ncol(result), 3)
  expect_true("P[Y=1]" %in% names(result))
  expect_true("P[Y=2]" %in% names(result))
  expect_true("P[Y=3]" %in% names(result))

  # Probabilities should sum to approximately 1
  row_sums <- result[["P[Y=1]"]] + result[["P[Y=2]"]] + result[["P[Y=3]"]]
  expect_equal(row_sums, rep(1, 5), tolerance = 0.01)
})

# =============================================================================
# prepare_prediction_matrix tests
# =============================================================================

test_that("prepare_prediction_matrix creates correct design matrix", {
  mock_obj <- list(
    formula = y ~ x1 + x2,
    data = data.frame(
      y = factor(1:5),
      x1 = c(1, 2, 3, 4, 5),
      x2 = c(5, 4, 3, 2, 1)
    )
  )

  newdata <- data.frame(x1 = c(0, 1), x2 = c(1, 0))

  X <- prepare_prediction_matrix(mock_obj, newdata)

  expect_equal(nrow(X), 2)
  expect_equal(ncol(X), 2)
  expect_true("x1" %in% colnames(X))
  expect_true("x2" %in% colnames(X))
  expect_false("(Intercept)" %in% colnames(X))
})

test_that("prepare_prediction_matrix handles factors correctly", {
  mock_obj <- list(
    formula = y ~ group,
    data = data.frame(
      y = factor(1:4),
      group = factor(c("A", "B", "A", "B"))
    )
  )

  newdata <- data.frame(group = factor(c("A", "B"), levels = c("A", "B")))

  X <- prepare_prediction_matrix(mock_obj, newdata)

  expect_equal(nrow(X), 2)
  # Factor with 2 levels creates 1 dummy variable
  expect_equal(ncol(X), 1)
})

test_that("prepare_prediction_matrix handles intercept-only model", {
  mock_obj <- list(
    formula = y ~ 1,
    data = data.frame(y = factor(1:3))
  )

  newdata <- data.frame(dummy = 1:2)

  X <- prepare_prediction_matrix(mock_obj, newdata)

  expect_equal(nrow(X), 2)
  expect_equal(ncol(X), 0)  # No predictors
})

# =============================================================================
# Integration tests with mock fit objects
# =============================================================================

# Here we test the R-side logic with mock objects

test_that("predict.clmstan type argument validation works", {
  mock_obj <- structure(
    list(
      formula = y ~ x,
      data = data.frame(y = factor(1:3), x = 1:3)
    ),
    class = "clmstan"
  )

  # Invalid type should error
  expect_error(
    predict.clmstan(mock_obj, type = "invalid"),
    "'arg' should be one of"
  )
})
