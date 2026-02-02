# =============================================================================
# Tests for S3 methods in methods.R
# =============================================================================

# =============================================================================
# Helper function tests (no fit object required)
# =============================================================================

test_that("get_threshold_names generates correct names for numeric levels", {
  # Create mock clmstan object
  mock_obj <- list(
    formula = y ~ x,
    data = data.frame(
      y = factor(c(1, 2, 3, 4, 5)),
      x = 1:5
    )
  )

  names <- get_threshold_names(mock_obj)

  expect_equal(length(names), 4)  # K-1 = 5-1 = 4

expect_equal(names[1], "1|2")
  expect_equal(names[2], "2|3")
  expect_equal(names[3], "3|4")
  expect_equal(names[4], "4|5")
})

test_that("get_threshold_names generates correct names for character levels", {
  mock_obj <- list(
    formula = rating ~ x,
    data = data.frame(
      rating = factor(c("poor", "fair", "good", "excellent"),
                      levels = c("poor", "fair", "good", "excellent")),
      x = 1:4
    )
  )

  names <- get_threshold_names(mock_obj)

  expect_equal(length(names), 3)
  expect_equal(names[1], "poor|fair")
  expect_equal(names[2], "fair|good")
  expect_equal(names[3], "good|excellent")
})

test_that("get_threshold_names handles 2-category response", {
  mock_obj <- list(
    formula = y ~ x,
    data = data.frame(
      y = factor(c(0, 1)),
      x = 1:2
    )
  )

  names <- get_threshold_names(mock_obj)

  expect_equal(length(names), 1)
  expect_equal(names[1], "0|1")
})

test_that("get_threshold_names converts non-factor to factor", {
  mock_obj <- list(
    formula = y ~ x,
    data = data.frame(
      y = c(1, 2, 3),  # Not a factor
      x = 1:3
    )
  )

  names <- get_threshold_names(mock_obj)

  expect_equal(length(names), 2)
  expect_equal(names[1], "1|2")
  expect_equal(names[2], "2|3")
})

test_that("get_coef_names returns correct names for single predictor", {
  mock_obj <- list(
    formula = y ~ x,
    data = data.frame(
      y = factor(c(1, 2, 3)),
      x = c(1.0, 2.0, 3.0)
    )
  )

  names <- get_coef_names(mock_obj)

  expect_equal(length(names), 1)
  expect_equal(names[1], "x")
})

test_that("get_coef_names returns correct names for multiple predictors", {
  mock_obj <- list(
    formula = y ~ x1 + x2 + x3,
    data = data.frame(
      y = factor(c(1, 2, 3)),
      x1 = c(1.0, 2.0, 3.0),
      x2 = c(4.0, 5.0, 6.0),
      x3 = c(7.0, 8.0, 9.0)
    )
  )

  names <- get_coef_names(mock_obj)

  expect_equal(length(names), 3)
  expect_equal(names, c("x1", "x2", "x3"))
})

test_that("get_coef_names handles factor predictors correctly", {
  mock_obj <- list(
    formula = y ~ temp + contact,
    data = data.frame(
      y = factor(c(1, 2, 3, 1, 2, 3)),
      temp = factor(c("cold", "warm", "cold", "warm", "cold", "warm")),
      contact = factor(c("no", "no", "yes", "yes", "no", "yes"))
    )
  )

  names <- get_coef_names(mock_obj)

  # Factor variables get dummy coded, so we expect "tempwarm" and "contactyes"
  expect_true("tempwarm" %in% names)
  expect_true("contactyes" %in% names)
})

test_that("get_coef_names returns empty for intercept-only model", {
  mock_obj <- list(
    formula = y ~ 1,
    data = data.frame(
      y = factor(c(1, 2, 3))
    )
  )

  names <- get_coef_names(mock_obj)

  expect_equal(length(names), 0)
})

# =============================================================================
# Tests requiring mock fit object
# =============================================================================

# Helper to create a minimal mock fit object
create_mock_fit <- function(beta_values, c_transformed_values) {
  # Create a mock that returns draws in matrix format
  list(
    draws = function(variables, format = "matrix") {
      if (format == "matrix") {
        if ("c_transformed" %in% variables || variables == "c_transformed") {
          n_samples <- 100
          K_minus_1 <- length(c_transformed_values)
          mat <- matrix(
            rep(c_transformed_values, each = n_samples) + rnorm(n_samples * K_minus_1, 0, 0.01),
            nrow = n_samples,
            ncol = K_minus_1
          )
          colnames(mat) <- paste0("c_transformed[", 1:K_minus_1, "]")
          return(mat)
        } else if ("beta" %in% variables || variables == "beta") {
          n_samples <- 100
          P <- length(beta_values)
          mat <- matrix(
            rep(beta_values, each = n_samples) + rnorm(n_samples * P, 0, 0.01),
            nrow = n_samples,
            ncol = P
          )
          colnames(mat) <- paste0("beta[", 1:P, "]")
          return(mat)
        }
      }
      stop("Unexpected call to draws()")
    }
  )
}

test_that("coef.clmstan returns named vector with thresholds and coefficients", {
  # Create mock clmstan object
  mock_fit <- create_mock_fit(
    beta_values = c(2.5, 1.5),
    c_transformed_values = c(0, 1.2, 3.1)
  )

  mock_obj <- structure(
    list(
      fit = mock_fit,
      formula = y ~ x1 + x2,
      data = data.frame(
        y = factor(c(1, 2, 3, 4)),
        x1 = c(1, 2, 3, 4),
        x2 = c(5, 6, 7, 8)
      ),
      link = "logit",
      base = NA_character_,
      threshold = "flexible",
      K = 4,
      N = 4,
      P = 2
    ),
    class = "clmstan"
  )

  coefficients <- coef(mock_obj)

  # Check structure
  expect_type(coefficients, "double")
  expect_true(is.numeric(coefficients))

  # Check names (thresholds first, then predictors)
  expected_names <- c("1|2", "2|3", "3|4", "x1", "x2")
  expect_equal(names(coefficients), expected_names)

  # Check values are approximately correct (unname to compare just values)
  expect_equal(unname(coefficients["1|2"]), 0, tolerance = 0.1)
  expect_equal(unname(coefficients["2|3"]), 1.2, tolerance = 0.1)
  expect_equal(unname(coefficients["3|4"]), 3.1, tolerance = 0.1)
  expect_equal(unname(coefficients["x1"]), 2.5, tolerance = 0.1)
  expect_equal(unname(coefficients["x2"]), 1.5, tolerance = 0.1)
})

# =============================================================================
# Tests for summary.clmstan and plot.clmstan
# =============================================================================

# Helper to create a more complete mock fit for summary/plot
create_mock_fit_for_summary <- function(beta_values, c_transformed_values, beta0_value = -1.0) {
  n_samples <- 100
  n_chains <- 4
  K_minus_1 <- length(c_transformed_values)
  P <- length(beta_values)

  list(
    draws = function(variables, format = "matrix") {
      if (format == "draws_df") {
        # Return a draws_df compatible format
        draws_list <- list()

        if ("c_transformed" %in% variables || any(grepl("c_transformed", variables))) {
          for (k in seq_len(K_minus_1)) {
            col_name <- paste0("c_transformed[", k, "]")
            draws_list[[col_name]] <- rep(c_transformed_values[k], n_samples * n_chains) +
              rnorm(n_samples * n_chains, 0, 0.01)
          }
        }

        if ("beta" %in% variables || any(grepl("beta\\[", variables))) {
          if (P > 0) {
            for (p in seq_len(P)) {
              col_name <- paste0("beta[", p, "]")
              draws_list[[col_name]] <- rep(beta_values[p], n_samples * n_chains) +
                rnorm(n_samples * n_chains, 0, 0.01)
            }
          }
        }

        if ("beta0" %in% variables) {
          draws_list[["beta0"]] <- rep(beta0_value, n_samples * n_chains) +
            rnorm(n_samples * n_chains, 0, 0.01)
        }

        # Add required columns for draws_df
        draws_list[[".chain"]] <- rep(1:n_chains, each = n_samples)
        draws_list[[".iteration"]] <- rep(1:n_samples, n_chains)
        draws_list[[".draw"]] <- seq_len(n_samples * n_chains)

        df <- as.data.frame(draws_list)
        class(df) <- c("draws_df", "draws", "tbl_df", "tbl", "data.frame")
        return(df)
      } else if (format == "matrix") {
        if ("c_transformed" %in% variables || variables == "c_transformed") {
          mat <- matrix(
            rep(c_transformed_values, each = n_samples) + rnorm(n_samples * K_minus_1, 0, 0.01),
            nrow = n_samples,
            ncol = K_minus_1
          )
          colnames(mat) <- paste0("c_transformed[", 1:K_minus_1, "]")
          return(mat)
        } else if ("beta" %in% variables || variables == "beta") {
          if (P > 0) {
            mat <- matrix(
              rep(beta_values, each = n_samples) + rnorm(n_samples * P, 0, 0.01),
              nrow = n_samples,
              ncol = P
            )
            colnames(mat) <- paste0("beta[", 1:P, "]")
            return(mat)
          } else {
            return(matrix(nrow = n_samples, ncol = 0))
          }
        } else if ("beta0" %in% variables || variables == "beta0") {
          mat <- matrix(
            rep(beta0_value, n_samples) + rnorm(n_samples, 0, 0.01),
            nrow = n_samples,
            ncol = 1
          )
          colnames(mat) <- "beta0"
          return(mat)
        }
      } else if (format == "draws_array") {
        # For plot function
        all_vars <- c()
        if ("beta" %in% variables && P > 0) {
          all_vars <- c(all_vars, paste0("beta[", 1:P, "]"))
        }
        if (any(grepl("c_transformed", variables))) {
          c_vars <- variables[grepl("c_transformed", variables)]
          all_vars <- c(all_vars, c_vars)
        }
        if ("beta0" %in% variables) {
          all_vars <- c(all_vars, "beta0")
        }

        arr <- array(
          rnorm(n_samples * n_chains * length(all_vars)),
          dim = c(n_samples, n_chains, length(all_vars)),
          dimnames = list(NULL, NULL, all_vars)
        )
        class(arr) <- c("draws_array", "draws", "array")
        return(arr)
      }
      stop("Unexpected call to draws()")
    }
  )
}

test_that("summary.clmstan returns correct structure", {
  mock_fit <- create_mock_fit_for_summary(
    beta_values = c(2.5, 1.5),
    c_transformed_values = c(0, 1.2, 3.1)
  )

  mock_obj <- structure(
    list(
      fit = mock_fit,
      formula = y ~ x1 + x2,
      data = data.frame(
        y = factor(c(1, 2, 3, 4)),
        x1 = c(1, 2, 3, 4),
        x2 = c(5, 6, 7, 8)
      ),
      link = "logit",
      base = NA_character_,
      threshold = "flexible",
      K = 4,
      N = 4,
      P = 2
    ),
    class = "clmstan"
  )

  summ <- summary(mock_obj)

  # Check class
  expect_s3_class(summ, "summary.clmstan")

  # Check structure
  expect_true("coefficients" %in% names(summ))
  expect_true("thresholds" %in% names(summ))
  expect_true("beta0" %in% names(summ))
  expect_true("model_info" %in% names(summ))

  # Check model_info
  expect_equal(summ$model_info$link, "logit")
  expect_equal(summ$model_info$threshold, "flexible")
  expect_equal(summ$model_info$K, 4)
  expect_equal(summ$model_info$N, 4)
  expect_equal(summ$model_info$P, 2)
})

test_that("summary.clmstan has correct coefficient names", {
  mock_fit <- create_mock_fit_for_summary(
    beta_values = c(2.5),
    c_transformed_values = c(0, 1.2)
  )

  mock_obj <- structure(
    list(
      fit = mock_fit,
      formula = y ~ x1,
      data = data.frame(
        y = factor(c("low", "mid", "high"), levels = c("low", "mid", "high")),
        x1 = c(1, 2, 3)
      ),
      link = "probit",
      base = NA_character_,
      threshold = "flexible",
      K = 3,
      N = 3,
      P = 1
    ),
    class = "clmstan"
  )

  summ <- summary(mock_obj)

  # Check coefficient names
  expect_equal(summ$coefficients$variable, "x1")

  # Check threshold names
  expect_equal(summ$thresholds$variable, c("low|mid", "mid|high"))
})

test_that("summary.clmstan works with intercept-only model", {
  n_samples <- 100
  n_chains <- 4
  total_draws <- n_samples * n_chains

  mock_fit <- list(
    draws = function(variables, format = "matrix") {
      if (format == "draws_df") {
        # Create draws_df with correct column naming
        draws_list <- list()
        draws_list[["c_transformed[1]"]] <- rep(0, total_draws) +
          rnorm(total_draws, 0, 0.01)
        draws_list[["c_transformed[2]"]] <- rep(1.5, total_draws) +
          rnorm(total_draws, 0, 0.01)

        if ("beta0" %in% variables) {
          draws_list[["beta0"]] <- rep(-0.5, total_draws) +
            rnorm(total_draws, 0, 0.01)
        }

        draws_list[[".chain"]] <- rep(1:n_chains, each = n_samples)
        draws_list[[".iteration"]] <- rep(1:n_samples, n_chains)
        draws_list[[".draw"]] <- seq_len(total_draws)

        df <- as.data.frame(draws_list, check.names = FALSE)
        class(df) <- c("draws_df", "draws", "tbl_df", "tbl", "data.frame")
        return(df)
      }
      stop("Unexpected call")
    }
  )

  mock_obj <- structure(
    list(
      fit = mock_fit,
      formula = y ~ 1,
      data = data.frame(y = factor(c(1, 2, 3))),
      link = "logit",
      base = NA_character_,
      threshold = "flexible",
      K = 3,
      N = 3,
      P = 0
    ),
    class = "clmstan"
  )

  summ <- summary(mock_obj)

  # Coefficients should be NULL for intercept-only
  expect_null(summ$coefficients)

  # Thresholds should exist
  expect_equal(nrow(summ$thresholds), 2)
})

test_that("plot.clmstan returns ggplot object", {
  skip_if_not_installed("bayesplot")

  mock_fit <- create_mock_fit_for_summary(
    beta_values = c(2.5),
    c_transformed_values = c(0, 1.2)
  )

  mock_obj <- structure(
    list(
      fit = mock_fit,
      formula = y ~ x1,
      data = data.frame(
        y = factor(c(1, 2, 3)),
        x1 = c(1, 2, 3)
      ),
      link = "logit",
      base = NA_character_,
      threshold = "flexible",
      K = 3,
      N = 3,
      P = 1
    ),
    class = "clmstan"
  )

  # Test each plot type returns a ggplot
  for (plot_type in c("trace", "dens", "hist", "areas", "intervals")) {
    p <- plot(mock_obj, type = plot_type)
    expect_true(inherits(p, "ggplot"),
                label = paste("Plot type", plot_type, "should return ggplot"))
  }
})

test_that("print.clmstan outputs expected format", {
  mock_obj <- structure(
    list(
      formula = y ~ x1 + x2,
      link = "logit",
      base = NA_character_,
      threshold = "flexible",
      K = 4,
      N = 100,
      P = 2
    ),
    class = "clmstan"
  )

  output <- capture.output(print(mock_obj))

  expect_true(any(grepl("Cumulative Link Model", output)))
  expect_true(any(grepl("Formula:", output)))
  expect_true(any(grepl("Link: logit", output)))
  expect_true(any(grepl("Categories: 4", output)))
})

test_that("print.clmstan shows SP link with base", {
  mock_obj <- structure(
    list(
      formula = y ~ x1,
      link = "sp",
      base = "probit",
      threshold = "flexible",
      K = 3,
      N = 50,
      P = 1
    ),
    class = "clmstan"
  )

  output <- capture.output(print(mock_obj))

  expect_true(any(grepl("Link: sp", output)))
  expect_true(any(grepl("base: probit", output)))
})

test_that("coef.clmstan with type='median' returns median estimates", {
  mock_fit <- create_mock_fit(
    beta_values = c(2.5),
    c_transformed_values = c(0, 1.2)
  )

  mock_obj <- structure(
    list(
      fit = mock_fit,
      formula = y ~ x,
      data = data.frame(
        y = factor(c(1, 2, 3)),
        x = c(1, 2, 3)
      ),
      link = "logit",
      base = NA_character_,
      threshold = "flexible",
      K = 3,
      N = 3,
      P = 1
    ),
    class = "clmstan"
  )

  coef_mean <- coef(mock_obj, type = "mean")
  coef_median <- coef(mock_obj, type = "median")

  # Both should return the same structure
  expect_equal(names(coef_mean), names(coef_median))
  expect_equal(length(coef_mean), length(coef_median))

  # Values should be similar (since mock has small variance)
  expect_equal(coef_mean, coef_median, tolerance = 0.1)
})

test_that("coef.clmstan works with P=0 (intercept-only model)", {
  mock_fit <- list(
    draws = function(variables, format = "matrix") {
      if ("c_transformed" %in% variables || variables == "c_transformed") {
        n_samples <- 100
        mat <- matrix(
          rep(c(0, 1.5), each = n_samples) + rnorm(n_samples * 2, 0, 0.01),
          nrow = n_samples,
          ncol = 2
        )
        colnames(mat) <- c("c_transformed[1]", "c_transformed[2]")
        return(mat)
      }
      stop("Unexpected call to draws()")
    }
  )

  mock_obj <- structure(
    list(
      fit = mock_fit,
      formula = y ~ 1,
      data = data.frame(
        y = factor(c(1, 2, 3))
      ),
      link = "logit",
      base = NA_character_,
      threshold = "flexible",
      K = 3,
      N = 3,
      P = 0
    ),
    class = "clmstan"
  )

  coefficients <- coef(mock_obj)

  # Only thresholds, no beta coefficients
  expect_equal(length(coefficients), 2)
  expect_equal(names(coefficients), c("1|2", "2|3"))
})

# =============================================================================
# Tests for link parameter summary
# =============================================================================

# Helper to create mock fit with link parameters
create_mock_fit_with_link_params <- function(beta_values, c_transformed_values,
                                              beta0_value = -1.0, link_params = list()) {
  n_samples <- 100
  n_chains <- 4
  total_draws <- n_samples * n_chains
  K_minus_1 <- length(c_transformed_values)
  P <- length(beta_values)

  list(
    draws = function(variables, format = "matrix") {
      if (format == "draws_df") {
        draws_list <- list()

        # Standard parameters
        if ("c_transformed" %in% variables || any(grepl("c_transformed", variables))) {
          for (k in seq_len(K_minus_1)) {
            col_name <- paste0("c_transformed[", k, "]")
            draws_list[[col_name]] <- rep(c_transformed_values[k], total_draws) +
              rnorm(total_draws, 0, 0.01)
          }
        }

        if ("beta" %in% variables || any(grepl("beta\\[", variables))) {
          if (P > 0) {
            for (p in seq_len(P)) {
              col_name <- paste0("beta[", p, "]")
              draws_list[[col_name]] <- rep(beta_values[p], total_draws) +
                rnorm(total_draws, 0, 0.01)
            }
          }
        }

        if ("beta0" %in% variables) {
          draws_list[["beta0"]] <- rep(beta0_value, total_draws) +
            rnorm(total_draws, 0, 0.01)
        }

        # Link parameters (with Stan name mapping)
        for (param_name in names(link_params)) {
          stan_name <- param_name
          # Map display names to Stan names
          if (param_name == "lambda_ao" || param_name == "lambda_lg") {
            stan_name <- param_name
          }
          if (stan_name %in% variables) {
            draws_list[[stan_name]] <- rep(link_params[[param_name]], total_draws) +
              rnorm(total_draws, 0, 0.01)
          }
        }

        draws_list[[".chain"]] <- rep(1:n_chains, each = n_samples)
        draws_list[[".iteration"]] <- rep(1:n_samples, n_chains)
        draws_list[[".draw"]] <- seq_len(total_draws)

        df <- as.data.frame(draws_list, check.names = FALSE)
        class(df) <- c("draws_df", "draws", "tbl_df", "tbl", "data.frame")
        return(df)
      }
      stop("Unexpected call to draws()")
    }
  )
}

test_that("summary.clmstan returns NULL link_params for non-full model", {
  mock_fit <- create_mock_fit_for_summary(
    beta_values = c(2.5),
    c_transformed_values = c(0, 1.2)
  )

  mock_obj <- structure(
    list(
      fit = mock_fit,
      formula = y ~ x,
      data = data.frame(y = factor(1:3), x = 1:3),
      link = "logit",
      base = NA_character_,
      threshold = "flexible",
      link_param = NULL,
      full = FALSE,
      K = 3, N = 3, P = 1
    ),
    class = "clmstan"
  )

  summ <- summary(mock_obj)
  expect_null(summ$link_params)
})

test_that("summary.clmstan returns link_params for full model with gev", {
  mock_fit <- create_mock_fit_with_link_params(
    beta_values = c(2.5),
    c_transformed_values = c(0, 1.2),
    link_params = list(xi = 0.25)
  )

  mock_obj <- structure(
    list(
      fit = mock_fit,
      formula = y ~ x,
      data = data.frame(y = factor(1:3), x = 1:3),
      link = "gev",
      base = NA_character_,
      threshold = "flexible",
      link_param = list(xi = "estimate"),
      full = TRUE,
      K = 3, N = 3, P = 1
    ),
    class = "clmstan"
  )

  summ <- summary(mock_obj)

  expect_false(is.null(summ$link_params))
  expect_equal(nrow(summ$link_params), 1)
  expect_equal(as.character(summ$link_params$variable), "xi")
  expect_true("mean" %in% names(summ$link_params))
  expect_true("sd" %in% names(summ$link_params))
  expect_true("rhat" %in% names(summ$link_params))
  expect_true("ess_bulk" %in% names(summ$link_params))
})

test_that("summary.clmstan handles AEP with both theta1 and theta2", {
  mock_fit <- create_mock_fit_with_link_params(
    beta_values = c(2.5),
    c_transformed_values = c(0, 1.2),
    link_params = list(theta1 = 2.0, theta2 = 1.5)
  )

  mock_obj <- structure(
    list(
      fit = mock_fit,
      formula = y ~ x,
      data = data.frame(y = factor(1:3), x = 1:3),
      link = "aep",
      base = NA_character_,
      threshold = "flexible",
      link_param = list(theta1 = "estimate", theta2 = "estimate"),
      full = TRUE,
      K = 3, N = 3, P = 1
    ),
    class = "clmstan"
  )

  summ <- summary(mock_obj)

  expect_false(is.null(summ$link_params))
  expect_equal(nrow(summ$link_params), 2)
  expect_true("theta1" %in% as.character(summ$link_params$variable))
  expect_true("theta2" %in% as.character(summ$link_params$variable))
})

test_that("summary.clmstan returns NULL link_params when full but no estimated params", {
  # Full model but all link params are fixed (not "estimate")
  mock_fit <- create_mock_fit_for_summary(
    beta_values = c(2.5),
    c_transformed_values = c(0, 1.2)
  )

  mock_obj <- structure(
    list(
      fit = mock_fit,
      formula = y ~ x,
      data = data.frame(y = factor(1:3), x = 1:3),
      link = "gev",
      base = NA_character_,
      threshold = "flexible",
      link_param = list(xi = 0.5),  # Fixed value, not "estimate"
      full = TRUE,
      K = 3, N = 3, P = 1
    ),
    class = "clmstan"
  )

  summ <- summary(mock_obj)
  expect_null(summ$link_params)
})

test_that("print.summary.clmstan displays link parameters section", {
  mock_fit <- create_mock_fit_with_link_params(
    beta_values = c(2.5),
    c_transformed_values = c(0, 1.2),
    link_params = list(xi = 0.25)
  )

  mock_obj <- structure(
    list(
      fit = mock_fit,
      formula = y ~ x,
      data = data.frame(y = factor(1:3), x = 1:3),
      link = "gev",
      base = NA_character_,
      threshold = "flexible",
      link_param = list(xi = "estimate"),
      full = TRUE,
      K = 3, N = 3, P = 1
    ),
    class = "clmstan"
  )

  summ <- summary(mock_obj)
  output <- capture.output(print(summ))

  expect_true(any(grepl("Link parameters", output)))
  expect_true(any(grepl("xi", output)))
})

test_that("print.summary.clmstan displays intercept in table format", {
  mock_fit <- create_mock_fit_for_summary(
    beta_values = c(2.5),
    c_transformed_values = c(0, 1.2),
    beta0_value = -0.5
  )

  mock_obj <- structure(
    list(
      fit = mock_fit,
      formula = y ~ x,
      data = data.frame(y = factor(1:3), x = 1:3),
      link = "logit",
      base = NA_character_,
      threshold = "flexible",
      link_param = NULL,
      full = FALSE,
      K = 3, N = 3, P = 1
    ),
    class = "clmstan"
  )

  summ <- summary(mock_obj)
  output <- capture.output(print(summ))

  # Check table format: should have column headers
  expect_true(any(grepl("Intercept", output)))
  expect_true(any(grepl("beta0", output)))
  expect_true(any(grepl("mean", output)))
  expect_true(any(grepl("rhat", output)))
})
