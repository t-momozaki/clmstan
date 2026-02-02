# Helper functions for simulation-based tests

#' Simulate ordinal response data with known parameters
#'
#' Generates ordinal categorical data from a cumulative link model with
#' specified true parameters. Used for testing parameter recovery.
#'
#' @param n Number of observations
#' @param K Number of ordinal categories (K >= 2)
#' @param beta Numeric vector of regression coefficients (length P)
#' @param thresholds Numeric vector of thresholds (length K-1),
#'   with thresholds[1] = 0 (c1=0 identification constraint)
#' @param link Link function name (e.g., "logit", "probit", "gev")
#' @param link_param List of link parameters (for flexible links)
#' @param X Optional design matrix. If NULL, generates standard normal predictors.
#' @param seed Random seed for reproducibility
#'
#' @return A list containing:
#'   \item{data}{data.frame with columns 'y' (factor) and predictors}
#'   \item{true_params}{list of true parameters used for generation}
#'   \item{eta}{linear predictor values}
#'   \item{probs}{category probability matrix (N x K)}
#'
#' @examples
#' # Simulate logit data
#' sim <- simulate_ordinal_data(
#'   n = 200, K = 4,
#'   beta = c(1.5, -0.8),
#'   thresholds = c(0, 1.2, 2.5),
#'   link = "logit"
#' )
#'
#' # Simulate GEV data with xi = 0.3
#' sim_gev <- simulate_ordinal_data(
#'   n = 300, K = 5,
#'   beta = c(1.0),
#'   thresholds = c(0, 0.8, 1.6, 2.4),
#'   link = "gev",
#'   link_param = list(xi = 0.3)
#' )
#'
simulate_ordinal_data <- function(n,
                                   K,
                                   beta,
                                   thresholds,
                                   link = "logit",
                                   link_param = list(),
                                   X = NULL,
                                   seed = NULL) {
  # Input validation
  stopifnot(K >= 2)
  stopifnot(length(thresholds) == K - 1)
  stopifnot(abs(thresholds[1]) < 1e-10)  # c1 must be 0
  stopifnot(all(diff(thresholds) > 0))   # thresholds must be ordered

  if (!is.null(seed)) set.seed(seed)

  P <- length(beta)

  # Generate design matrix if not provided
  if (is.null(X)) {
    if (P > 0) {
      X <- matrix(rnorm(n * P), nrow = n, ncol = P)
      colnames(X) <- paste0("x", seq_len(P))
    } else {
      X <- matrix(0, nrow = n, ncol = 0)
    }
  }

  # Compute linear predictor
  # Note: In CLM, eta = X %*% beta (no intercept; absorbed into thresholds)
  if (P > 0) {
    eta <- as.numeric(X %*% beta)
  } else {
    eta <- rep(0, n)
  }

  # Compute cumulative probabilities using clm_cdf
  # P(Y <= k) = F(c_k - eta)
  # Extend thresholds: c_0 = -Inf, c_K = +Inf
  c_extended <- c(-Inf, thresholds, Inf)

  # Cumulative probabilities matrix (N x K)
  cum_probs <- matrix(0, nrow = n, ncol = K)
  for (k in seq_len(K)) {
    # P(Y <= k) = F(c_k - eta)
    cum_probs[, k] <- clmstan:::clm_cdf(c_extended[k + 1] - eta, link, link_param)
  }

  # Category probabilities: P(Y = k) = P(Y <= k) - P(Y <= k-1)
  probs <- matrix(0, nrow = n, ncol = K)
  probs[, 1] <- cum_probs[, 1]
  for (k in 2:K) {
    probs[, k] <- cum_probs[, k] - cum_probs[, k - 1]
  }

  # Handle numerical issues (ensure valid probabilities)
  probs[probs < 0] <- 0
  probs <- probs / rowSums(probs)

  # Sample ordinal responses
  y <- integer(n)
  for (i in seq_len(n)) {
    y[i] <- sample(seq_len(K), 1, prob = probs[i, ])
  }

  # Create data frame
  data <- as.data.frame(X)
  data$y <- factor(y, levels = seq_len(K))

  # Return with true parameters for comparison
  list(
    data = data,
    true_params = list(
      beta = beta,
      thresholds = thresholds,
      link = link,
      link_param = link_param
    ),
    eta = eta,
    probs = probs
  )
}


#' Generate simple ordinal data for quick tests
#'
#' A convenience wrapper for simulate_ordinal_data with sensible defaults.
#' Used for basic functionality tests where parameter recovery is not critical.
#'
#' @param n Number of observations (default 50)
#' @param K Number of categories (default 3)
#' @param link Link function name
#' @param link_param List of link parameters
#' @param seed Random seed
#'
#' @return Same as simulate_ordinal_data()
#'
generate_test_data <- function(n = 50,
                                K = 3,
                                link = "logit",
                                link_param = list(),
                                seed = NULL) {
  # Default: one predictor, simple thresholds
  beta <- c(0.5)
  thresholds <- seq(0, by = 1, length.out = K - 1)

  simulate_ordinal_data(
    n = n,
    K = K,
    beta = beta,
    thresholds = thresholds,
    link = link,
    link_param = link_param,
    seed = seed
  )
}
