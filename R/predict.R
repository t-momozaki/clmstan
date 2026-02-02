# =============================================================================
# Prediction methods for clmstan objects
# =============================================================================
#
# RELATED FILES:
# - R/link_cdfs.R: CDF implementations for each link function
# - R/methods.R: Other S3 methods (print, summary, coef, plot)
# - R/prepare_data.R: parse_clm_formula(), make_design_matrix()
# - R/links.R: get_link_params(), is_flexible_link()
#
# =============================================================================

# =============================================================================
# Main exported functions
# =============================================================================

#' Predict method for clmstan objects
#'
#' Generates predictions from a fitted cumulative link model.
#'
#' @param object A `clmstan` object returned by [clm_stan()].
#' @param newdata Optional data frame for prediction. If `NULL` (default),
#'   predictions are made for the original training data.
#' @param type Type of prediction:
#'   * `"class"`: Predicted category (most likely class)
#'   * `"probs"`: Predicted probabilities for each category
#' @param summary Logical. If `TRUE` (default), return summary statistics
#'   (mean, SD, quantiles). If `FALSE`, return raw posterior draws.
#' @param robust Logical. If `TRUE`, use median instead of mean for
#'   point estimates. Default is `FALSE`.
#' @param probs Numeric vector of probabilities for quantiles.
#'   Default is `c(0.025, 0.975)` for 95% credible intervals.
#' @param ndraws Number of posterior draws to use. If `NULL` (default),
#'   all available draws are used.
#' @param ... Additional arguments (currently ignored).
#'
#' @return Depending on `type` and `summary`:
#'   * `type = "class"`, `summary = TRUE`: A data frame with columns
#'     `Estimate` (mean/median predicted class), `Est.Error` (SD),
#'     quantile columns, and `Class` (modal predicted category).
#'   * `type = "class"`, `summary = FALSE`: An S x N integer matrix
#'     of predicted categories (1 to K), where S is the number of
#'     posterior draws and N is the number of observations.
#'   * `type = "probs"`, `summary = TRUE`: A data frame with columns
#'     for each category probability (`P[Y=1]`, `P[Y=2]`, etc.).
#'   * `type = "probs"`, `summary = FALSE`: An S x N x K array of
#'     predicted probabilities.
#'
#' @seealso [fitted.clmstan()] for expected probabilities,
#'   [posterior_predict.clmstan()] for posterior predictive samples.
#'
#' @export
predict.clmstan <- function(object, newdata = NULL, type = c("class", "probs"),
                            summary = TRUE, robust = FALSE,
                            probs = c(0.025, 0.975), ndraws = NULL, ...) {
  type <- match.arg(type)

  # Validate inputs
  validate_prediction_input(object, newdata, ndraws)

  # Fast path: For training data class predictions, reuse y_rep already

  # sampled in Stan. This is faster than recomputing and uses exact
  # posterior draws (no approximation from subsetting draws).
  if (is.null(newdata) && type == "class" && !summary) {
    return(extract_y_rep(object, ndraws))
  }

  # Prepare data for prediction
  if (is.null(newdata)) {
    newdata <- object$data
  }

  # Create design matrix
  X <- prepare_prediction_matrix(object, newdata)

  # Extract posterior draws
  draws <- extract_prediction_draws(object, ndraws)

  # Get link parameters (fixed or from draws)
  base_link_param <- get_base_link_params(object)

  # Compute probabilities for all draws: S x N x K array
  probs_array <- compute_probs_all(draws, X, object$link, base_link_param,
                                   object$K, object$full)

  if (type == "class") {
    # Convert probabilities to predicted classes
    class_draws <- apply(probs_array, c(1, 2), which.max)  # S x N matrix

    if (summary) {
      result <- summarize_class_draws(class_draws, robust, probs)
      return(result)
    } else {
      return(class_draws)
    }
  } else {
    # type == "probs"
    if (summary) {
      return(summarize_probs_draws(probs_array, robust))
    } else {
      return(probs_array)
    }
  }
}

#' Fitted values for clmstan objects
#'
#' Returns expected category probabilities for each observation.
#' This is equivalent to `predict(object, type = "probs", summary = TRUE)`.
#'
#' @inheritParams predict.clmstan
#'
#' @return If `summary = TRUE` (default): A data frame with N rows and
#'   columns for each category probability (`P[Y=1]`, `P[Y=2]`, etc.).
#'   If `summary = FALSE`: An S x N x K array of probability draws.
#'
#' @seealso [predict.clmstan()], [posterior_predict.clmstan()]
#'
#' @export
fitted.clmstan <- function(object, newdata = NULL,
                           summary = TRUE, robust = FALSE,
                           probs = c(0.025, 0.975), ndraws = NULL, ...) {
  predict.clmstan(object, newdata = newdata, type = "probs",
                  summary = summary, robust = robust,
                  probs = probs, ndraws = ndraws, ...)
}

#' Posterior predictive distribution for clmstan objects
#'
#' Draws from the posterior predictive distribution. For each posterior
#' sample, a predicted category is sampled from the categorical distribution
#' with the predicted probabilities.
#'
#' @inheritParams predict.clmstan
#'
#' @return An integer matrix of dimension S x N containing predicted
#'   categories (1 to K), where S is the number of posterior draws and
#'   N is the number of observations.
#'
#' @seealso [predict.clmstan()], [fitted.clmstan()]
#'
#' @export
posterior_predict.clmstan <- function(object, newdata = NULL,
                                      ndraws = NULL, ...) {
  # Handle training data case - use y_rep from Stan (most efficient)
  if (is.null(newdata)) {
    return(extract_y_rep(object, ndraws))
  }

  # For new data, compute probabilities and sample
  probs_array <- predict.clmstan(object, newdata = newdata, type = "probs",
                                  summary = FALSE, ndraws = ndraws)

  S <- dim(probs_array)[1]
  N <- dim(probs_array)[2]
  K <- dim(probs_array)[3]

  # Sample categories from probabilities
  y_pred <- matrix(0L, nrow = S, ncol = N)
  for (s in seq_len(S)) {
    for (n in seq_len(N)) {
      y_pred[s, n] <- sample.int(K, 1, prob = probs_array[s, n, ])
    }
  }

  y_pred
}

# =============================================================================
# Helper functions: Input validation
# =============================================================================

#' Validate prediction inputs
#'
#' @param object A clmstan object
#' @param newdata New data frame (or NULL)
#' @param ndraws Number of draws to use
#' @keywords internal
validate_prediction_input <- function(object, newdata, ndraws) {
  # Check object class
  if (!inherits(object, "clmstan")) {
    stop("'object' must be a clmstan object")
  }

  # Check newdata compatibility
  if (!is.null(newdata)) {
    if (!is.data.frame(newdata)) {
      stop("'newdata' must be a data frame")
    }

    # Get required variables from formula (excluding response)
    required_vars <- all.vars(object$formula)[-1]
    missing_vars <- setdiff(required_vars, names(newdata))
    if (length(missing_vars) > 0) {
      stop("Variables missing from newdata: ", paste(missing_vars, collapse = ", "))
    }

    # Check factor levels match training data
    for (var in required_vars) {
      if (is.factor(object$data[[var]])) {
        if (var %in% names(newdata)) {
          new_levels <- unique(as.character(newdata[[var]]))
          train_levels <- levels(object$data[[var]])
          unknown_levels <- setdiff(new_levels, train_levels)
          if (length(unknown_levels) > 0) {
            stop("Unknown factor levels in '", var, "': ",
                 paste(unknown_levels, collapse = ", "))
          }
        }
      }
    }
  }

  # Check ndraws
  if (!is.null(ndraws)) {
    if (!is.numeric(ndraws) || length(ndraws) != 1 || ndraws < 1) {
      stop("'ndraws' must be a positive integer")
    }
  }

  invisible(TRUE)
}

# =============================================================================
# Helper functions: Data preparation
# =============================================================================

#' Prepare design matrix for prediction
#'
#' @param object A clmstan object
#' @param newdata Data frame for prediction
#' @return Design matrix (N x P)
#' @keywords internal
prepare_prediction_matrix <- function(object, newdata) {
  # Parse formula to get predictor formula
  parsed <- parse_clm_formula(object$formula, object$data)

  # Create design matrix from newdata
  # Note: Use contrasts from original data for consistency
  X <- model.matrix(parsed$predictor_formula, data = newdata)

  # Remove intercept column if present
  intercept_col <- which(colnames(X) == "(Intercept)")
  if (length(intercept_col) > 0) {
    X <- X[, -intercept_col, drop = FALSE]
  }

  X
}

#' Get base (fixed) link parameters from object
#'
#' @param object A clmstan object
#' @return List of fixed link parameters
#' @keywords internal
get_base_link_params <- function(object) {
  result <- list()

  if (!is.null(object$link_param)) {
    for (name in names(object$link_param)) {
      val <- object$link_param[[name]]
      if (is.numeric(val)) {
        result[[name]] <- val
      }
    }
  }

  # Add base for SP link
  if (object$link == "sp" && !is.null(object$base) && !is.na(object$base)) {
    result$base <- object$base
  }

  result
}

# =============================================================================
# Helper functions: Posterior draw extraction
# =============================================================================

#' Extract y_rep from Stan fit
#'
#' @param object A clmstan object
#' @param ndraws Number of draws to extract
#' @return S x N integer matrix
#' @keywords internal
extract_y_rep <- function(object, ndraws = NULL) {
  y_rep <- object$fit$draws(variables = "y_rep", format = "matrix")

  if (!is.null(ndraws) && ndraws < nrow(y_rep)) {
    idx <- sample.int(nrow(y_rep), ndraws)
    y_rep <- y_rep[idx, , drop = FALSE]
  }

  # Ensure integer
  storage.mode(y_rep) <- "integer"
  y_rep
}

#' Extract posterior draws needed for prediction
#'
#' @param object A clmstan object
#' @param ndraws Number of draws to extract
#' @return List with c (cutpoints), beta (coefficients), link_params (if full model)
#' @keywords internal
extract_prediction_draws <- function(object, ndraws = NULL) {
  fit <- object$fit

  # Extract raw cutpoints (not c_transformed)
  c_draws <- fit$draws(variables = "c", format = "matrix")

  # Extract beta if P > 0
  if (object$P > 0) {
    beta_draws <- fit$draws(variables = "beta", format = "matrix")
  } else {
    beta_draws <- NULL
  }

  # For full model, extract link parameters
  link_param_draws <- NULL
  if (object$full) {
    link_param_draws <- extract_link_param_draws(object)
  }

  # Subsample if ndraws specified
  S <- nrow(c_draws)
  if (!is.null(ndraws) && ndraws < S) {
    idx <- sample.int(S, ndraws)
    c_draws <- c_draws[idx, , drop = FALSE]
    if (!is.null(beta_draws)) {
      beta_draws <- beta_draws[idx, , drop = FALSE]
    }
    if (!is.null(link_param_draws)) {
      link_param_draws <- lapply(link_param_draws, function(x) x[idx])
    }
    S <- ndraws
  }

  list(
    c = c_draws,
    beta = beta_draws,
    link_params = link_param_draws,
    S = S
  )
}

#' Extract link parameter draws for full model
#'
#' @param object A clmstan object
#' @return List of link parameter vectors
#' @keywords internal
extract_link_param_draws <- function(object) {
  fit <- object$fit
  link <- object$link
  result <- list()

  # Helper to check if parameter was estimated
  has_est <- function(param_name) {
    if (is.null(object$link_param)) return(FALSE)
    param_val <- object$link_param[[param_name]]
    is.character(param_val) && param_val == "estimate"
  }

  # Extract estimated parameters
  if (link == "tlink") {
    if (has_est("df")) {
      result$df <- as.vector(fit$draws(variables = "df", format = "matrix"))
    }
  }

  if (link == "aranda_ordaz") {
    if (has_est("lambda")) {
      result$lambda <- as.vector(fit$draws(variables = "lambda_ao", format = "matrix"))
    }
  }

  if (link == "log_gamma") {
    if (has_est("lambda")) {
      result$lambda <- as.vector(fit$draws(variables = "lambda_lg", format = "matrix"))
    }
  }

  if (link == "gev") {
    if (has_est("xi")) {
      result$xi <- as.vector(fit$draws(variables = "xi", format = "matrix"))
    }
  }

  if (link == "sp") {
    if (has_est("r")) {
      result$r <- as.vector(fit$draws(variables = "r", format = "matrix"))
    }
    # df for tlink base
    if (has_est("df")) {
      result$df <- as.vector(fit$draws(variables = "df", format = "matrix"))
    }
  }

  if (link == "aep") {
    if (has_est("theta1")) {
      result$theta1 <- as.vector(fit$draws(variables = "theta1", format = "matrix"))
    }
    if (has_est("theta2")) {
      result$theta2 <- as.vector(fit$draws(variables = "theta2", format = "matrix"))
    }
  }

  result
}

# =============================================================================
# Helper functions: Probability computation
# =============================================================================

#' Compute category probabilities for a single draw
#'
#' @param c_vec Vector of K-1 cutpoints
#' @param eta_vec Vector of N linear predictors
#' @param link Link function name
#' @param link_param Link parameters (for this draw)
#' @param K Number of categories
#' @return Matrix of N x K probabilities
#' @keywords internal
compute_probs_single <- function(c_vec, eta_vec, link, link_param, K) {
  N <- length(eta_vec)
  probs <- matrix(0, nrow = N, ncol = K)

  for (k in seq_len(K)) {
    if (k == 1) {
      # P(Y = 1) = F(c_1 - eta)
      probs[, k] <- clm_cdf(c_vec[1] - eta_vec, link, link_param)
    } else if (k == K) {
      # P(Y = K) = 1 - F(c_{K-1} - eta)
      probs[, k] <- 1 - clm_cdf(c_vec[K - 1] - eta_vec, link, link_param)
    } else {
      # P(Y = k) = F(c_k - eta) - F(c_{k-1} - eta)
      probs[, k] <- clm_cdf(c_vec[k] - eta_vec, link, link_param) -
                    clm_cdf(c_vec[k - 1] - eta_vec, link, link_param)
    }
  }

  # Ensure valid probabilities (numerical safety).
  # CDF subtraction can produce small negative values or values > 1 due to
  # floating-point errors, especially at extreme eta values. Clamp to [0, 1].
  probs[probs < 0] <- 0
  probs[probs > 1] <- 1
  probs[is.na(probs)] <- 1 / K

  # Renormalize rows
  row_sums <- rowSums(probs)
  row_sums[row_sums == 0] <- 1
  probs <- probs / row_sums

  probs
}

#' Compute category probabilities for all draws
#'
#' @param draws Output from extract_prediction_draws()
#' @param X Design matrix (N x P)
#' @param link Link function name
#' @param base_link_param Fixed link parameters
#' @param K Number of categories
#' @param full Whether link parameters were estimated
#' @return Array of S x N x K probabilities
#' @keywords internal
compute_probs_all <- function(draws, X, link, base_link_param, K, full = FALSE) {
  S <- draws$S
  N <- nrow(X)

  # Pre-allocate result array
  probs_array <- array(0, dim = c(S, N, K))

  for (s in seq_len(S)) {
    # Get cutpoints for this draw
    c_vec <- draws$c[s, ]

    # Compute linear predictor
    if (!is.null(draws$beta) && ncol(X) > 0) {
      eta_vec <- as.vector(X %*% draws$beta[s, ])
    } else {
      eta_vec <- rep(0, N)
    }

    # Get link parameters for this draw
    if (full && !is.null(draws$link_params)) {
      link_param <- get_link_param_for_draw(draws$link_params, s, base_link_param)
    } else {
      link_param <- base_link_param
    }

    # Compute probabilities
    probs_array[s, , ] <- compute_probs_single(c_vec, eta_vec, link, link_param, K)
  }

  probs_array
}

#' Get link parameters for a specific draw
#'
#' @param link_param_draws List of link parameter vectors
#' @param s Draw index
#' @param base_param Base (fixed) parameters
#' @return Link parameters for draw s
#' @keywords internal
get_link_param_for_draw <- function(link_param_draws, s, base_param) {
  result <- base_param
  for (name in names(link_param_draws)) {
    result[[name]] <- link_param_draws[[name]][s]
  }
  result
}

# =============================================================================
# Helper functions: Summarization
# =============================================================================

#' Summarize class prediction draws
#'
#' @param class_draws S x N matrix of predicted classes
#' @param robust Use median instead of mean
#' @param probs Quantile probabilities
#' @return Data frame with summary statistics
#' @keywords internal
summarize_class_draws <- function(class_draws, robust = FALSE, probs = c(0.025, 0.975)) {
  center_fn <- if (robust) stats::median else mean

  # Point estimate and SD
  estimate <- apply(class_draws, 2, center_fn)
  est_error <- apply(class_draws, 2, stats::sd)

  # Quantiles
  quantiles <- apply(class_draws, 2, stats::quantile, probs = probs)

  # Modal class
  modal_class <- apply(class_draws, 2, function(x) {
    as.integer(names(which.max(table(x))))
  })

  # Build result data frame
  result <- data.frame(
    Estimate = estimate,
    Est.Error = est_error
  )

  # Add quantile columns
  if (is.matrix(quantiles)) {
    for (i in seq_len(nrow(quantiles))) {
      col_name <- paste0("Q", probs[i] * 100)
      result[[col_name]] <- quantiles[i, ]
    }
  } else {
    # Single quantile case
    col_name <- paste0("Q", probs[1] * 100)
    result[[col_name]] <- quantiles
  }

  result$Class <- modal_class

  result
}

#' Summarize probability draws
#'
#' @param probs_array S x N x K array of probability draws
#' @param robust Use median instead of mean
#' @return Data frame with mean probabilities per category
#' @keywords internal
summarize_probs_draws <- function(probs_array, robust = FALSE) {
  center_fn <- if (robust) stats::median else mean

  K <- dim(probs_array)[3]
  N <- dim(probs_array)[2]

  result <- data.frame(matrix(nrow = N, ncol = 0))

  for (k in seq_len(K)) {
    probs_k <- probs_array[, , k]  # S x N matrix
    result[[paste0("P[Y=", k, "]")]] <- apply(probs_k, 2, center_fn)
  }

  result
}
