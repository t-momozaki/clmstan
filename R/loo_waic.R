# =============================================================================
# LOO-CV and WAIC for clmstan objects
# =============================================================================
# RELATED FILES:
# - R/methods.R: Other S3 methods for clmstan
# - R/predict.R: Pattern for extract_* helper functions
# - src/stan/clm_base.stan: log_lik computed in generated quantities (line 144)
# - src/stan/clm_full.stan: log_lik computed in generated quantities (line 265)
# =============================================================================

# =============================================================================
# Helper functions
# =============================================================================

#' Extract log-likelihood matrix from clmstan object
#'
#' @param object A clmstan object
#' @return An S x N matrix of log-likelihood values, where S is the number
#'   of posterior samples and N is the number of observations.
#' @keywords internal
extract_log_lik <- function(object) {
  # Check that fit object exists
  if (is.null(object$fit)) {
    stop("No Stan fit object found. The model may not have been fitted successfully.")
  }

  # Extract log_lik
  log_lik <- tryCatch(
    object$fit$draws(variables = "log_lik", format = "matrix"),
    error = function(e) {
      stop("Failed to extract log_lik from Stan fit. ",
           "Error: ", conditionMessage(e))
    }
  )

  # Validate dimensions
  if (ncol(log_lik) != object$N) {
    stop("log_lik dimension mismatch: expected ", object$N,
         " observations but got ", ncol(log_lik))
  }

  log_lik
}

#' Compute relative effective sample size for log-likelihood
#'
#' @param object A clmstan object
#' @param log_lik Log-likelihood matrix (S x N)
#' @param cores Number of cores for parallel computation
#' @return A vector of relative effective sample sizes (length N)
#' @keywords internal
compute_r_eff <- function(object, log_lik, cores = 1) {
  fit <- object$fit

  # Get chain information from CmdStanMCMC
  num_chains <- fit$num_chains()
  iter_sampling <- fit$metadata()$iter_sampling

  S <- nrow(log_lik)
  expected_S <- num_chains * iter_sampling

  if (S != expected_S) {
    # Draws may have been thinned or subsampled after Stan sampling.
    # Try to reconstruct chain structure by dividing draws evenly.
    if (S %% num_chains == 0) {
      draws_per_chain <- S / num_chains
      chain_id <- rep(seq_len(num_chains), each = draws_per_chain)
    } else {
      # Cannot evenly divide draws among chains; assume single chain.
      # r_eff = 1 is conservative (no efficiency gain from multiple chains).
      warning("Cannot determine chain structure. Using r_eff = 1.")
      return(rep(1, ncol(log_lik)))
    }
  } else {
    chain_id <- rep(seq_len(num_chains), each = iter_sampling)
  }

  # Compute relative effective sample size.
  # relative_eff() requires likelihood values (not log), so we exponentiate.
  # This measures how much MCMC autocorrelation reduces effective samples.
  loo::relative_eff(
    exp(log_lik),
    chain_id = chain_id,
    cores = cores
  )
}

# =============================================================================
# S3 methods
# =============================================================================

#' Leave-One-Out Cross-Validation for clmstan objects
#'
#' Computes approximate leave-one-out cross-validation (LOO-CV) for a
#' fitted cumulative link model using Pareto smoothed importance sampling (PSIS).
#'
#' @param x A \code{clmstan} object returned by \code{\link{clm_stan}}.
#' @param ... Additional arguments passed to \code{\link[loo]{loo}}.
#' @param r_eff A vector of relative effective sample sizes for each observation,
#'   or \code{NULL} (default) to compute them automatically using
#'   \code{\link[loo]{relative_eff}}. Set to \code{NA} to skip r_eff computation
#'   (faster but diagnostics may be over-optimistic).
#' @param cores The number of cores to use for parallel computation.
#'   Defaults to \code{getOption("mc.cores", 1)}.
#' @param save_psis If \code{TRUE}, the PSIS object is saved in the returned
#'   object. This is required for some downstream functions like \code{E_loo()}.
#'   Default is \code{FALSE}.
#'
#' @return An object of class \code{c("psis_loo", "loo")} containing:
#'   \itemize{
#'     \item \code{estimates}: A matrix with columns \code{Estimate} and \code{SE}
#'       for \code{elpd_loo}, \code{p_loo}, and \code{looic}.
#'     \item \code{pointwise}: A matrix with pointwise contributions.
#'     \item \code{diagnostics}: A list with Pareto k values and effective
#'       sample sizes for each observation.
#'   }
#'
#' @details
#' The function extracts the log-likelihood matrix (\code{log_lik}) computed in
#' the generated quantities block of the Stan model and passes it to
#' \code{\link[loo]{loo}}.
#'
#' \strong{Pareto k diagnostics:} Observations with high Pareto k values
#' (k > 0.7) indicate potential problems with the LOO approximation for those
#' observations. Use \code{plot()} on the returned object to visualize the
#' Pareto k values.
#'
#' \strong{Model comparison:} Use \code{\link[loo]{loo_compare}} to compare
#' multiple models. Models with higher \code{elpd_loo} are preferred.
#'
#' @seealso
#' \code{\link{waic.clmstan}} for WAIC computation,
#' \code{\link[loo]{loo}} for details on the LOO algorithm,
#' \code{\link[loo]{loo_compare}} for model comparison.
#'
#' @examples
#' \dontrun{
#' fit <- clm_stan(rating ~ temp, data = wine)
#' loo_result <- loo(fit)
#' print(loo_result)
#' plot(loo_result)
#'
#' # Compare two models
#' fit1 <- clm_stan(rating ~ temp, data = wine, link = "logit")
#' fit2 <- clm_stan(rating ~ temp, data = wine, link = "probit")
#' loo::loo_compare(loo(fit1), loo(fit2))
#' }
#'
#' @importFrom loo loo relative_eff
#' @method loo clmstan
#' @export
loo.clmstan <- function(x, ..., r_eff = NULL,
                        cores = getOption("mc.cores", 1),
                        save_psis = FALSE) {
  # Validate input
  if (!inherits(x, "clmstan")) {
    stop("'x' must be a clmstan object")
  }

  # Extract log-likelihood matrix
  log_lik <- extract_log_lik(x)

  # Handle r_eff argument
  if (is.null(r_eff)) {
    # Compute r_eff automatically (recommended)
    message("Computing r_eff (relative effective sample size)...")
    r_eff <- compute_r_eff(x, log_lik, cores = cores)
  } else if (identical(r_eff, NA)) {
    # Skip r_eff computation (faster but less accurate)
    r_eff <- rep(1, ncol(log_lik))
  }
  # Otherwise, use user-provided r_eff

  # Call loo::loo with the log-likelihood matrix
  loo_result <- loo::loo(
    log_lik,
    r_eff = r_eff,
    cores = cores,
    save_psis = save_psis,
    ...
  )

  # Add model information for loo_compare compatibility
  attr(loo_result, "model_name") <- deparse(x$formula)
  attr(loo_result, "nobs") <- x$N

  loo_result
}

#' Widely Applicable Information Criterion for clmstan objects
#'
#' Computes the Widely Applicable Information Criterion (WAIC) for a
#' fitted cumulative link model.
#'
#' @param x A \code{clmstan} object returned by \code{\link{clm_stan}}.
#' @param ... Additional arguments (currently ignored).
#'
#' @return An object of class \code{c("waic", "loo")} containing:
#'   \itemize{
#'     \item \code{estimates}: A matrix with columns \code{Estimate} and \code{SE}
#'       for \code{elpd_waic}, \code{p_waic}, and \code{waic}.
#'     \item \code{pointwise}: A matrix with pointwise contributions.
#'   }
#'
#' @details
#' WAIC is an alternative to LOO-CV that is asymptotically equivalent to
#' leave-one-out cross-validation. However, LOO-CV with PSIS is generally
#' preferred because:
#' \itemize{
#'   \item It provides useful diagnostics (Pareto k values)
#'   \item It is more robust in finite samples
#'   \item It has been shown to be more reliable in practice
#' }
#'
#' For most purposes, \code{\link{loo.clmstan}} is recommended over WAIC.
#'
#' @seealso
#' \code{\link{loo.clmstan}} for LOO-CV (recommended),
#' \code{\link[loo]{waic}} for details on WAIC computation,
#' \code{\link[loo]{loo_compare}} for model comparison.
#'
#' @examples
#' \dontrun{
#' fit <- clm_stan(rating ~ temp, data = wine)
#' waic_result <- waic(fit)
#' print(waic_result)
#' }
#'
#' @importFrom loo waic
#' @method waic clmstan
#' @export
waic.clmstan <- function(x, ...) {
  # Validate input
  if (!inherits(x, "clmstan")) {
    stop("'x' must be a clmstan object")
  }

  # Extract log-likelihood matrix
  log_lik <- extract_log_lik(x)

  # Call loo::waic
  waic_result <- loo::waic(log_lik, ...)

  # Add model information
  attr(waic_result, "model_name") <- deparse(x$formula)
  attr(waic_result, "nobs") <- x$N

  waic_result
}
