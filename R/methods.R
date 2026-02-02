# =============================================================================
# Helper functions (internal)
# =============================================================================

#' Generate threshold names in ordinal::clm style
#'
#' Creates threshold names like "1|2", "2|3", etc. based on factor levels.
#'
#' @param object A clmstan object
#' @return A character vector of threshold names
#' @keywords internal
get_threshold_names <- function(object) {
  # Get response variable name from formula
  response_name <- all.vars(object$formula)[1]
  y <- object$data[[response_name]]

  if (!is.factor(y)) {
    y <- factor(y)
  }

  lvls <- levels(y)
  K <- length(lvls)

  # Generate names like "1|2", "2|3", etc.
  sapply(seq_len(K - 1), function(k) {
    paste(lvls[k], lvls[k + 1], sep = "|")
  })
}

#' Get coefficient names from design matrix
#'
#' @param object A clmstan object
#' @return A character vector of coefficient names
#' @keywords internal
get_coef_names <- function(object) {
  # Reconstruct design matrix to get column names
  parsed <- parse_clm_formula(object$formula, object$data)
  X <- make_design_matrix(parsed$predictor_formula, object$data)
  colnames(X)
}

#' Extract posterior summary for specified parameters
#'
#' @param fit A CmdStanMCMC object
#' @param pars Character vector of parameter names
#' @param probs Quantile probabilities
#' @return A data frame with posterior summaries
#' @keywords internal
extract_posterior_summary <- function(fit, pars, probs = c(0.025, 0.5, 0.975)) {
  # Get draws for specified parameters
  draws <- fit$draws(variables = pars, format = "draws_df")

  # Use posterior package for summary
  summary_df <- posterior::summarise_draws(
    draws,
    mean = mean,
    sd = stats::sd,
    ~stats::quantile(.x, probs = probs),
    rhat = posterior::rhat,
    ess_bulk = posterior::ess_bulk,
    ess_tail = posterior::ess_tail
  )

  summary_df
}

#' Extract posterior summary for link parameters
#'
#' @param object A clmstan object
#' @param probs Quantile probabilities
#' @return A data frame with posterior summaries, or NULL if no estimated params
#' @keywords internal
extract_link_params_summary <- function(object, probs = c(0.025, 0.5, 0.975)) {
  if (!isTRUE(object$full)) return(NULL)

  link <- object$link
  link_param <- object$link_param

  # Helper to check if parameter was estimated
  is_estimated <- function(param_name) {
    if (is.null(link_param)) return(FALSE)
    val <- link_param[[param_name]]
    is.character(val) && val == "estimate"
  }

  # Build list of estimated parameters with Stan names and display names
  param_info <- list()

  if (link == "tlink") {
    if (is_estimated("df")) {
      param_info[["df"]] <- list(stan_name = "df", display_name = "df")
    }
  }

  if (link == "aranda_ordaz") {
    if (is_estimated("lambda")) {
      param_info[["lambda"]] <- list(stan_name = "lambda", display_name = "lambda")
    }
  }

  if (link == "log_gamma") {
    if (is_estimated("lambda")) {
      param_info[["lambda"]] <- list(stan_name = "lambda", display_name = "lambda")
    }
  }

  if (link == "gev") {
    if (is_estimated("xi")) {
      param_info[["xi"]] <- list(stan_name = "xi", display_name = "xi")
    }
  }

  if (link == "sp") {
    if (is_estimated("r")) {
      param_info[["r"]] <- list(stan_name = "r", display_name = "r")
    }
    if (is_estimated("df")) {
      param_info[["df"]] <- list(stan_name = "df", display_name = "df")
    }
  }

  if (link == "aep") {
    if (is_estimated("theta1")) {
      param_info[["theta1"]] <- list(stan_name = "theta1", display_name = "theta1")
    }
    if (is_estimated("theta2")) {
      param_info[["theta2"]] <- list(stan_name = "theta2", display_name = "theta2")
    }
  }

  if (length(param_info) == 0) return(NULL)

  # Extract Stan variable names and display names
  stan_names <- vapply(param_info, function(x) x$stan_name, character(1))
  display_names <- vapply(param_info, function(x) x$display_name, character(1))

  # Get posterior summary using existing helper
  summary_df <- extract_posterior_summary(object$fit, stan_names, probs)

  # Replace Stan names with display names
  summary_df$variable <- display_names

  summary_df
}

#' Get draws for plotting with bayesplot
#'
#' Returns draws for default parameters (excluding log_lik, y_rep, eta, raw c).
#'
#' @param object A clmstan object
#' @param pars Character vector of parameter names (NULL for default)
#' @return A draws_array suitable for bayesplot
#' @keywords internal
get_clmstan_draws <- function(object, pars = NULL) {
  if (is.null(pars)) {
    # Default: beta, c_transformed[2:K-1], beta0
    pars_to_include <- c()

    # Add beta if P > 0
    if (object$P > 0) {
      pars_to_include <- c(pars_to_include, "beta")
    }

    # Add c_transformed (skip first since it's fixed at 0 for identifiability)
    # The first threshold c[1] is set to 0 as a reference point, and beta0
    # absorbs the location information. This is a standard identification
    # constraint in cumulative link models (same as ordinal::clm).
    if (object$K > 2) {
      c_indices <- 2:(object$K - 1)
      pars_to_include <- c(pars_to_include,
                           paste0("c_transformed[", c_indices, "]"))
    }

    # Add beta0 for non-symmetric thresholds
    if (object$threshold != "symmetric") {
      pars_to_include <- c(pars_to_include, "beta0")
    }

    pars <- pars_to_include
  }

  # Get draws as draws_array for bayesplot
  object$fit$draws(variables = pars, format = "draws_array")
}


# =============================================================================
# S3 Methods
# =============================================================================

#' Print method for clmstan objects
#'
#' @param x A clmstan object
#' @param ... Additional arguments (ignored)
#'
#' @return Invisibly returns x
#' @export
print.clmstan <- function(x, ...) {
  cat("Cumulative Link Model (clmstan)\n")
  cat("-------------------------------\n")
  cat("Formula:", deparse(x$formula), "\n")
  cat("Link:", x$link)
  if (x$link == "sp") {
    cat(" (base:", x$base, ")")
  }
  cat("\n")
  cat("Threshold:", x$threshold, "\n")
  cat("Categories:", x$K, "\n")
  cat("Observations:", x$N, "\n")
  cat("Predictors:", x$P, "\n")
  cat("\nUse summary() for parameter estimates.\n")
  invisible(x)
}

#' Summary method for clmstan objects
#'
#' @param object A clmstan object
#' @param probs Quantile probabilities for credible intervals
#' @param digits Number of significant digits for display
#' @param ... Additional arguments (ignored)
#'
#' @return An object of class "summary.clmstan" containing:
#'   \itemize{
#'     \item coefficients: Posterior summary for regression coefficients
#'     \item thresholds: Posterior summary for threshold parameters
#'     \item beta0: Posterior summary for intercept
#'     \item link_params: Posterior summary for link parameters (full model only)
#'     \item model_info: Model metadata (formula, link, threshold, K, N, P)
#'   }
#' @export
summary.clmstan <- function(object,
                            probs = c(0.025, 0.5, 0.975),
                            digits = 3,
                            ...) {
  # Prepare result structure
  result <- list(
    coefficients = NULL,
    thresholds = NULL,
    beta0 = NULL,
    link_params = NULL,
    model_info = list(
      formula = object$formula,
      link = object$link,
      base = object$base,
      threshold = object$threshold,
      K = object$K,
      N = object$N,
      P = object$P
    ),
    probs = probs,
    digits = digits
  )

  # Get threshold names
  threshold_names <- get_threshold_names(object)

  # --- Regression coefficients (beta) ---
  if (object$P > 0) {
    coef_names <- get_coef_names(object)
    beta_summary <- extract_posterior_summary(object$fit, "beta", probs)

    # Rename rows with coefficient names
    beta_summary$variable <- coef_names
    result$coefficients <- beta_summary
  }

  # --- Threshold coefficients (c_transformed) ---
  c_summary <- extract_posterior_summary(object$fit, "c_transformed", probs)

  # Rename rows with threshold names
  c_summary$variable <- threshold_names
  result$thresholds <- c_summary

  # --- Intercept (beta0) ---
  if (object$threshold != "symmetric") {
    beta0_summary <- extract_posterior_summary(object$fit, "beta0", probs)
    beta0_summary$variable <- "beta0"
    result$beta0 <- beta0_summary
  }

  # --- Link parameters (full model only) ---
  if (isTRUE(object$full)) {
    link_params_summary <- extract_link_params_summary(object, probs)
    if (!is.null(link_params_summary) && nrow(link_params_summary) > 0) {
      result$link_params <- link_params_summary
    }
  }

  class(result) <- "summary.clmstan"
  result
}

#' Print method for summary.clmstan objects
#'
#' @param x A summary.clmstan object
#' @param ... Additional arguments (ignored)
#'
#' @return Invisibly returns x
#' @export
print.summary.clmstan <- function(x, ...) {
  cat("Cumulative Link Model (clmstan)\n")
  cat("===============================\n\n")

  # Model info
  cat("Formula:", deparse(x$model_info$formula), "\n")
  cat("Link:", x$model_info$link)
  if (!is.na(x$model_info$base)) {
    cat(" (base:", x$model_info$base, ")")
  }
  cat("\n")
  cat("Threshold:", x$model_info$threshold, "\n")
  cat("Categories:", x$model_info$K, " | ",
      "Observations:", x$model_info$N, " | ",
      "Predictors:", x$model_info$P, "\n\n")

  # Format and print a posterior summary table.
  # Args:
  #   df: Data frame with columns: variable, mean, sd, quantiles, rhat, ess_*
  #   title: Section title to display above the table
  #   reference_row: Variable name to mark as reference (shows mean only, NA for others)
  print_summary_table <- function(df, title, reference_row = NULL) {
    cat(title, "\n")
    cat(strrep("-", nchar(title)), "\n")

    # Prepare display matrix
    display_cols <- c("variable", "mean", "sd",
                      names(df)[grep("%$", names(df))],
                      "rhat", "ess_bulk", "ess_tail")
    display_cols <- intersect(display_cols, names(df))
    display_df <- df[, display_cols, drop = FALSE]

    # Round numeric columns
    num_cols <- sapply(display_df, is.numeric)
    display_df[num_cols] <- lapply(display_df[num_cols], function(col) {
      round(col, x$digits)
    })

    # Mark reference row if specified
    if (!is.null(reference_row)) {
      ref_idx <- which(display_df$variable == reference_row)
      if (length(ref_idx) > 0) {
        # Replace numeric values with NA for reference
        for (col in names(display_df)[num_cols]) {
          if (col != "mean") {
            display_df[ref_idx, col] <- NA
          }
        }
      }
    }

    # Print as formatted table
    print(display_df, row.names = FALSE)
    cat("\n")
  }

  # Coefficients
  if (!is.null(x$coefficients) && nrow(x$coefficients) > 0) {
    print_summary_table(x$coefficients, "Coefficients:")
  }

  # Thresholds
  if (!is.null(x$thresholds) && nrow(x$thresholds) > 0) {
    # First threshold is reference (c_transformed[1] = 0)
    first_threshold <- x$thresholds$variable[1]
    cat("Threshold coefficients:\n")
    cat("(Note: First threshold fixed at 0 as reference)\n")
    print_summary_table(x$thresholds, "", reference_row = first_threshold)
  }

  # Intercept
  if (!is.null(x$beta0)) {
    print_summary_table(x$beta0, "Intercept (beta0):")
  }

  # Link parameters (for full model)
  if (!is.null(x$link_params) && nrow(x$link_params) > 0) {
    print_summary_table(x$link_params, "Link parameters:")
  }

  invisible(x)
}

#' Extract coefficients from clmstan objects
#'
#' Returns posterior point estimates (mean or median) for all model parameters.
#'
#' @param object A clmstan object
#' @param type Type of point estimate: "mean" (default) or "median"
#' @param ... Additional arguments (ignored)
#'
#' @return A named numeric vector with:
#'   \itemize{
#'     \item Threshold coefficients (e.g., "1|2", "2|3", ...)
#'     \item Regression coefficients (variable names from formula)
#'   }
#'
#' @examples
#' \dontrun{
#' fit <- clm_stan(rating ~ temp, data = wine)
#' coef(fit)
#' coef(fit, type = "median")
#' }
#'
#' @export
coef.clmstan <- function(object, type = c("mean", "median"), ...) {
  type <- match.arg(type)

  # Get threshold and coefficient names

  threshold_names <- get_threshold_names(object)
  coef_names <- if (object$P > 0) get_coef_names(object) else character(0)

  # Extract c_transformed (thresholds)
  c_draws <- object$fit$draws(variables = "c_transformed", format = "matrix")

  if (type == "mean") {
    c_estimates <- colMeans(c_draws)
  } else {
    c_estimates <- apply(c_draws, 2, stats::median)
  }
  names(c_estimates) <- threshold_names

  # Extract beta (coefficients) if P > 0
  if (object$P > 0) {
    beta_draws <- object$fit$draws(variables = "beta", format = "matrix")
    if (type == "mean") {
      beta_estimates <- colMeans(beta_draws)
    } else {
      beta_estimates <- apply(beta_draws, 2, stats::median)
    }
    names(beta_estimates) <- coef_names

    # Combine: thresholds first, then coefficients (like ordinal::clm)
    result <- c(c_estimates, beta_estimates)
  } else {
    result <- c_estimates
  }

  result
}

#' Plot method for clmstan objects
#'
#' Produces diagnostic plots using the bayesplot package.
#'
#' @param x A clmstan object
#' @param type Type of plot: "trace" (default), "dens", "hist", "areas",
#'   "intervals", or "acf" (autocorrelation).
#' @param pars Character vector of parameter names to plot. If NULL, plots
#'   beta, c_transformed (except first), and beta0.
#' @param ... Additional arguments passed to bayesplot functions.
#'   For "acf" type, you can use \code{lags} to control the number of lags.
#'
#' @return A ggplot object
#'
#' @examples
#' \dontrun{
#' fit <- clm_stan(rating ~ temp, data = wine)
#' plot(fit)                    # trace plots
#' plot(fit, type = "dens")     # density plots
#' plot(fit, type = "intervals") # credible intervals
#' plot(fit, type = "acf")      # autocorrelation plots
#' plot(fit, pars = "beta")     # only beta parameters
#' }
#'
#' @export
plot.clmstan <- function(x,
                         type = c("trace", "dens", "hist", "areas",
                                  "intervals", "acf"),
                         pars = NULL,
                         ...) {
  type <- match.arg(type)

  # Get draws for plotting
  draws <- get_clmstan_draws(x, pars)

  # Dispatch to bayesplot function
  plot_fn <- switch(
    type,
    "trace" = bayesplot::mcmc_trace,
    "dens" = bayesplot::mcmc_dens,
    "hist" = bayesplot::mcmc_hist,
    "areas" = bayesplot::mcmc_areas,
    "intervals" = bayesplot::mcmc_intervals,
    "acf" = bayesplot::mcmc_acf
  )

  plot_fn(draws, ...)
}

# =============================================================================
# predict() and fitted() methods are now in R/predict.R
# loo() and waic() methods are now in R/loo_waic.R
# =============================================================================

# =============================================================================
# Convergence diagnostics
# =============================================================================

#' MCMC Diagnostics for clmstan objects
#'
#' Provides a summary of MCMC convergence diagnostics including HMC-specific
#' diagnostics (divergences, treedepth, E-BFMI) and general convergence
#' measures (Rhat, ESS).
#'
#' @param object A clmstan object
#' @param detail Logical. If TRUE, show full parameter-level diagnostics table.
#'   If FALSE (default), show only summary and any problematic parameters.
#' @param rhat_threshold Threshold for flagging high Rhat values. Default 1.01.
#' @param ess_threshold Threshold for flagging low ESS values. Default 400.
#' @param ... Additional arguments (ignored)
#'
#' @return Invisibly returns a list containing:
#'   \itemize{
#'     \item hmc: HMC diagnostics from CmdStanMCMC$diagnostic_summary()
#'     \item convergence: Data frame of per-parameter Rhat and ESS values
#'     \item issues: Logical indicating whether any issues were detected
#'   }
#'
#' @details
#' The function checks for the following issues:
#' \itemize{
#'   \item \strong{Divergences}: Number of divergent transitions (ideally 0)
#'   \item \strong{Treedepth}: Transitions hitting max treedepth (efficiency issue)
#'   \item \strong{E-BFMI}: Energy Bayesian Fraction of Missing Information
#'     (values < 0.3 indicate problems)
#'   \item \strong{Rhat}: Potential scale reduction factor (values > 1.01
#'     indicate lack of convergence)
#'   \item \strong{ESS}: Effective sample size for bulk and tail
#'     (low values indicate high autocorrelation)
#' }
#'
#' @examples
#' \dontrun{
#' fit <- clm_stan(rating ~ temp, data = wine)
#' diagnostics(fit)
#' diagnostics(fit, detail = TRUE)
#' }
#'
#' @export
diagnostics <- function(object, ...) {
  UseMethod("diagnostics")
}

#' @rdname diagnostics
#' @export
diagnostics.clmstan <- function(object, detail = FALSE,
                                 rhat_threshold = 1.01,
                                 ess_threshold = 400, ...) {
  if (!inherits(object, "clmstan")) {
    stop("'object' must be a clmstan object")
  }

  fit <- object$fit
  if (is.null(fit)) {
    stop("No Stan fit object found")
  }

  # --- HMC diagnostics ---
  hmc_diag <- fit$diagnostic_summary()

  num_divergent <- sum(hmc_diag$num_divergent)
  num_max_treedepth <- sum(hmc_diag$num_max_treedepth)
  ebfmi_values <- hmc_diag$ebfmi
  ebfmi_low <- any(ebfmi_values < 0.3)

  # --- Convergence diagnostics (Rhat, ESS) ---
  # Get all parameters (excluding internal ones)
  all_pars <- c()
  if (object$P > 0) all_pars <- c(all_pars, "beta")
  all_pars <- c(all_pars, "c_transformed", "beta0")

  draws <- fit$draws(variables = all_pars, format = "draws_df")
  conv_summary <- posterior::summarise_draws(
    draws,
    rhat = posterior::rhat,
    ess_bulk = posterior::ess_bulk,
    ess_tail = posterior::ess_tail
  )

  # Identify problematic parameters
  rhat_issues <- conv_summary$rhat > rhat_threshold
  ess_bulk_issues <- conv_summary$ess_bulk < ess_threshold
  ess_tail_issues <- conv_summary$ess_tail < ess_threshold

  has_issues <- num_divergent > 0 || num_max_treedepth > 0 || ebfmi_low ||
    any(rhat_issues, na.rm = TRUE) ||
    any(ess_bulk_issues, na.rm = TRUE) ||
    any(ess_tail_issues, na.rm = TRUE)

  # --- Print output ---
  if (has_issues) {
    cat("MCMC Diagnostics: Issues detected\n")
  } else {
    cat("MCMC Diagnostics: All OK\n")
  }
  cat(strrep("=", 40), "\n\n")

  # HMC section
  cat("HMC Diagnostics:\n")
  if (num_divergent > 0) {
    cat(sprintf("  Divergences: %d (WARNING!)\n", num_divergent))
  } else {
    cat("  Divergences: 0 (OK)\n")
  }

  if (num_max_treedepth > 0) {
    cat(sprintf("  Max treedepth: %d transitions hit limit\n", num_max_treedepth))
  } else {
    cat("  Treedepth: OK\n")
  }

  if (ebfmi_low) {
    low_chains <- which(ebfmi_values < 0.3)
    cat(sprintf("  E-BFMI: Low in chain(s) %s (WARNING!)\n",
                paste(low_chains, collapse = ", ")))
  } else {
    cat("  E-BFMI: OK\n")
  }
  cat("\n")

  # Convergence section
  cat("Convergence Diagnostics:\n")
  max_rhat <- max(conv_summary$rhat, na.rm = TRUE)
  min_ess_bulk <- min(conv_summary$ess_bulk, na.rm = TRUE)
  min_ess_tail <- min(conv_summary$ess_tail, na.rm = TRUE)

  problem_params <- which(rhat_issues | ess_bulk_issues | ess_tail_issues)

  if (length(problem_params) == 0) {
    cat(sprintf("  All Rhat < %.2f (max: %.3f)\n", rhat_threshold, max_rhat))
    cat(sprintf("  All ESS > %d (min bulk: %.0f, min tail: %.0f)\n",
                ess_threshold, min_ess_bulk, min_ess_tail))
  } else {
    cat(sprintf("  Max Rhat: %.3f%s\n", max_rhat,
                if (max_rhat > rhat_threshold) " (WARNING!)" else ""))
    cat(sprintf("  Min ESS bulk: %.0f%s\n", min_ess_bulk,
                if (min_ess_bulk < ess_threshold) " (WARNING!)" else ""))
    cat(sprintf("  Min ESS tail: %.0f%s\n", min_ess_tail,
                if (min_ess_tail < ess_threshold) " (WARNING!)" else ""))
    cat("\n  Problematic parameters:\n")

    # Show problematic parameters table
    prob_df <- conv_summary[problem_params, ]
    prob_df$rhat <- round(prob_df$rhat, 3)
    prob_df$ess_bulk <- round(prob_df$ess_bulk, 0)
    prob_df$ess_tail <- round(prob_df$ess_tail, 0)
    print(prob_df, row.names = FALSE)
  }

  # Detail mode: show all parameters
  if (detail) {
    cat("\nFull parameter diagnostics:\n")
    cat(strrep("-", 40), "\n")
    detail_df <- conv_summary
    detail_df$rhat <- round(detail_df$rhat, 3)
    detail_df$ess_bulk <- round(detail_df$ess_bulk, 0)
    detail_df$ess_tail <- round(detail_df$ess_tail, 0)
    print(detail_df, row.names = FALSE)
  }

  # Return invisibly
  invisible(list(
    hmc = hmc_diag,
    convergence = conv_summary,
    issues = has_issues
  ))
}

#' Extract ACF values from clmstan object
#'
#' Computes autocorrelation function (ACF) values for MCMC chains and returns
#' them in a tidy data frame format.
#'
#' @param object A clmstan object
#' @param pars Character vector of parameter names. If NULL (default), uses
#'   beta, c_transformed (except first), and beta0.
#' @param lags Maximum number of lags to compute. Default is 20.
#' @param ... Additional arguments (ignored)
#'
#' @return A data frame with columns:
#'   \itemize{
#'     \item parameter: Parameter name
#'     \item chain: Chain number
#'     \item lag: Lag value (0, 1, 2, ...)
#'     \item acf: Autocorrelation value
#'   }
#'
#' @details
#' The ACF measures how correlated each draw is with previous draws in the
#' same chain. High autocorrelation at many lags indicates slow mixing and
#' the need for more samples or reparameterization.
#'
#' Ideally, ACF should drop to near zero within a few lags. Persistent
#' high autocorrelation suggests the sampler is exploring the posterior
#' slowly.
#'
#' @examples
#' \dontrun{
#' fit <- clm_stan(rating ~ temp, data = wine)
#' acf_df <- extract_acf(fit)
#' head(acf_df)
#'
#' # Plot ACF for specific parameters
#' library(ggplot2)
#' acf_df |>
#'   dplyr::filter(parameter == "beta[1]") |>
#'   ggplot(aes(x = lag, y = acf, color = factor(chain))) +
#'   geom_line() +
#'   geom_hline(yintercept = 0, linetype = "dashed")
#' }
#'
#' @export
extract_acf <- function(object, pars = NULL, lags = 20, ...) {
  if (!inherits(object, "clmstan")) {
    stop("'object' must be a clmstan object")
  }

  # Get draws
  draws <- get_clmstan_draws(object, pars)

  # Get dimensions
  n_iter <- dim(draws)[1]
  n_chains <- dim(draws)[2]
  par_names <- dimnames(draws)[[3]]

  # Compute ACF for each parameter and chain
  result_list <- list()


  for (par in par_names) {
    for (chain in seq_len(n_chains)) {
      chain_draws <- as.numeric(draws[, chain, par])

      # Compute ACF
      acf_result <- stats::acf(chain_draws, lag.max = lags, plot = FALSE)

      # Create data frame for this parameter/chain
      df <- data.frame(
        parameter = par,
        chain = chain,
        lag = 0:lags,
        acf = as.numeric(acf_result$acf)
      )

      result_list[[length(result_list) + 1]] <- df
    }
  }

  # Combine all results
  do.call(rbind, result_list)
}
