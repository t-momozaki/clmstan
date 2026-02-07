# =============================================================================
# Prior Distribution Type Constants
# =============================================================================

# Prior distribution type constants (matches Stan integer codes)
# Used by map_class_to_params() and prepare_stan_data_*() functions
PRIOR_TYPES <- list(
  normal = 1L,
  student_t = 2L,
  cauchy = 3L,
  flat = 4L
)

# =============================================================================
# Legacy Prior API (clm_prior)
# =============================================================================

#' Prior Specification for clmstan
#'
#' @description
#' Create prior specifications for cumulative link models in clmstan.
#'
#' **Default priors:**
#' * Regression coefficients (beta): `normal(0, 2.5)`
#' * Cutpoints (c): `normal(0, 10)` for flexible, `normal(0, 5)` for symmetric
#' * Interval (d): `gamma(2, 0.5)` for equidistant threshold
#'
#' **Link parameter priors (when estimated):**
#' | Link | Parameter | Default Prior |
#' |------|-----------|---------------|
#' | tlink | df | gamma(2, 0.1) |
#' | aranda_ordaz | lambda | gamma(0.5, 0.5) |
#' | gev | xi | normal(0, 2) |
#' | sp | r | gamma(0.5, 0.5) |
#' | log_gamma | lambda | normal(0, 1) |
#' | aep | theta1, theta2 | gamma(2, 1) |
#'
#' @param beta_sd SD for normal prior on regression coefficients.
#'   Default: 2.5 (weakly informative)
#' @param c_sd SD for normal prior on cutpoints (flexible threshold).
#'   Default: 10
#' @param c1_mu Mean for normal prior on first cutpoint (equidistant threshold).
#'   Default: 0
#' @param c1_sd SD for normal prior on first cutpoint (equidistant threshold).
#'   Default: 10
#' @param d_alpha Gamma shape for interval d (equidistant threshold).
#'   Default: 2
#' @param d_beta Gamma rate for interval d (equidistant threshold).
#'   Default: 0.5
#' @param cpos_sd SD for half-normal prior on positive cutpoints (symmetric threshold).
#'   Default: 5
#' @param df_alpha Gamma shape for tlink df. Default: 2
#' @param df_beta Gamma rate for tlink df. Default: 0.1
#' @param lambda_ao_alpha Gamma shape for aranda_ordaz lambda. Default: 0.5
#' @param lambda_ao_beta Gamma rate for aranda_ordaz lambda. Default: 0.5
#' @param lambda_lg_mu Normal mean for log_gamma lambda. Default: 0
#' @param lambda_lg_sd Normal SD for log_gamma lambda. Default: 1
#' @param xi_mu Normal mean for GEV xi. Default: 0
#' @param xi_sd Normal SD for GEV xi. Default: 2
#' @param r_alpha Gamma shape for SP r. Default: 0.5
#' @param r_beta Gamma rate for SP r. Default: 0.5
#' @param theta1_alpha Gamma shape for AEP theta1. Default: 2
#' @param theta1_beta Gamma rate for AEP theta1. Default: 1
#' @param theta2_alpha Gamma shape for AEP theta2. Default: 2
#' @param theta2_beta Gamma rate for AEP theta2. Default: 1
#'
#' @return An object of class `"clm_prior"` containing prior specifications.
#'
#' @export
#'
#' @examples
#' # Create a prior object (does not require Stan)
#' my_prior <- clm_prior(beta_sd = 2, c_sd = 5)
#' print(my_prior)
#'
#' \dontrun{
#' # Examples below require CmdStan and compiled Stan models
#' data(wine, package = "ordinal")
#'
#' # Default priors (no customization needed)
#' fit <- clm_stan(rating ~ temp, data = wine,
#'                 chains = 2, iter = 500, warmup = 250, refresh = 0)
#'
#' # Custom prior for regression coefficients
#' fit2 <- clm_stan(rating ~ temp, data = wine,
#'                  prior = clm_prior(beta_sd = 1),
#'                  chains = 2, iter = 500, warmup = 250, refresh = 0)
#' }
clm_prior <- function(
    # Basic priors
    beta_sd = NULL,
    c_sd = NULL,
    # Equidistant threshold priors
    c1_mu = NULL,
    c1_sd = NULL,
    d_alpha = NULL,
    d_beta = NULL,
    # Symmetric threshold priors
    cpos_sd = NULL,
    # Link parameter priors (tlink)
    df_alpha = NULL,
    df_beta = NULL,
    # Link parameter priors (aranda_ordaz)
    lambda_ao_alpha = NULL,
    lambda_ao_beta = NULL,
    # Link parameter priors (log_gamma)
    lambda_lg_mu = NULL,
    lambda_lg_sd = NULL,
    # Link parameter priors (gev)
    xi_mu = NULL,
    xi_sd = NULL,
    # Link parameter priors (sp)
    r_alpha = NULL,
    r_beta = NULL,
    # Link parameter priors (aep)
    theta1_alpha = NULL,
    theta1_beta = NULL,
    theta2_alpha = NULL,
    theta2_beta = NULL
) {
  # Collect all non-NULL arguments
  args <- list(
    beta_sd = beta_sd,
    c_sd = c_sd,
    c1_mu = c1_mu,
    c1_sd = c1_sd,
    d_alpha = d_alpha,
    d_beta = d_beta,
    cpos_sd = cpos_sd,
    df_alpha = df_alpha,
    df_beta = df_beta,
    lambda_ao_alpha = lambda_ao_alpha,
    lambda_ao_beta = lambda_ao_beta,
    lambda_lg_mu = lambda_lg_mu,
    lambda_lg_sd = lambda_lg_sd,
    xi_mu = xi_mu,
    xi_sd = xi_sd,
    r_alpha = r_alpha,
    r_beta = r_beta,
    theta1_alpha = theta1_alpha,
    theta1_beta = theta1_beta,
    theta2_alpha = theta2_alpha,
    theta2_beta = theta2_beta
  )

  # Remove NULL values
  prior <- args[!vapply(args, is.null, logical(1))]

  # Validate values
  validate_prior_values(prior)

  # Return clm_prior object
  structure(prior, class = "clm_prior")
}

#' Print method for clm_prior objects
#'
#' @param x A clm_prior object
#' @param ... Additional arguments (ignored)
#'
#' @return Invisibly returns the input \code{clm_prior} object.
#'
#' @export
print.clm_prior <- function(x, ...) {
  cat("clmstan prior specification:\n")

  if (length(x) == 0) {
    cat("  (using all defaults)\n")
    return(invisible(x))
  }

  # Group by parameter type
  beta_params <- c("beta_sd")
  c_flexible_params <- c("c_sd")
  c_equidistant_params <- c("c1_mu", "c1_sd", "d_alpha", "d_beta")
  c_symmetric_params <- c("cpos_sd")
  link_params <- c("df_alpha", "df_beta", "lambda_ao_alpha", "lambda_ao_beta",
                   "lambda_lg_mu", "lambda_lg_sd", "xi_mu", "xi_sd",
                   "r_alpha", "r_beta", "theta1_alpha", "theta1_beta",
                   "theta2_alpha", "theta2_beta")

  # Print beta priors
  beta_set <- names(x)[names(x) %in% beta_params]
  if (length(beta_set) > 0) {
    cat("  Regression coefficients (beta):\n")
    for (p in beta_set) {
      cat(sprintf("    %s = %s\n", p, x[[p]]))
    }
  }

  # Print cutpoint priors (flexible)
  c_flex_set <- names(x)[names(x) %in% c_flexible_params]
  if (length(c_flex_set) > 0) {
    cat("  Cutpoints (flexible):\n")
    for (p in c_flex_set) {
      cat(sprintf("    %s = %s\n", p, x[[p]]))
    }
  }

  # Print cutpoint priors (equidistant)
  c_equi_set <- names(x)[names(x) %in% c_equidistant_params]
  if (length(c_equi_set) > 0) {
    cat("  Cutpoints (equidistant):\n")
    for (p in c_equi_set) {
      cat(sprintf("    %s = %s\n", p, x[[p]]))
    }
  }

  # Print cutpoint priors (symmetric)
  c_sym_set <- names(x)[names(x) %in% c_symmetric_params]
  if (length(c_sym_set) > 0) {
    cat("  Cutpoints (symmetric):\n")
    for (p in c_sym_set) {
      cat(sprintf("    %s = %s\n", p, x[[p]]))
    }
  }

  # Print link parameter priors
  link_set <- names(x)[names(x) %in% link_params]
  if (length(link_set) > 0) {
    cat("  Link parameters:\n")
    for (p in link_set) {
      cat(sprintf("    %s = %s\n", p, x[[p]]))
    }
  }

  invisible(x)
}

#' Validate prior values
#'
#' @param prior A list of prior values
#' @keywords internal
validate_prior_values <- function(prior) {
  # SD and scale parameters must be positive
  positive_params <- c("beta_sd", "c_sd", "c1_sd", "cpos_sd",
                       "d_alpha", "d_beta",
                       "df_alpha", "df_beta",
                       "lambda_ao_alpha", "lambda_ao_beta",
                       "lambda_lg_sd",
                       "xi_sd",
                       "r_alpha", "r_beta",
                       "theta1_alpha", "theta1_beta",
                       "theta2_alpha", "theta2_beta")

  for (param in names(prior)) {
    value <- prior[[param]]

    if (!is.numeric(value) || length(value) != 1) {
      stop(sprintf("Prior parameter '%s' must be a single numeric value.", param))
    }

    if (param %in% positive_params && value <= 0) {
      stop(sprintf("Prior parameter '%s' must be positive (got %s).", param, value))
    }
  }

  invisible(TRUE)
}

#' Get default priors for a threshold structure
#'
#' @param threshold A character string specifying the threshold structure
#'   ("flexible", "equidistant", or "symmetric")
#'
#' @return A list with default prior values
#' @keywords internal
get_default_priors <- function(threshold = "flexible") {
  # Common defaults - beta prior with full parameters
  defaults <- list(
    beta_type = PRIOR_TYPES$normal,
    beta_mu = 0,
    beta_sd = 2.5,
    beta_df = 7  # placeholder (unused for normal)
  )

  # Threshold-specific defaults
  if (threshold == "flexible") {
    defaults$c_type <- PRIOR_TYPES$normal
    defaults$c_mu <- 0
    defaults$c_sd <- 10
    defaults$c_df <- 7  # placeholder (unused for normal)
  } else if (threshold == "equidistant") {
    defaults$c1_type <- PRIOR_TYPES$normal
    defaults$c1_mu <- 0
    defaults$c1_sd <- 10
    defaults$c1_df <- 7  # placeholder (unused for normal)
    defaults$d_alpha <- 2
    defaults$d_beta <- 0.5
  } else if (threshold == "symmetric") {
    defaults$cpos_type <- PRIOR_TYPES$normal
    defaults$cpos_sd <- 5
    defaults$cpos_df <- 7  # placeholder (unused for normal)
  }

  # Link parameter defaults (always included for full model)
  defaults$df_alpha <- 2
  defaults$df_beta <- 0.1
  defaults$lambda_ao_alpha <- 0.5
  defaults$lambda_ao_beta <- 0.5
  defaults$lambda_lg_mu <- 0
  defaults$lambda_lg_sd <- 1
  defaults$xi_mu <- 0
  defaults$xi_sd <- 2
  defaults$r_alpha <- 0.5
  defaults$r_beta <- 0.5
  defaults$theta1_alpha <- 2
  defaults$theta1_beta <- 1
  defaults$theta2_alpha <- 2
  defaults$theta2_beta <- 1

  defaults
}

#' Apply user priors to Stan data
#'
#' Merges user-specified priors with default values and updates the Stan data list.
#'
#' @param stan_data A list of Stan data prepared by prepare_stan_data_*()
#' @param prior A clm_prior object or NULL
#' @param threshold The threshold structure ("flexible", "equidistant", "symmetric")
#' @param full Whether this is a full model (with link parameter estimation)
#'
#' @return Updated stan_data list with prior values
#' @keywords internal
apply_priors_to_stan_data <- function(stan_data, prior, threshold, full = FALSE) {
  # Get defaults
  defaults <- get_default_priors(threshold)

  # If no user prior, just ensure all required fields are present
  if (is.null(prior)) {
    prior <- list()
  }

  # Merge user values with defaults (user values take precedence)
  merged <- defaults
  for (name in names(prior)) {
    if (name %in% names(merged)) {
      merged[[name]] <- prior[[name]]
    }
  }

  # Map prior values to Stan data field names
  # Basic priors - beta (type, mu, sd, df)
  if ("prior_beta_type" %in% names(stan_data)) {
    stan_data$prior_beta_type <- merged$beta_type
  }
  if ("prior_beta_mu" %in% names(stan_data)) {
    stan_data$prior_beta_mu <- merged$beta_mu
  }
  if ("prior_beta_sd" %in% names(stan_data)) {
    stan_data$prior_beta_sd <- merged$beta_sd
  }
  if ("prior_beta_df" %in% names(stan_data)) {
    stan_data$prior_beta_df <- merged$beta_df
  }

  # Flexible threshold (type, mu, sd, df)
  if ("prior_c_type" %in% names(stan_data)) {
    stan_data$prior_c_type <- merged$c_type
  }
  if ("prior_c_mu" %in% names(stan_data)) {
    stan_data$prior_c_mu <- merged$c_mu
  }
  if ("prior_c_sd" %in% names(stan_data)) {
    stan_data$prior_c_sd <- merged$c_sd
  }
  if ("prior_c_df" %in% names(stan_data)) {
    stan_data$prior_c_df <- merged$c_df
  }

  # Equidistant threshold (c1: type, mu, sd, df; d: alpha, beta)
  if ("prior_c1_type" %in% names(stan_data)) {
    stan_data$prior_c1_type <- merged$c1_type
  }
  if ("prior_c1_mu" %in% names(stan_data)) {
    stan_data$prior_c1_mu <- merged$c1_mu
  }
  if ("prior_c1_sd" %in% names(stan_data)) {
    stan_data$prior_c1_sd <- merged$c1_sd
  }
  if ("prior_c1_df" %in% names(stan_data)) {
    stan_data$prior_c1_df <- merged$c1_df
  }
  if ("prior_d_alpha" %in% names(stan_data)) {
    stan_data$prior_d_alpha <- merged$d_alpha
  }
  if ("prior_d_beta" %in% names(stan_data)) {
    stan_data$prior_d_beta <- merged$d_beta
  }

  # Symmetric threshold (cpos: type, sd, df)
  if ("prior_cpos_type" %in% names(stan_data)) {
    stan_data$prior_cpos_type <- merged$cpos_type
  }
  if ("prior_cpos_sd" %in% names(stan_data)) {
    stan_data$prior_cpos_sd <- merged$cpos_sd
  }
  if ("prior_cpos_df" %in% names(stan_data)) {
    stan_data$prior_cpos_df <- merged$cpos_df
  }

  # Link parameter priors (only for full model)
  if (full) {
    if ("prior_df_alpha" %in% names(stan_data)) {
      stan_data$prior_df_alpha <- merged$df_alpha
      stan_data$prior_df_beta <- merged$df_beta
    }
    if ("prior_lambda_ao_alpha" %in% names(stan_data)) {
      stan_data$prior_lambda_ao_alpha <- merged$lambda_ao_alpha
      stan_data$prior_lambda_ao_beta <- merged$lambda_ao_beta
    }
    if ("prior_lambda_lg_mu" %in% names(stan_data)) {
      stan_data$prior_lambda_lg_mu <- merged$lambda_lg_mu
      stan_data$prior_lambda_lg_sd <- merged$lambda_lg_sd
    }
    if ("prior_xi_mu" %in% names(stan_data)) {
      stan_data$prior_xi_mu <- merged$xi_mu
      stan_data$prior_xi_sd <- merged$xi_sd
    }
    if ("prior_r_alpha" %in% names(stan_data)) {
      stan_data$prior_r_alpha <- merged$r_alpha
      stan_data$prior_r_beta <- merged$r_beta
    }
    if ("prior_theta1_alpha" %in% names(stan_data)) {
      stan_data$prior_theta1_alpha <- merged$theta1_alpha
      stan_data$prior_theta1_beta <- merged$theta1_beta
    }
    if ("prior_theta2_alpha" %in% names(stan_data)) {
      stan_data$prior_theta2_alpha <- merged$theta2_alpha
      stan_data$prior_theta2_beta <- merged$theta2_beta
    }
  }

  stan_data
}

#' Validate prior specification for a model
#'
#' Checks that the prior specification is compatible with the model settings.
#'
#' @param prior A clm_prior object or NULL
#' @param threshold The threshold structure
#' @param link The link function
#' @param link_param The link parameters
#'
#' @return TRUE if valid, otherwise throws a warning
#' @keywords internal
validate_prior <- function(prior, threshold, link, link_param) {
  if (is.null(prior) || length(prior) == 0) {
    return(invisible(TRUE))
  }

  # Check for incompatible threshold priors
  if (threshold == "flexible") {
    equi_params <- c("c1_mu", "c1_sd", "d_alpha", "d_beta")
    sym_params <- c("cpos_sd")
    used_equi <- names(prior)[names(prior) %in% equi_params]
    used_sym <- names(prior)[names(prior) %in% sym_params]

    if (length(used_equi) > 0) {
      warning(sprintf("Prior parameters %s are for equidistant threshold but threshold='flexible'. These will be ignored.",
                      paste(used_equi, collapse = ", ")))
    }
    if (length(used_sym) > 0) {
      warning(sprintf("Prior parameters %s are for symmetric threshold but threshold='flexible'. These will be ignored.",
                      paste(used_sym, collapse = ", ")))
    }
  } else if (threshold == "equidistant") {
    flex_params <- c("c_sd")
    sym_params <- c("cpos_sd")
    used_flex <- names(prior)[names(prior) %in% flex_params]
    used_sym <- names(prior)[names(prior) %in% sym_params]

    if (length(used_flex) > 0) {
      warning(sprintf("Prior parameter %s is for flexible threshold but threshold='equidistant'. This will be ignored.",
                      paste(used_flex, collapse = ", ")))
    }
    if (length(used_sym) > 0) {
      warning(sprintf("Prior parameters %s are for symmetric threshold but threshold='equidistant'. These will be ignored.",
                      paste(used_sym, collapse = ", ")))
    }
  } else if (threshold == "symmetric") {
    flex_params <- c("c_sd")
    equi_params <- c("c1_mu", "c1_sd", "d_alpha", "d_beta")
    used_flex <- names(prior)[names(prior) %in% flex_params]
    used_equi <- names(prior)[names(prior) %in% equi_params]

    if (length(used_flex) > 0) {
      warning(sprintf("Prior parameter %s is for flexible threshold but threshold='symmetric'. This will be ignored.",
                      paste(used_flex, collapse = ", ")))
    }
    if (length(used_equi) > 0) {
      warning(sprintf("Prior parameters %s are for equidistant threshold but threshold='symmetric'. These will be ignored.",
                      paste(used_equi, collapse = ", ")))
    }
  }

  # Check for link parameter priors without estimation
  link_prior_params <- list(
    tlink = c("df_alpha", "df_beta"),
    aranda_ordaz = c("lambda_ao_alpha", "lambda_ao_beta"),
    log_gamma = c("lambda_lg_mu", "lambda_lg_sd"),
    gev = c("xi_mu", "xi_sd"),
    sp = c("r_alpha", "r_beta"),
    aep = c("theta1_alpha", "theta1_beta", "theta2_alpha", "theta2_beta")
  )

  # Check if link parameter priors are set but not being estimated
  if (!is.null(link_prior_params[[link]])) {
    relevant_params <- link_prior_params[[link]]
    used_params <- names(prior)[names(prior) %in% relevant_params]

    if (length(used_params) > 0) {
      # Check if corresponding parameter is being estimated
      estimating <- !is.null(link_param) && any(vapply(link_param, function(v) {
        is.character(v) && v == "estimate"
      }, logical(1)))

      if (!estimating) {
        warning(sprintf("Prior parameters %s are set but link parameters are not being estimated. ",
                        paste(used_params, collapse = ", ")),
                "Use link_param = list(...= 'estimate') to enable estimation.")
      }
    }
  }

  invisible(TRUE)
}

# =============================================================================
# Distribution-based Prior API
# =============================================================================

#' Normal Distribution for Prior Specification
#'
#' Creates a normal distribution object for use with [prior()].
#'
#' @param mu Mean of the normal distribution. Default: 0
#' @param sigma Standard deviation of the normal distribution. Must be positive. Default: 1
#'
#' @return An object of class `"clm_dist"` representing a normal distribution.
#'
#' @seealso [prior()], [gamma()], [student_t()], [cauchy()]
#'
#' @export
#'
#' @examples
#' # Create a normal prior
#' normal(0, 2.5)
#'
#' # Use with prior()
#' prior(normal(0, 2.5), class = "b")
normal <- function(mu = 0, sigma = 1) {
  if (!is.numeric(mu) || length(mu) != 1) {
    stop("'mu' must be a single numeric value")
  }
  if (!is.numeric(sigma) || length(sigma) != 1 || sigma <= 0) {
    stop("'sigma' must be a single positive numeric value")
  }
  structure(
    list(dist = "normal", mu = mu, sigma = sigma),
    class = "clm_dist"
  )
}

#' Gamma Distribution for Prior Specification
#'
#' Creates a gamma distribution object for use with [prior()].
#'
#' @param alpha Shape parameter of the gamma distribution. Must be positive.
#' @param beta Rate parameter of the gamma distribution. Must be positive.
#'
#' @return An object of class `"clm_dist"` representing a gamma distribution.
#'
#' @note This function masks `base::gamma()`. To use the base gamma function,
#'   use `base::gamma()` explicitly.
#'
#' @seealso [prior()], [normal()], [student_t()], [cauchy()]
#'
#' @export
#'
#' @examples
#' # Create a gamma prior
#' gamma(2, 0.1)
#'
#' # Use with prior() for degrees of freedom
#' prior(gamma(2, 0.1), class = "df")
gamma <- function(alpha, beta) {
  if (!is.numeric(alpha) || length(alpha) != 1 || alpha <= 0) {
    stop("'alpha' must be a single positive numeric value")
  }

  if (!is.numeric(beta) || length(beta) != 1 || beta <= 0) {
    stop("'beta' must be a single positive numeric value")
  }
  structure(
    list(dist = "gamma", alpha = alpha, beta = beta),
    class = "clm_dist"
  )
}

#' Student-t Distribution for Prior Specification
#'
#' Creates a Student-t distribution object for use with [prior()].
#'
#' @param df Degrees of freedom. Must be positive. Default: 3
#' @param mu Location parameter. Default: 0
#' @param sigma Scale parameter. Must be positive. Default: 1
#'
#' @return An object of class `"clm_dist"` representing a Student-t distribution.
#'
#' @seealso [prior()], [normal()], [gamma()], [cauchy()]
#'
#' @export
#'
#' @examples
#' # Create a Student-t prior with heavy tails
#' student_t(3, 0, 2.5)
#'
#' # Use with prior()
#' prior(student_t(3, 0, 2.5), class = "b")
student_t <- function(df = 3, mu = 0, sigma = 1) {
  if (!is.numeric(df) || length(df) != 1 || df <= 0) {
    stop("'df' must be a single positive numeric value")
  }
  if (!is.numeric(mu) || length(mu) != 1) {
    stop("'mu' must be a single numeric value")
  }
  if (!is.numeric(sigma) || length(sigma) != 1 || sigma <= 0) {
    stop("'sigma' must be a single positive numeric value")
  }
  structure(
    list(dist = "student_t", df = df, mu = mu, sigma = sigma),
    class = "clm_dist"
  )
}

#' Cauchy Distribution for Prior Specification
#'
#' Creates a Cauchy distribution object for use with [prior()].
#'
#' @param mu Location parameter. Default: 0
#' @param sigma Scale parameter. Must be positive. Default: 1
#'
#' @return An object of class `"clm_dist"` representing a Cauchy distribution.
#'
#' @seealso [prior()], [normal()], [gamma()], [student_t()]
#'
#' @export
#'
#' @examples
#' # Create a Cauchy prior (weakly informative)
#' cauchy(0, 2.5)
#'
#' # Use with prior()
#' prior(cauchy(0, 2.5), class = "b")
cauchy <- function(mu = 0, sigma = 1) {
  if (!is.numeric(mu) || length(mu) != 1) {
    stop("'mu' must be a single numeric value")
  }
  if (!is.numeric(sigma) || length(sigma) != 1 || sigma <= 0) {
    stop("'sigma' must be a single positive numeric value")
  }
  structure(
    list(dist = "cauchy", mu = mu, sigma = sigma),
    class = "clm_dist"
  )
}

#' Flat (Improper Uniform) Prior Distribution
#'
#' Creates a flat (improper uniform) distribution object for use with [prior()].
#' A flat prior assigns equal probability density to all values, which is
#' improper (does not integrate to 1) but can be used when the likelihood
#' provides sufficient information for identification.
#'
#' @return An object of class `"clm_dist"` representing a flat distribution.
#'
#' @note Flat priors are supported for:
#'   \itemize{
#'     \item Regression coefficients (class "b")
#'     \item Threshold classes ("Intercept", "c1", "cpos")
#'   }
#'   Using flat priors may lead to improper posteriors if the likelihood
#'   does not provide sufficient information. For thresholds with ordered
#'   constraints, Stan's internal transformation provides implicit regularization.
#'
#' @seealso [prior()], [normal()], [student_t()], [cauchy()]
#'
#' @export
#'
#' @examples
#' # Create a flat prior for regression coefficients
#' prior(flat(), class = "b")
#'
#' # Flat prior for thresholds (flexible)
#' prior(flat(), class = "Intercept")
flat <- function() {
  structure(
    list(dist = "flat"),
    class = "clm_dist"
  )
}

#' Print method for clm_dist objects
#'
#' @param x A clm_dist object
#' @param ... Additional arguments (ignored)
#'
#' @return Invisibly returns the input \code{clm_dist} object.
#'
#' @export
print.clm_dist <- function(x, ...) {
  cat(format_dist(x), "\n")
  invisible(x)
}

#' Format a distribution object as a string
#'
#' @param x A clm_dist object
#' @return A character string representation
#' @keywords internal
format_dist <- function(x) {
  switch(x$dist,
    "normal" = sprintf("normal(%s, %s)", x$mu, x$sigma),
    "gamma" = sprintf("gamma(%s, %s)", x$alpha, x$beta),
    "student_t" = sprintf("student_t(%s, %s, %s)", x$df, x$mu, x$sigma),
    "cauchy" = sprintf("cauchy(%s, %s)", x$mu, x$sigma),
    "flat" = "flat()",
    paste0(x$dist, "(...)")
  )
}

#' Specify Prior Distributions
#'
#' Specify prior distributions for model parameters using distribution functions.
#'
#' @param prior A distribution object created by [normal()], [gamma()],
#'   [student_t()], or [cauchy()].
#' @param class The parameter class. Valid classes are:
#'   * `"b"`: Regression coefficients (beta)
#'   * `"Intercept"`: Cutpoints/thresholds (flexible)
#'   * `"c1"`: First cutpoint (equidistant)
#'   * `"d"`: Threshold interval (equidistant)
#'   * `"cpos"`: Positive cutpoints (symmetric)
#'   * `"df"`: Degrees of freedom (tlink)
#'   * `"lambda_ao"`: Lambda parameter (aranda_ordaz)
#'   * `"lambda_lg"`: Lambda parameter (log_gamma)
#'   * `"xi"`: Xi parameter (gev)
#'   * `"r"`: R parameter (sp)
#'   * `"theta1"`, `"theta2"`: Theta parameters (aep)
#' @param coef Optional coefficient name (for future extension).
#'
#' @return An object of class `"clm_prior_spec"` representing the prior specification.
#'
#' @seealso [normal()], [gamma()], [student_t()], [cauchy()], [clm_prior()]
#'
#' @export
#'
#' @examples
#' # Specify a normal prior for regression coefficients
#' prior(normal(0, 2.5), class = "b")
#'
#' # Specify a gamma prior for degrees of freedom
#' prior(gamma(2, 0.1), class = "df")
#'
#' # Combine multiple priors
#' c(
#'   prior(normal(0, 2.5), class = "b"),
#'   prior(normal(0, 10), class = "Intercept")
#' )
prior <- function(prior, class = "b", coef = "") {
  # Validate distribution object
  if (!inherits(prior, "clm_dist")) {
    stop("'prior' must be a distribution object (normal, gamma, student_t, or cauchy)")
  }

  # Define valid parameter classes
  valid_classes <- c("b", "Intercept", "c1", "d", "cpos",
                     "df", "lambda_ao", "lambda_lg", "xi", "r",
                     "theta1", "theta2")
  if (!class %in% valid_classes) {
    stop("Invalid class '", class, "'. Valid classes: ",
         paste(valid_classes, collapse = ", "))
  }

  # Validate class-distribution compatibility
  validate_prior_class_dist(class, prior)

  structure(
    list(
      prior = prior,
      class = class,
      coef = coef
    ),
    class = "clm_prior_spec"
  )
}

#' Validate that the distribution is compatible with the parameter class
#'
#' @param class The parameter class
#' @param dist A clm_dist object
#' @keywords internal
validate_prior_class_dist <- function(class, dist) {
  # Classes that accept flat prior in addition to normal, student_t, cauchy:
  # - "b" (regression coefficients)
  # - "Intercept", "c1", "cpos" (threshold classes)
  flat_allowed_classes <- c("b", "Intercept", "c1", "cpos")

  if (class %in% flat_allowed_classes) {
    if (!dist$dist %in% c("normal", "student_t", "cauchy", "flat")) {
      stop(sprintf(
        "Class '%s' requires normal, student_t, cauchy, or flat distribution (got '%s')",
        class, dist$dist
      ))
    }
    return(invisible(TRUE))
  }

  # Location/scale parameters use symmetric distributions (normal, student_t, cauchy)
  # because they can take any real value (positive or negative).
  # These do NOT support flat priors.
  normal_classes <- c("lambda_lg", "xi")

  # Positive-only parameters require gamma distribution because they must be > 0.
  # This includes shape parameters (df, alpha, beta) and scale parameters (r, theta).
  gamma_classes <- c("d", "df", "lambda_ao", "r", "theta1", "theta2")

  if (class %in% normal_classes) {
    if (!dist$dist %in% c("normal", "student_t", "cauchy")) {
      stop(sprintf(
        "Class '%s' requires normal, student_t, or cauchy distribution (got '%s')",
        class, dist$dist
      ))
    }
  } else if (class %in% gamma_classes) {
    if (dist$dist != "gamma") {
      stop(sprintf(
        "Class '%s' requires gamma distribution (got '%s')",
        class, dist$dist
      ))
    }
  }

  invisible(TRUE)
}

#' Print method for clm_prior_spec objects
#'
#' @param x A clm_prior_spec object
#' @param ... Additional arguments (ignored)
#'
#' @return Invisibly returns the input \code{clm_prior_spec} object.
#'
#' @export
print.clm_prior_spec <- function(x, ...) {
  cat("Prior: ", format_dist(x$prior), "\n", sep = "")
  cat("Class: ", x$class, "\n", sep = "")
  if (nchar(x$coef) > 0) {
    cat("Coef:  ", x$coef, "\n", sep = "")
  }
  invisible(x)
}

#' Combine Multiple Prior Specifications
#'
#' Combines multiple [prior()] objects into a single prior specification list.
#'
#' @param ... [prior()] objects to combine.
#'
#' @return An object of class `"clm_prior_list"` containing all prior specifications.
#'
#' @export
#'
#' @examples
#' # Combine multiple priors
#' priors <- c(
#'   prior(normal(0, 2.5), class = "b"),
#'   prior(normal(0, 10), class = "Intercept"),
#'   prior(gamma(2, 0.1), class = "df")
#' )
#' print(priors)
c.clm_prior_spec <- function(...) {
  priors <- list(...)

  # Validate all arguments are clm_prior_spec objects
  for (i in seq_along(priors)) {
    if (!inherits(priors[[i]], "clm_prior_spec")) {
      stop("All arguments must be clm_prior_spec objects (created by prior())")
    }
  }

  structure(priors, class = c("clm_prior_list", "list"))
}

#' Print method for clm_prior_list objects
#'
#' @param x A clm_prior_list object
#' @param ... Additional arguments (ignored)
#'
#' @return Invisibly returns the input \code{clm_prior_list} object.
#'
#' @export
print.clm_prior_list <- function(x, ...) {
  cat("Prior specifications:\n")
  for (i in seq_along(x)) {
    cat("  ", i, ". ", x[[i]]$class, ": ",
        format_dist(x[[i]]$prior), "\n", sep = "")
  }
  invisible(x)
}

#' Convert Prior Specification to Internal Format
#'
#' Internal function to convert distribution-based prior specifications to
#' the internal format used by apply_priors_to_stan_data().
#'
#' @param prior A clm_prior_spec, clm_prior_list, or clm_prior object
#' @return A list with clm_prior class containing prior parameter values
#' @keywords internal
convert_prior_spec_to_legacy <- function(prior) {

  # Return if already in legacy format (clm_prior object)
  if (inherits(prior, "clm_prior")) {
    return(prior)
  }

  # Handle single prior spec
  if (inherits(prior, "clm_prior_spec")) {
    prior <- list(prior)
    class(prior) <- c("clm_prior_list", "list")
  }

  # Must be a list of prior specs
  if (!inherits(prior, "clm_prior_list")) {
    stop("Unknown prior format. Use clm_prior() or prior() to specify priors.")
  }

  # Build merged parameters list
  # Note: We create a clm_prior object directly instead of calling clm_prior()
  # because the distribution-based API supports parameters (beta_type, beta_mu,
  # beta_df) that are not in the legacy clm_prior() function signature.
  merged_params <- list()

  for (p in prior) {
    params <- map_class_to_params(p$class, p$prior)
    merged_params <- c(merged_params, params)
  }

  structure(merged_params, class = "clm_prior")
}

#' Map Parameter Class and Distribution to Legacy Parameter Names
#'
#' @param class The parameter class
#' @param dist A clm_dist object
#' @return A named list of legacy parameter values
#' @keywords internal
map_class_to_params <- function(class, dist) {
  switch(class,
    "b" = {
      # Map all distribution parameters for beta prior
      # Returns beta_type, beta_mu, beta_sd, beta_df for Stan
      if (dist$dist == "normal") {
        list(
          beta_type = PRIOR_TYPES$normal,
          beta_mu = dist$mu,
          beta_sd = dist$sigma,
          beta_df = 7  # placeholder (unused for normal)
        )
      } else if (dist$dist == "student_t") {
        list(
          beta_type = PRIOR_TYPES$student_t,
          beta_mu = dist$mu,
          beta_sd = dist$sigma,
          beta_df = dist$df
        )
      } else if (dist$dist == "cauchy") {
        list(
          beta_type = PRIOR_TYPES$cauchy,
          beta_mu = dist$mu,
          beta_sd = dist$sigma,
          beta_df = 7  # placeholder (unused for cauchy)
        )
      } else if (dist$dist == "flat") {
        list(
          beta_type = PRIOR_TYPES$flat,
          beta_mu = 0,  # placeholder (unused for flat)
          beta_sd = 1,  # placeholder (unused for flat)
          beta_df = 7   # placeholder (unused for flat)
        )
      } else {
        stop("Class 'b' requires normal, student_t, cauchy, or flat distribution")
      }
    },
    "Intercept" = {
      # Flexible threshold: returns c_type, c_mu, c_sd, c_df for Stan
      if (dist$dist == "normal") {
        list(
          c_type = PRIOR_TYPES$normal,
          c_mu = dist$mu,
          c_sd = dist$sigma,
          c_df = 7  # placeholder (unused for normal)
        )
      } else if (dist$dist == "student_t") {
        list(
          c_type = PRIOR_TYPES$student_t,
          c_mu = dist$mu,
          c_sd = dist$sigma,
          c_df = dist$df
        )
      } else if (dist$dist == "cauchy") {
        list(
          c_type = PRIOR_TYPES$cauchy,
          c_mu = dist$mu,
          c_sd = dist$sigma,
          c_df = 7  # placeholder (unused for cauchy)
        )
      } else if (dist$dist == "flat") {
        list(
          c_type = PRIOR_TYPES$flat,
          c_mu = 0,   # placeholder (unused for flat)
          c_sd = 1,   # placeholder (unused for flat)
          c_df = 7    # placeholder (unused for flat)
        )
      } else {
        stop("Class 'Intercept' requires normal, student_t, cauchy, or flat distribution")
      }
    },
    "c1" = {
      # Equidistant threshold c1: returns c1_type, c1_mu, c1_sd, c1_df for Stan
      if (dist$dist == "normal") {
        list(
          c1_type = PRIOR_TYPES$normal,
          c1_mu = dist$mu,
          c1_sd = dist$sigma,
          c1_df = 7  # placeholder (unused for normal)
        )
      } else if (dist$dist == "student_t") {
        list(
          c1_type = PRIOR_TYPES$student_t,
          c1_mu = dist$mu,
          c1_sd = dist$sigma,
          c1_df = dist$df
        )
      } else if (dist$dist == "cauchy") {
        list(
          c1_type = PRIOR_TYPES$cauchy,
          c1_mu = dist$mu,
          c1_sd = dist$sigma,
          c1_df = 7  # placeholder (unused for cauchy)
        )
      } else if (dist$dist == "flat") {
        list(
          c1_type = PRIOR_TYPES$flat,
          c1_mu = 0,   # placeholder (unused for flat)
          c1_sd = 1,   # placeholder (unused for flat)
          c1_df = 7    # placeholder (unused for flat)
        )
      } else {
        stop("Class 'c1' requires normal, student_t, cauchy, or flat distribution")
      }
    },
    "d" = {
      if (dist$dist != "gamma") {
        stop("Class 'd' requires gamma distribution")
      }
      list(d_alpha = dist$alpha, d_beta = dist$beta)
    },
    "cpos" = {
      # Symmetric threshold cpos: returns cpos_type, cpos_sd, cpos_df for Stan
      # Note: c_pos is positive_ordered, so acts as half-normal/half-t/half-cauchy
      if (dist$dist == "normal") {
        list(
          cpos_type = PRIOR_TYPES$normal,
          cpos_sd = dist$sigma,
          cpos_df = 7  # placeholder (unused for normal)
        )
      } else if (dist$dist == "student_t") {
        list(
          cpos_type = PRIOR_TYPES$student_t,
          cpos_sd = dist$sigma,
          cpos_df = dist$df
        )
      } else if (dist$dist == "cauchy") {
        list(
          cpos_type = PRIOR_TYPES$cauchy,
          cpos_sd = dist$sigma,
          cpos_df = 7  # placeholder (unused for cauchy)
        )
      } else if (dist$dist == "flat") {
        list(
          cpos_type = PRIOR_TYPES$flat,
          cpos_sd = 1,  # placeholder (unused for flat)
          cpos_df = 7   # placeholder (unused for flat)
        )
      } else {
        stop("Class 'cpos' requires normal, student_t, cauchy, or flat distribution")
      }
    },
    "df" = {
      if (dist$dist != "gamma") {
        stop("Class 'df' requires gamma distribution")
      }
      list(df_alpha = dist$alpha, df_beta = dist$beta)
    },
    "lambda_ao" = {
      if (dist$dist != "gamma") {
        stop("Class 'lambda_ao' requires gamma distribution")
      }
      list(lambda_ao_alpha = dist$alpha, lambda_ao_beta = dist$beta)
    },
    "lambda_lg" = {
      if (dist$dist == "normal") {
        list(lambda_lg_mu = dist$mu, lambda_lg_sd = dist$sigma)
      } else {
        stop("Class 'lambda_lg' requires normal distribution")
      }
    },
    "xi" = {
      if (dist$dist == "normal") {
        list(xi_mu = dist$mu, xi_sd = dist$sigma)
      } else {
        stop("Class 'xi' requires normal distribution")
      }
    },
    "r" = {
      if (dist$dist != "gamma") {
        stop("Class 'r' requires gamma distribution")
      }
      list(r_alpha = dist$alpha, r_beta = dist$beta)
    },
    "theta1" = {
      if (dist$dist != "gamma") {
        stop("Class 'theta1' requires gamma distribution")
      }
      list(theta1_alpha = dist$alpha, theta1_beta = dist$beta)
    },
    "theta2" = {
      if (dist$dist != "gamma") {
        stop("Class 'theta2' requires gamma distribution")
      }
      list(theta2_alpha = dist$alpha, theta2_beta = dist$beta)
    },
    stop("Unknown class: ", class)
  )
}
