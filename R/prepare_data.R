#' Prepare data for Stan model
#'
#' @param formula A formula specifying the model
#' @param data A data frame containing the variables
#' @param link Link function name
#' @param link_param A list of link parameters (for flexible links)
#' @param prior_beta_sd Prior SD for regression coefficients (default: 2.5)
#' @param prior_c_sd Prior SD for cutpoints (default: 10)
#'
#' @return A list suitable for passing to CmdStan
#' @keywords internal
prepare_stan_data <- function(formula, data, link = "logit",
                              link_param = NULL,
                              prior_beta_sd = 2.5,
                              prior_c_sd = 10) {
  # Parse formula and extract response/predictors
  parsed <- parse_clm_formula(formula, data)

  # Create design matrix (without intercept, as beta0 is separate)
  X <- make_design_matrix(parsed$predictor_formula, data)

  # Get response variable
  y <- parsed$response
  if (!is.factor(y)) {
    y <- factor(y)
  }
  y_int <- as.integer(y)
  K <- nlevels(y)

  if (K < 2) {
    stop("Response variable must have at least 2 categories.")
  }

  # Get link type
  link_type <- get_link_type(link)

  # Prepare link parameters with defaults
  link_params <- prepare_link_params(link, link_param)

  # Construct Stan data list
  stan_data <- list(
    K = K,
    N = length(y_int),
    P = ncol(X),
    y = y_int,
    X = X,
    link_type = link_type,
    df = link_params$df,
    lambda = link_params$lambda,
    xi = link_params$xi,
    r = link_params$r,
    base_type = link_params$base_type,
    theta1 = link_params$theta1,
    theta2 = link_params$theta2,
    prior_beta_type = PRIOR_TYPES$normal,
    prior_beta_mu = 0,
    prior_beta_sd = prior_beta_sd,
    prior_beta_df = 7,  # placeholder (unused for normal)
    prior_c_type = PRIOR_TYPES$normal,
    prior_c_mu = 0,
    prior_c_sd = prior_c_sd,
    prior_c_df = 7  # placeholder (unused for normal)
  )

  stan_data
}

#' Parse formula for CLM
#'
#' @param formula A formula object
#' @param data A data frame
#'
#' @return A list with response and predictor_formula
#' @keywords internal
parse_clm_formula <- function(formula, data) {
  # Get terms
  terms_obj <- terms(formula, data = data)

  # Extract response variable name
  response_name <- all.vars(formula)[1]

  # Extract response from data
  if (!response_name %in% names(data)) {
    stop(sprintf("Response variable '%s' not found in data.", response_name))
  }
  response <- data[[response_name]]

  # Create predictor-only formula (RHS)
  predictor_formula <- delete.response(terms_obj)

  list(
    response = response,
    response_name = response_name,
    predictor_formula = predictor_formula
  )
}

#' Create design matrix
#'
#' Creates the design matrix for predictors (without intercept column).
#'
#' @param formula A formula object or terms (RHS only)
#' @param data A data frame
#'
#' @return A design matrix (N x P)
#' @keywords internal
make_design_matrix <- function(formula, data) {
  # Create model matrix without intercept
  # The intercept is handled separately as beta0 in the Stan model
  X <- model.matrix(formula, data = data)

  # Remove intercept column if present
  intercept_col <- which(colnames(X) == "(Intercept)")
  if (length(intercept_col) > 0) {
    X <- X[, -intercept_col, drop = FALSE]
  }

  X
}

#' Prepare link parameters for Stan
#'
#' Fills in default values for link parameters and validates them.
#'
#' @param link Link function name
#' @param link_param A list of user-specified link parameters
#'
#' @return A list with all link parameters (df, lambda, xi, r, base_type, theta1, theta2)
#' @keywords internal
prepare_link_params <- function(link, link_param = NULL) {
  # Default values for all link parameters. These are passed to Stan even
  # when not used by the selected link (Stan requires all data variables).
  # Each link only uses its relevant parameters; others are ignored.
  params <- list(
    df = 8,           # tlink, sp(t-base) - must be > 0
    lambda = 1,       # aranda_ordaz (>0), log_gamma
    xi = 0,           # gev
    r = 1,            # sp - must be > 0
    base_type = 1L,   # sp: 1=logit (default)
    theta1 = 2,       # aep - must be > 0
    theta2 = 2        # aep - must be > 0
  )

  # If no link_param provided, return defaults

  if (is.null(link_param)) {
    return(params)
  }

  # Validate and set user-provided parameters based on link type
  required_params <- get_link_params(link)

  for (p in required_params) {
    if (p == "base") {
      # SP link requires a base distribution selection (logit, probit, etc.).
      # The base is converted to an integer code for Stan (base_type).
      # Other links don't use this parameter.
      if ("base" %in% names(link_param)) {
        params$base_type <- get_sp_base_type(link_param$base)
        # If base is tlink, also need df for the Student-t base distribution
        if (link_param$base == "tlink" && "df" %in% names(link_param)) {
          params$df <- link_param$df
        }
      }
    } else if (p %in% names(link_param)) {
      value <- link_param[[p]]
      if (is.character(value) && value == "estimate") {
        stop(sprintf("Parameter '%s' is set to 'estimate', but clm_base.stan ",
                     "only supports fixed parameters. Use clm_full.stan for ",
                     "Bayesian inference of link parameters.", p))
      }
      params[[p]] <- value
    } else {
      warning(sprintf("Link '%s' requires parameter '%s'. Using default value.",
                      link, p))
    }
  }

  # Validation
  validate_link_params(link, params)

  params
}

#' Validate link parameters
#'
#' @param link Link function name
#' @param params A list of link parameters
#' @keywords internal
validate_link_params <- function(link, params) {
  if (link == "tlink" && params$df <= 0) {
    stop("tlink requires df > 0")
  }
  if (link == "aranda_ordaz" && params$lambda <= 0) {
    stop("aranda_ordaz requires lambda > 0")
  }
  if (link == "sp" && params$r <= 0) {
    stop("sp requires r > 0")
  }
  if (link == "aep") {
    if (params$theta1 <= 0) stop("aep requires theta1 > 0")
    if (params$theta2 <= 0) stop("aep requires theta2 > 0")
  }
  invisible(TRUE)
}


# =============================================================================
# Full model support (clm_full.stan)
# =============================================================================

#' Prepare data for full Stan model with link parameter inference
#'
#' @param formula A formula specifying the model
#' @param data A data frame containing the variables
#' @param link Link function name
#' @param link_param A list of link parameters. Values can be:
#'   - numeric: Use as fixed value
#'   - "estimate": Estimate the parameter with default prior
#' @param prior_beta_sd Prior SD for regression coefficients (default: 2.5)
#' @param prior_c_sd Prior SD for cutpoints (default: 10)
#' @param link_prior A list of custom prior specifications for link parameters
#'
#' @return A list suitable for passing to CmdStan (clm_full.stan)
#' @keywords internal
prepare_stan_data_full <- function(formula, data, link = "logit",
                                       link_param = NULL,
                                       prior_beta_sd = 2.5,
                                       prior_c_sd = 10,
                                       link_prior = NULL) {
  # Parse formula and extract response/predictors
  parsed <- parse_clm_formula(formula, data)

  # Create design matrix (without intercept, as beta0 is separate)
  X <- make_design_matrix(parsed$predictor_formula, data)

  # Get response variable
  y <- parsed$response
  if (!is.factor(y)) {
    y <- factor(y)
  }
  y_int <- as.integer(y)
  K <- nlevels(y)

  if (K < 2) {
    stop("Response variable must have at least 2 categories.")
  }

  # Get link type
  link_type <- get_link_type(link)

  # Prepare link parameters for full model
  full_params <- prepare_link_params_full(link, link_param, link_prior)

  # Construct Stan data list for clm_full.stan

  stan_data <- list(
    # Basic data
    K = K,
    N = length(y_int),
    P = ncol(X),
    y = y_int,
    X = X,
    link_type = link_type,

    # SP base type (always fixed)
    base_type = full_params$base_type,

    # Estimation flags
    estimate_df = full_params$estimate_df,
    estimate_lambda = full_params$estimate_lambda,
    estimate_xi = full_params$estimate_xi,
    estimate_r = full_params$estimate_r,
    estimate_theta1 = full_params$estimate_theta1,
    estimate_theta2 = full_params$estimate_theta2,

    # Fixed values
    df_fixed = full_params$df_fixed,
    lambda_fixed = full_params$lambda_fixed,
    xi_fixed = full_params$xi_fixed,
    r_fixed = full_params$r_fixed,
    theta1_fixed = full_params$theta1_fixed,
    theta2_fixed = full_params$theta2_fixed,

    # Prior hyperparameters for beta
    prior_beta_type = PRIOR_TYPES$normal,
    prior_beta_mu = 0,
    prior_beta_sd = prior_beta_sd,
    prior_beta_df = 7,  # placeholder (unused for normal)

    # Prior hyperparameters for cutpoints
    prior_c_type = PRIOR_TYPES$normal,
    prior_c_mu = 0,
    prior_c_sd = prior_c_sd,
    prior_c_df = 7,  # placeholder (unused for normal)

    # Prior hyperparameters for link parameters
    prior_df_alpha = full_params$prior_df_alpha,
    prior_df_beta = full_params$prior_df_beta,
    prior_lambda_ao_alpha = full_params$prior_lambda_ao_alpha,
    prior_lambda_ao_beta = full_params$prior_lambda_ao_beta,
    prior_lambda_lg_mu = full_params$prior_lambda_lg_mu,
    prior_lambda_lg_sd = full_params$prior_lambda_lg_sd,
    prior_xi_mu = full_params$prior_xi_mu,
    prior_xi_sd = full_params$prior_xi_sd,
    prior_r_alpha = full_params$prior_r_alpha,
    prior_r_beta = full_params$prior_r_beta,
    prior_theta1_alpha = full_params$prior_theta1_alpha,
    prior_theta1_beta = full_params$prior_theta1_beta,
    prior_theta2_alpha = full_params$prior_theta2_alpha,
    prior_theta2_beta = full_params$prior_theta2_beta
  )

  stan_data
}

#' Prepare link parameters for full Stan model
#'
#' Determines which parameters to estimate vs. fix, and sets up priors.
#'
#' @param link Link function name
#' @param link_param A list of user-specified link parameters
#' @param link_prior A list of custom prior specifications
#'
#' @return A list with estimation flags, fixed values, and prior hyperparameters
#' @keywords internal
prepare_link_params_full <- function(link, link_param = NULL, link_prior = NULL) {
  # Default fixed values
  defaults <- list(
    df = 8,
    lambda = 1,
    xi = 0,
    r = 1,
    theta1 = 2,
    theta2 = 2
  )

  # Default prior hyperparameters (from links.R get_default_link_prior)
  default_priors <- list(
    df = list(alpha = 2, beta = 0.1),           # gamma(2, 0.1)
    lambda_ao = list(alpha = 0.5, beta = 0.5),  # aranda_ordaz: gamma(0.5, 0.5)
    lambda_lg = list(mu = 0, sd = 1),           # log_gamma: normal(0, 1)
    xi = list(mu = 0, sd = 2),                  # normal(0, 2)
    r = list(alpha = 0.5, beta = 0.5),          # gamma(0.5, 0.5)
    theta1 = list(alpha = 2, beta = 1),         # gamma(2, 1)
    theta2 = list(alpha = 2, beta = 1)          # gamma(2, 1)
  )

  # Initialize result
  result <- list(
    # Estimation flags (0 = fixed, 1 = estimate)
    estimate_df = 0L,
    estimate_lambda = 0L,
    estimate_xi = 0L,
    estimate_r = 0L,
    estimate_theta1 = 0L,
    estimate_theta2 = 0L,

    # Fixed values (used when not estimating)
    df_fixed = defaults$df,
    lambda_fixed = defaults$lambda,
    xi_fixed = defaults$xi,
    r_fixed = defaults$r,
    theta1_fixed = defaults$theta1,
    theta2_fixed = defaults$theta2,

    # SP base type
    base_type = 1L,  # default: logit

    # Prior hyperparameters
    prior_df_alpha = default_priors$df$alpha,
    prior_df_beta = default_priors$df$beta,
    prior_lambda_ao_alpha = default_priors$lambda_ao$alpha,
    prior_lambda_ao_beta = default_priors$lambda_ao$beta,
    prior_lambda_lg_mu = default_priors$lambda_lg$mu,
    prior_lambda_lg_sd = default_priors$lambda_lg$sd,
    prior_xi_mu = default_priors$xi$mu,
    prior_xi_sd = default_priors$xi$sd,
    prior_r_alpha = default_priors$r$alpha,
    prior_r_beta = default_priors$r$beta,
    prior_theta1_alpha = default_priors$theta1$alpha,
    prior_theta1_beta = default_priors$theta1$beta,
    prior_theta2_alpha = default_priors$theta2$alpha,
    prior_theta2_beta = default_priors$theta2$beta
  )

  # If no link_param provided, return defaults (all fixed)
  if (is.null(link_param)) {
    return(result)
  }

  # Process each parameter in link_param
  for (param_name in names(link_param)) {
    value <- link_param[[param_name]]

    if (param_name == "base") {
      # SP base distribution (always fixed, not estimated)
      result$base_type <- get_sp_base_type(value)
      next
    }

    if (is.character(value) && value == "estimate") {
      # Mark for estimation
      flag_name <- paste0("estimate_", param_name)
      if (flag_name %in% names(result)) {
        result[[flag_name]] <- 1L
      }
    } else if (is.numeric(value)) {
      # Use as fixed value
      fixed_name <- paste0(param_name, "_fixed")
      if (fixed_name %in% names(result)) {
        result[[fixed_name]] <- value
      }
    }
  }

  # Apply custom priors if provided
  if (!is.null(link_prior)) {
    result <- apply_custom_link_priors(result, link_prior)
  }

  # Validate: check that estimation makes sense for the link
  validate_full_params(link, result)

  result
}

#' Apply custom prior specifications for link parameters
#'
#' @param result Current result list
#' @param link_prior List of custom prior specifications
#'
#' @return Updated result list with custom priors
#' @keywords internal
apply_custom_link_priors <- function(result, link_prior) {
  # Expected format: list(df = list(alpha = 3, beta = 0.2), ...)
  for (param_name in names(link_prior)) {
    prior_spec <- link_prior[[param_name]]

    if (param_name == "df" && is.list(prior_spec)) {
      if ("alpha" %in% names(prior_spec)) result$prior_df_alpha <- prior_spec$alpha
      if ("beta" %in% names(prior_spec)) result$prior_df_beta <- prior_spec$beta
    } else if (param_name == "lambda_ao" && is.list(prior_spec)) {
      if ("alpha" %in% names(prior_spec)) result$prior_lambda_ao_alpha <- prior_spec$alpha
      if ("beta" %in% names(prior_spec)) result$prior_lambda_ao_beta <- prior_spec$beta
    } else if (param_name == "lambda_lg" && is.list(prior_spec)) {
      if ("mu" %in% names(prior_spec)) result$prior_lambda_lg_mu <- prior_spec$mu
      if ("sd" %in% names(prior_spec)) result$prior_lambda_lg_sd <- prior_spec$sd
    } else if (param_name == "xi" && is.list(prior_spec)) {
      if ("mu" %in% names(prior_spec)) result$prior_xi_mu <- prior_spec$mu
      if ("sd" %in% names(prior_spec)) result$prior_xi_sd <- prior_spec$sd
    } else if (param_name == "r" && is.list(prior_spec)) {
      if ("alpha" %in% names(prior_spec)) result$prior_r_alpha <- prior_spec$alpha
      if ("beta" %in% names(prior_spec)) result$prior_r_beta <- prior_spec$beta
    } else if (param_name == "theta1" && is.list(prior_spec)) {
      if ("alpha" %in% names(prior_spec)) result$prior_theta1_alpha <- prior_spec$alpha
      if ("beta" %in% names(prior_spec)) result$prior_theta1_beta <- prior_spec$beta
    } else if (param_name == "theta2" && is.list(prior_spec)) {
      if ("alpha" %in% names(prior_spec)) result$prior_theta2_alpha <- prior_spec$alpha
      if ("beta" %in% names(prior_spec)) result$prior_theta2_beta <- prior_spec$beta
    }
  }

  result
}

#' Validate full model parameters
#'
#' Checks that estimation flags make sense for the specified link.
#'
#' @param link Link function name
#' @param params Full model parameters list
#' @keywords internal
validate_full_params <- function(link, params) {
  # Check that we're only estimating relevant parameters
  link_type <- get_link_type(link)

  # df: only for tlink (6) or sp with tlink base (8)
  if (params$estimate_df == 1L) {
    if (link_type != 6 && !(link_type == 8 && params$base_type == 6)) {
      warning(sprintf("df estimation requested but link '%s' does not use df.", link))
    }
  }

  # lambda: only for aranda_ordaz (7) or log_gamma (9)
  if (params$estimate_lambda == 1L) {
    if (link_type != 7 && link_type != 9) {
      warning(sprintf("lambda estimation requested but link '%s' does not use lambda.", link))
    }
  }

  # xi: only for gev (10)
  if (params$estimate_xi == 1L) {
    if (link_type != 10) {
      warning(sprintf("xi estimation requested but link '%s' does not use xi.", link))
    }
  }

  # r: only for sp (8)
  if (params$estimate_r == 1L) {
    if (link_type != 8) {
      warning(sprintf("r estimation requested but link '%s' does not use r.", link))
    }
  }

  # theta1, theta2: only for aep (11)
  if (params$estimate_theta1 == 1L || params$estimate_theta2 == 1L) {
    if (link_type != 11) {
      warning(sprintf("theta estimation requested but link '%s' does not use theta1/theta2.", link))
    }
  }

  invisible(TRUE)
}

#' Check if any link parameter requires estimation
#'
#' @param link_param A list of link parameters
#' @return TRUE if any parameter is set to "estimate"
#' @keywords internal
needs_full_model <- function(link_param) {
  if (is.null(link_param)) {
    return(FALSE)
  }

  for (value in link_param) {
    if (is.character(value) && value == "estimate") {
      return(TRUE)
    }
  }

  FALSE
}


# =============================================================================
# Equidistant threshold support (clm_equidistant.stan)
# =============================================================================

#' Prepare data for equidistant threshold Stan model
#'
#' Creates a Stan data list for cumulative link models with equidistant
#' (equally spaced) thresholds: c_k = c_1 + (k-1) * d
#'
#' @param formula A formula specifying the model
#' @param data A data frame containing the variables
#' @param link Link function name
#' @param link_param A list of link parameters (for flexible links)
#' @param prior_beta_sd Prior SD for regression coefficients (default: 2.5)
#' @param prior_c1_mu Prior mean for first threshold c1 (default: 0)
#' @param prior_c1_sd Prior SD for first threshold c1 (default: 10)
#' @param prior_d_alpha Gamma prior shape for interval d (default: 2)
#' @param prior_d_beta Gamma prior rate for interval d (default: 0.5)
#'
#' @return A list suitable for passing to CmdStan (clm_equidistant.stan)
#' @keywords internal
prepare_stan_data_equidistant <- function(formula, data, link = "logit",
                                          link_param = NULL,
                                          prior_beta_sd = 2.5,
                                          prior_c1_mu = 0,
                                          prior_c1_sd = 10,
                                          prior_d_alpha = 2,
                                          prior_d_beta = 0.5) {
  # Parse formula and extract response/predictors
  parsed <- parse_clm_formula(formula, data)

  # Create design matrix (without intercept, as beta0 is separate)
  X <- make_design_matrix(parsed$predictor_formula, data)

  # Get response variable
  y <- parsed$response
  if (!is.factor(y)) {
    y <- factor(y)
  }
  y_int <- as.integer(y)
  K <- nlevels(y)

  if (K < 2) {
    stop("Response variable must have at least 2 categories.")
  }

  # Warn if K == 2 (d is not identifiable)
  if (K == 2) {
    warning("For K = 2 (2 categories), the interval parameter 'd' is not identifiable. ",
            "Consider using 'flexible' threshold structure instead.")
  }

  # Get link type
  link_type <- get_link_type(link)

  # Prepare link parameters with defaults
  link_params <- prepare_link_params(link, link_param)

  # Construct Stan data list
  stan_data <- list(
    K = K,
    N = length(y_int),
    P = ncol(X),
    y = y_int,
    X = X,
    link_type = link_type,
    df = link_params$df,
    lambda = link_params$lambda,
    xi = link_params$xi,
    r = link_params$r,
    base_type = link_params$base_type,
    theta1 = link_params$theta1,
    theta2 = link_params$theta2,
    prior_beta_type = PRIOR_TYPES$normal,
    prior_beta_mu = 0,
    prior_beta_sd = prior_beta_sd,
    prior_beta_df = 7,  # placeholder (unused for normal)
    prior_c1_type = PRIOR_TYPES$normal,
    prior_c1_mu = prior_c1_mu,
    prior_c1_sd = prior_c1_sd,
    prior_c1_df = 7,  # placeholder (unused for normal)
    prior_d_alpha = prior_d_alpha,
    prior_d_beta = prior_d_beta
  )

  stan_data
}


# =============================================================================
# Symmetric threshold support (clm_symmetric.stan)
# =============================================================================

#' Prepare data for symmetric threshold Stan model
#'
#' Creates a Stan data list for cumulative link models with symmetric
#' thresholds centered at 0: `c[k] = -c[K-k]`
#'
#' Examples:
#' * K=4 (3 thresholds): c = (-a, 0, a)
#' * K=5 (4 thresholds): c = (-b, -a, a, b)
#' * K=6 (5 thresholds): c = (-b, -a, 0, a, b)
#'
#' @param formula A formula specifying the model
#' @param data A data frame containing the variables
#' @param link Link function name
#' @param link_param A list of link parameters (for flexible links)
#' @param prior_beta_sd Prior SD for regression coefficients (default: 2.5)
#' @param prior_cpos_sd Prior SD for positive thresholds (default: 5)
#'
#' @return A list suitable for passing to CmdStan (clm_symmetric.stan)
#' @keywords internal
prepare_stan_data_symmetric <- function(formula, data, link = "logit",
                                        link_param = NULL,
                                        prior_beta_sd = 2.5,
                                        prior_cpos_sd = 5) {
  # Parse formula and extract response/predictors
  parsed <- parse_clm_formula(formula, data)

  # Create design matrix (without intercept, as beta0 is separate)
  X <- make_design_matrix(parsed$predictor_formula, data)

  # Get response variable
  y <- parsed$response
  if (!is.factor(y)) {
    y <- factor(y)
  }
  y_int <- as.integer(y)
  K <- nlevels(y)

  # Symmetric thresholds require K >= 3
  if (K < 3) {
    stop("Symmetric thresholds require at least 3 categories (K >= 3). ",
         "For K = 2, use 'flexible' threshold structure instead.")
  }

  # Get link type
  link_type <- get_link_type(link)

  # Prepare link parameters with defaults
  link_params <- prepare_link_params(link, link_param)

  # Construct Stan data list
  stan_data <- list(
    K = K,
    N = length(y_int),
    P = ncol(X),
    y = y_int,
    X = X,
    link_type = link_type,
    df = link_params$df,
    lambda = link_params$lambda,
    xi = link_params$xi,
    r = link_params$r,
    base_type = link_params$base_type,
    theta1 = link_params$theta1,
    theta2 = link_params$theta2,
    prior_beta_type = PRIOR_TYPES$normal,
    prior_beta_mu = 0,
    prior_beta_sd = prior_beta_sd,
    prior_beta_df = 7,  # placeholder (unused for normal)
    prior_cpos_type = PRIOR_TYPES$normal,
    prior_cpos_sd = prior_cpos_sd,
    prior_cpos_df = 7  # placeholder (unused for normal)
  )

  stan_data
}


# =============================================================================
# Data dispatch function (used by clm_stan())
# =============================================================================

#' Dispatch to appropriate data preparation function
#'
#' Routes to the correct prepare function based on threshold structure
#' and whether link parameters are being estimated.
#'
#' @param formula A formula specifying the model
#' @param data A data frame containing the variables
#' @param link Link function name
#' @param base Base distribution for SP link
#' @param link_param A list of link parameters
#' @param threshold Threshold structure
#' @param full Whether to use full model (estimate link parameters)
#' @param prior A clm_prior object or NULL for default priors
#'
#' @return A list suitable for passing to CmdStan
#' @keywords internal
prepare_stan_data_dispatch <- function(formula, data, link, base, link_param,
                                        threshold, full, prior) {
  # Validate prior specification
  validate_prior(prior, threshold, link, link_param)

  # Add base to link_param if SP link
  if (link == "sp" && !is.null(base)) {
    if (is.null(link_param)) {
      link_param <- list(base = base)
    } else if (!"base" %in% names(link_param)) {
      link_param$base <- base
    }
  }

  # Dispatch based on threshold and full flag
  if (threshold == "flexible") {
    if (full) {
      stan_data <- prepare_stan_data_full(formula, data, link, link_param)
    } else {
      stan_data <- prepare_stan_data(formula, data, link, link_param)
    }
  } else if (threshold == "equidistant") {
    if (full) {
      # equidistant + full: not yet implemented
      warning("Equidistant threshold with link parameter estimation is not yet ",
              "implemented. Using base model with fixed link parameters.")
      stan_data <- prepare_stan_data_equidistant(formula, data, link, link_param)
    } else {
      stan_data <- prepare_stan_data_equidistant(formula, data, link, link_param)
    }
  } else if (threshold == "symmetric") {
    if (full) {
      # symmetric + full: not yet implemented
      warning("Symmetric threshold with link parameter estimation is not yet ",
              "implemented. Using base model with fixed link parameters.")
      stan_data <- prepare_stan_data_symmetric(formula, data, link, link_param)
    } else {
      stan_data <- prepare_stan_data_symmetric(formula, data, link, link_param)
    }
  } else {
    stop(sprintf("Unknown threshold structure: %s", threshold))
  }

  # Apply user-specified priors
  stan_data <- apply_priors_to_stan_data(stan_data, prior, threshold, full)

  stan_data
}
