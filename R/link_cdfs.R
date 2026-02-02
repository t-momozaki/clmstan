# =============================================================================
# R implementations of link function CDFs for prediction
# =============================================================================
#
# RELATED FILES:
# - src/stan/functions/link_*.stan: Stan implementations (for sampling)
# - R/links.R: Link function utilities (get_link_type, supported_links, etc.)
# - R/predict.R: Uses these CDFs for prediction
#
# These R implementations mirror the Stan link functions and are used
# for computing predictions on new data in R (rather than in Stan).
#
# IMPORTANT: This package defines clmstan::gamma() as a prior distribution
# helper (see priors.R). When computing the mathematical gamma function
# Gamma(x) in this file, always use base::gamma() explicitly to avoid
# calling the prior helper by mistake.
# =============================================================================

#' Unified CDF dispatcher for clmstan
#'
#' Computes the cumulative distribution function (CDF) for any supported
#' link function. This is the R equivalent of `unified_F()` in Stan.
#'
#' @param x Numeric vector of values
#' @param link Link function name (character)
#' @param link_param List of link parameters (for flexible links)
#' @return CDF values F(x)
#' @keywords internal
clm_cdf <- function(x, link, link_param = list()) {
  switch(link,
    # Standard links (no parameters)
    "logit" = plogis(x),
    "probit" = pnorm(x),
    "cloglog" = cloglog_cdf(x),
    "loglog" = loglog_cdf(x),
    "cauchit" = pcauchy(x),
    # Flexible links (with parameters)
    "tlink" = pt(x, df = link_param$df %||% 8),
    "aranda_ordaz" = aranda_ordaz_cdf(x, link_param$lambda %||% 1),
    "sp" = sp_cdf(x, link_param$r %||% 1, link_param$base %||% "logit",
                  link_param$df %||% 8),
    "log_gamma" = loggamma_cdf(x, link_param$lambda %||% 0),
    "gev" = gev_cdf(x, link_param$xi %||% 0),
    "aep" = aep_cdf(x, link_param$theta1 %||% 2, link_param$theta2 %||% 2),
    stop("Unknown link function: ", link)
  )
}

# =============================================================================
# Standard link CDFs
# =============================================================================

#' Complementary log-log CDF
#'
#' F(x) = 1 - exp(-exp(x))
#'
#' This corresponds to the Gumbel (maximum) distribution.
#'
#' @param x Numeric vector
#' @return CDF values
#' @keywords internal
cloglog_cdf <- function(x) {
  # Use numerically stable computation
  # For large positive x: 1 - exp(-exp(x)) -> 1
  # For large negative x: 1 - exp(-exp(x)) -> 0
  1 - exp(-exp(x))
}

#' Log-log CDF
#'
#' F(x) = exp(-exp(-x))
#'
#' This corresponds to the Gumbel (minimum) distribution.
#' It is the reflection of cloglog: loglog(x) = 1 - cloglog(-x)
#'
#' @param x Numeric vector
#' @return CDF values
#' @keywords internal
loglog_cdf <- function(x) {
  exp(-exp(-x))
}

# =============================================================================
# Flexible link CDFs
# =============================================================================

#' Aranda-Ordaz asymmetric link CDF
#'
#' F(x; lambda) = 1 - (1 + exp(x))^(-lambda)
#'
#' Special cases:
#' - lambda = 1: logit
#' - lambda -> 0: cloglog
#'
#' Reference: Aranda-Ordaz (1981) Biometrika
#'
#' @param x Numeric vector
#' @param lambda Shape parameter (lambda > 0)
#' @return CDF values
#' @keywords internal
aranda_ordaz_cdf <- function(x, lambda) {
  if (lambda <= 0) {
    stop("Aranda-Ordaz lambda must be positive")
  }
  1 - (1 + exp(x))^(-lambda)
}

#' Symmetric Power (SP) link CDF
#'
#' The SP link applies a power transformation to a base CDF:
#'
#' For r <= 1: F_sp(x) = F_0(x/r)^r
#' For r > 1:  F_sp(x) = 1 - F_0(-r*x)^(1/r)
#'
#' Special case: r = 1 gives the base distribution F_0.
#'
#' Reference: Li et al. (2019) Environmetrics
#'
#' @param x Numeric vector
#' @param r Power parameter (r > 0)
#' @param base Base distribution name
#' @param df Degrees of freedom (for tlink base)
#' @return CDF values
#' @keywords internal
sp_cdf <- function(x, r, base, df = 8) {
  if (r <= 0) {
    stop("SP r must be positive")
  }


  # Get base CDF function (symmetric distributions only, per Li et al. 2019)
  F0 <- switch(base,
    "logit" = plogis,
    "probit" = pnorm,
    "cauchit" = pcauchy,
    "tlink" = function(z) pt(z, df = df),
    stop("Unknown SP base distribution: ", base,
         ". Supported: logit, probit, cauchit, tlink")
  )

  if (r <= 1) {
    # F_sp = F_0^r(x/r)
    F0(x / r)^r
  } else {
    # F_sp = 1 - F_0^{1/r}(-r*x)
    1 - F0(-r * x)^(1 / r)
  }
}

#' Log-gamma link CDF
#'
#' Based on the log-gamma distribution:
#' - lambda = 0: probit (normal)
#' - lambda > 0: F(x) = pgamma(exp(x), shape = lambda, rate = lambda)
#' - lambda < 0: F(x) = 1 - pgamma(exp(-x), shape = -lambda, rate = -lambda)
#'
#' Reference: Prentice (1976) Biometrics
#'
#' @param x Numeric vector
#' @param lambda Shape parameter
#' @return CDF values
#' @keywords internal
loggamma_cdf <- function(x, lambda) {
  if (abs(lambda) < 1e-10) {
    # lambda = 0: probit
    pnorm(x)
  } else if (lambda > 0) {
    # F(x) = pgamma(exp(x), shape = lambda, rate = lambda)
    pgamma(exp(x), shape = lambda, rate = lambda)
  } else {
    # lambda < 0: F(x) = 1 - pgamma(exp(-x), shape = -lambda, rate = -lambda)
    1 - pgamma(exp(-x), shape = -lambda, rate = -lambda)
  }
}

#' Generalized Extreme Value (GEV) link CDF
#'
#' F(x; xi) = exp(-(1 + xi * x)^(-1/xi))
#'
#' Special cases:
#' - xi = 0: Gumbel (equals loglog)
#' - xi < 0: Weibull (bounded above)
#' - xi > 0: Frechet (heavy tail)
#'
#' Reference: Wang & Dey (2011)
#'
#' @param x Numeric vector
#' @param xi Shape parameter
#' @return CDF values
#' @keywords internal
gev_cdf <- function(x, xi) {
  if (abs(xi) < 1e-10) {
    # Gumbel: exp(-exp(-x)) = loglog
    exp(-exp(-x))
  } else {
    t <- 1 + xi * x
    # Handle support constraints
    # xi > 0: x > -1/xi (left bounded)
    # xi < 0: x < -1/xi (right bounded)
    result <- ifelse(
      t <= 0,
      ifelse(xi > 0, 0, 1),
      exp(-t^(-1 / xi))
    )
    result
  }
}

#' Asymmetric Exponential Power (AEP) link CDF
#'
#' The AEP distribution with alpha = 0.5 (fixed for identifiability).
#'
#' For x <= 0:
#'   F(x) = 0.5 * (1 - P(1/theta1, u1))
#'   where u1 = (|x| * 2 * Gamma(1 + 1/theta1))^theta1
#'
#' For x > 0:
#'   F(x) = 0.5 + 0.5 * P(1/theta2, u2)
#'   where u2 = (x * 2 * Gamma(1 + 1/theta2))^theta2
#'
#' P(a, x) is the regularized incomplete gamma function (pgamma).
#'
#' Special case: theta1 = theta2 gives a symmetric distribution.
#' Note: theta = 2 has a Gaussian kernel but is NOT equal to probit due to scaling.
#'
#' Reference: Naranjo et al. (2015) Statistics and Computing
#'
#' @param x Numeric vector
#' @param theta1 Left tail parameter (theta1 > 0)
#' @param theta2 Right tail parameter (theta2 > 0)
#' @return CDF values
#' @keywords internal
aep_cdf <- function(x, theta1, theta2) {
  if (theta1 <= 0 || theta2 <= 0) {
    stop("AEP theta parameters must be positive")
  }

  result <- numeric(length(x))

  # Left tail (x <= 0)
  left <- x <= 0
  if (any(left)) {
    a1 <- 1 / theta1
    # Use base::gamma() for mathematical Gamma function (see file header)
    gamma1 <- base::gamma(1 + a1)
    z1 <- abs(x[left]) * 2 * gamma1
    u1 <- z1^theta1
    # Compute 1 - P(a1, u1) using upper tail for numerical stability
    result[left] <- 0.5 * pgamma(u1, shape = a1, rate = 1, lower.tail = FALSE)
  }

  # Right tail (x > 0)
  right <- x > 0
  if (any(right)) {
    a2 <- 1 / theta2
    # Use base::gamma() for mathematical Gamma function (see file header)
    gamma2 <- base::gamma(1 + a2)
    z2 <- x[right] * 2 * gamma2
    u2 <- z2^theta2
    result[right] <- 0.5 + 0.5 * pgamma(u2, shape = a2, rate = 1)
  }

  result
}

# =============================================================================
# Utility: null-coalescing operator
# =============================================================================

# Null-coalescing operator (internal utility)
# Returns the left-hand side if it is not NULL, otherwise the right-hand side.
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
