#' Available Link Functions
#'
#' @description
#' clmstan supports the following link functions for cumulative link models:
#'
#' **Standard links (no additional parameters):**
#' * `"logit"` - Logistic (proportional odds model)
#' * `"probit"` - Normal (latent variable interpretation)
#' * `"cloglog"` - Complementary log-log (proportional hazards)
#' * `"loglog"` - Log-log (Gumbel minimum)
#' * `"cauchit"` - Cauchy (heavy tails)
#'
#' **Flexible links (with additional parameters):**
#' * `"tlink"` - Student-t (df > 0)
#'   - df = Inf: equals probit
#'   - df < 3: increasingly heavy tails; df > 30 is nearly normal
#' * `"aranda_ordaz"` - Aranda-Ordaz asymmetric (lambda > 0)
#'   - lambda = 1: equals logit
#'   - lambda -> 0: approaches cloglog
#' * `"gev"` - Generalized extreme value (shape parameter xi)
#'   - xi = 0: Gumbel (equals loglog)
#'   - xi < 0: Weibull (short tail)
#'   - xi > 0: Frechet (heavy tail)
#' * `"sp"` - Symmetric power (r > 0, base distribution)
#'   - r = 1: equals base distribution
#'   - 0 < r < 1: positively skewed
#'   - r > 1: negatively skewed
#' * `"log_gamma"` - Log-gamma (lambda)
#'   - lambda = 0: equals probit
#'   - lambda > 0 or < 0: asymmetric
#' * `"aep"` - Asymmetric exponential power (theta1 > 0, theta2 > 0)
#'   - alpha = 0.5 fixed for identifiability
#'   - theta1 = theta2: symmetric distribution
#'   - theta = 2: Gaussian kernel (but NOT equal to probit due to scaling)
#'   - theta < 2: heavy tails (leptokurtic)
#'   - theta > 2: light tails (platykurtic)
#'
#' @section Link Parameter Specification:
#' Flexible link parameters can be either **fixed** or **estimated** (inferred).
#'
#' **Fixed parameters:** Specify a numeric value
#' ```
#' clm_stan(y ~ x, link = "tlink", link_param = list(df = 8))
#' clm_stan(y ~ x, link = "gev", link_param = list(xi = 0))  # equals loglog
#' clm_stan(y ~ x, link = "aep", link_param = list(theta1 = 2, theta2 = 2))  # symmetric
#' ```
#'
#' **Estimated parameters:** Use `"estimate"` (with default prior)
#' ```
#' clm_stan(y ~ x, link = "tlink", link_param = list(df = "estimate"))
#' clm_stan(y ~ x, link = "gev", link_param = list(xi = "estimate"))
#' ```
#'
#' **Custom priors:** Combine `"estimate"` with `prior` argument
#' ```
#' clm_stan(y ~ x, link = "gev",
#'          link_param = list(xi = "estimate"),
#'          prior = prior(normal(0, 0.3), class = "xi"))
#' ```
#'
#' @section Default Priors for Link Parameters:
#' When using `"estimate"`, the following default priors are used:
#'
#' | Link | Parameter | Default Prior | Notes |
#' |------|-----------|---------------|-------|
#' | tlink | df | gamma(2, 0.1) | Mode around 10, allows heavy tails |
#' | aranda_ordaz | lambda | gamma(0.5, 0.5) | Centered near 1 (logit) |
#' | gev | xi | normal(0, 2) | Weakly informative, Wang & Dey (2011) |
#' | sp | r | gamma(0.5, 0.5) | Centered near 1 (base distribution) |
#' | log_gamma | lambda | normal(0, 1) | Centered at 0 (probit) |
#' | aep | theta1 | gamma(2, 1) | Mode at 1, symmetric at theta1=theta2 |
#' | aep | theta2 | gamma(2, 1) | Mode at 1, symmetric at theta1=theta2 |
#'
#' @section SP Link Details (Li et al., 2019):
#' The Symmetric Power link uses a symmetric base distribution F_0,
#' specified via the `base` argument. Supported bases:
#' * `base = "logit"`: Logistic base (default)
#' * `base = "probit"`: Normal base
#' * `base = "cauchit"`: Cauchy base
#' * `base = "tlink"`: Student-t base (requires df)
#'
#' Note: Li et al. (2019) define F_0 as a CDF "whose corresponding PDF is
#' symmetric about 0".
#'
#' @name link_functions
#' @family link functions
NULL

#' Get supported link functions
#'
#' @param type Character string specifying which links to return:
#'   * `"all"` (default): All supported link functions
#'   * `"standard"`: Standard links without additional parameters
#'   * `"flexible"`: Flexible links with additional parameters
#' @return A character vector of supported link function names
#' @export
#' @examples
#' supported_links()
#' supported_links("standard")
#' supported_links("flexible")
supported_links <- function(type = c("all", "standard", "flexible")) {
  type <- match.arg(type)
  standard <- c("logit", "probit", "cloglog", "loglog", "cauchit")
  flexible <- c("tlink", "aranda_ordaz", "gev", "sp", "log_gamma", "aep")
  switch(type,
    "all" = c(standard, flexible),
    "standard" = standard,
    "flexible" = flexible
  )
}

#' Check if a link function is valid
#'
#' @param link A character string specifying the link function
#' @return TRUE if valid, otherwise throws an error
#' @keywords internal
validate_link <- function(link) {
  all_links <- supported_links("all")
  if (!link %in% all_links) {
    stop(sprintf("Unknown link function '%s'. Supported links: %s",
                 link, paste(all_links, collapse = ", ")))
  }
  TRUE
}

#' Check if a link function requires parameters
#'
#' @param link A character string specifying the link function
#' @return TRUE if the link requires additional parameters
#' @keywords internal
is_flexible_link <- function(link) {
  link %in% supported_links("flexible")
}

#' Get required parameters for a link function
#'
#' @param link A character string specifying the link function
#' @return A character vector of required parameter names
#' @keywords internal
get_link_params <- function(link) {
  switch(link,
    "tlink" = "df",
    "aranda_ordaz" = "lambda",
    "gev" = "xi",
    "sp" = c("r", "base"),
    "log_gamma" = "lambda",
    "aep" = c("theta1", "theta2"),
    character(0)
  )
}

#' Get default prior for a link parameter
#'
#' @param link A character string specifying the link function
#' @param param A character string specifying the parameter name
#' @return A list with prior specification (family, args)
#' @keywords internal
get_default_link_prior <- function(link, param) {
  priors <- list(
    tlink = list(df = list(family = "gamma", args = c(2, 0.1))),
    aranda_ordaz = list(lambda = list(family = "gamma", args = c(0.5, 0.5))),
    gev = list(xi = list(family = "normal", args = c(0, 2))),
    sp = list(r = list(family = "gamma", args = c(0.5, 0.5))),
    log_gamma = list(lambda = list(family = "normal", args = c(0, 1))),
    aep = list(
      theta1 = list(family = "gamma", args = c(2, 1)),
      theta2 = list(family = "gamma", args = c(2, 1))
    )
  )
  if (!link %in% names(priors)) return(NULL)
  if (!param %in% names(priors[[link]])) return(NULL)
  priors[[link]][[param]]
}

#' Get link type number for Stan
#'
#' Converts a link function name to the integer code used in Stan.
#'
#' @param link A character string specifying the link function
#' @return An integer (1-11) representing the link type
#' @keywords internal
#'
#' @details
#' Link type mapping:
#' * 1: logit
#' * 2: probit
#' * 3: cloglog
#' * 4: loglog
#' * 5: cauchit
#' * 6: tlink
#' * 7: aranda_ordaz
#' * 8: sp
#' * 9: log_gamma
#' * 10: gev
#' * 11: aep
get_link_type <- function(link) {
  validate_link(link)
  link_map <- c(
    logit = 1L,
    probit = 2L,
    cloglog = 3L,
    loglog = 4L,
    cauchit = 5L,
    tlink = 6L,
    aranda_ordaz = 7L,
    sp = 8L,
    log_gamma = 9L,
    gev = 10L,
    aep = 11L
  )
  link_map[[link]]
}

#' Get base type number for SP link
#'
#' Converts a base distribution name to the integer code used in Stan for SP link.
#'
#' @param base A character string specifying the base distribution
#' @return An integer (1-6) representing the base type
#' @keywords internal
#'
#' @details
#' Base type mapping (symmetric distributions only, per Li et al. 2019):
#' * 1: logit
#' * 2: probit
#' * 3: cauchit
#' * 4: tlink
get_sp_base_type <- function(base) {
  base_map <- c(
    logit = 1L,
    probit = 2L,
    cauchit = 3L,
    tlink = 4L
  )
  if (!base %in% names(base_map)) {
    stop(sprintf("Unknown base distribution '%s' for SP link. Supported: %s",
                 base, paste(names(base_map), collapse = ", ")))
  }
  base_map[[base]]
}

# =============================================================================
# Threshold Structure Functions
# =============================================================================

#' Get supported threshold structures
#'
#' @return A character vector of supported threshold structure names
#' @export
#' @examples
#' supported_thresholds()
supported_thresholds <- function() {
  c("flexible", "equidistant", "symmetric")
}

#' Validate threshold specification
#'
#' @param threshold A character string specifying the threshold structure
#' @param K Number of ordinal categories (optional)
#' @return TRUE if valid, otherwise throws an error
#' @keywords internal
#'
#' @details
#' Validation rules:
#' * `flexible`: No restrictions
#' * `equidistant`: K = 2 triggers a warning (d is not identifiable)
#' * `symmetric`: Requires K >= 3
validate_threshold <- function(threshold, K = NULL) {
  all_thresholds <- supported_thresholds()
  if (!threshold %in% all_thresholds) {
    stop(sprintf("Unknown threshold structure '%s'. Supported: %s",
                 threshold, paste(all_thresholds, collapse = ", ")))
  }

  if (!is.null(K)) {
    if (threshold == "symmetric" && K < 3) {
      stop("Symmetric thresholds require at least 3 categories (K >= 3).")
    }
    if (threshold == "equidistant" && K == 2) {
      warning("For K = 2, the interval parameter 'd' is not identifiable. ",
              "Consider using 'flexible' threshold structure instead.")
    }
  }

  TRUE
}

#' Get threshold parameter information
#'
#' @param threshold A character string specifying the threshold structure
#' @return A list with parameter names and their Stan types
#' @keywords internal
#'
#' @details
#' Threshold parameters:
#' * `flexible`: `c` (ordered\[K-1\])
#' * `equidistant`: `c1` (real), `d` (real<lower=0>)
#' * `symmetric`: `c_pos` (positive_ordered\[(K-1)/2\])
get_threshold_params <- function(threshold) {
  switch(threshold,
    "flexible" = list(
      params = "c",
      stan_type = "ordered[K-1]",
      n_params = function(K) K - 1
    ),
    "equidistant" = list(
      params = c("c1", "d"),
      stan_type = c("real", "real<lower=0>"),
      n_params = function(K) 2
    ),
    "symmetric" = list(
      params = "c_pos",
      stan_type = "positive_ordered[half]",
      n_params = function(K) (K - 1) %/% 2
    ),
    NULL
  )
}

#' Get default priors for threshold parameters
#'
#' @param threshold A character string specifying the threshold structure
#' @return A list with default prior specifications
#' @keywords internal
#'
#' @details
#' Default priors:
#' * `flexible`: c ~ normal(0, prior_c_sd)
#' * `equidistant`: c1 ~ normal(0, 10), d ~ gamma(2, 0.5)
#' * `symmetric`: c_pos ~ normal(0, 5) (truncated to positive)
get_default_threshold_prior <- function(threshold) {
  switch(threshold,
    "flexible" = list(
      c = list(family = "normal", args = c(0, 10))
    ),
    "equidistant" = list(
      c1 = list(family = "normal", args = c(0, 10)),
      d = list(family = "gamma", args = c(2, 0.5))
    ),
    "symmetric" = list(
      c_pos = list(family = "normal", args = c(0, 5))
    ),
    NULL
  )
}

#' Get Stan model name based on threshold and full model flag
#'
#' @param threshold A character string specifying the threshold structure
#' @param full Logical indicating whether link parameters are estimated
#' @return A character string with the Stan model name
#' @keywords internal
get_model_name <- function(threshold, full = FALSE) {
  base_name <- switch(threshold,
    "flexible" = "clm",
    "equidistant" = "clm_equidistant",
    "symmetric" = "clm_symmetric"
  )

  if (full) {
    if (threshold == "flexible") {
      "clm_full"
    } else {
      paste0(base_name, "_full")
    }
  } else {
    if (threshold == "flexible") {
      "clm_base"
    } else {
      base_name
    }
  }
}
