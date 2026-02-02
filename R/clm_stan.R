#' Fit a Cumulative Link Model using CmdStanR
#'
#' @param formula A formula specifying the model (response ~ predictors)
#' @param data A data frame containing the variables in the formula
#' @param link Link function. One of "logit" (default), "probit", "cloglog",
#'   "loglog", "cauchit", "tlink", "gev", "aep", "sp", "aranda_ordaz", "log_gamma"
#' @param base Base distribution for SP link. One of "logit" (default),
#'   "probit", "cloglog", "loglog", "cauchit", "tlink". Ignored for other link functions.
#' @param threshold Threshold structure. One of "flexible" (default),
#'   "equidistant", "symmetric"
#' @param link_param A list of link parameters. For flexible links, values can be:
#'   \itemize{
#'     \item Numeric: Use as fixed value (e.g., \code{list(df = 8)})
#'     \item "estimate": Estimate the parameter with Bayesian inference
#'   }
#' @param prior Prior specification. Can be either:
#'   \itemize{
#'     \item A \code{clm_prior} object created by [clm_prior()]
#'     \item A distribution-based prior using [prior()] with distribution functions
#'       ([normal()], [gamma()], [student_t()], [cauchy()])
#'   }
#' @param chains Number of MCMC chains (default: 4)
#' @param iter Total iterations per chain (default: 2000)
#' @param warmup Warmup iterations per chain. If NULL (default), uses floor(iter/2)
#' @param ... Additional arguments passed to cmdstanr::sample()
#'
#' @return An object of class "clmstan"
#' @export
#'
#' @examples
#' \dontrun{
#' # Fit a proportional odds model
#' library(ordinal)
#' data(wine)
#' fit <- clm_stan(rating ~ temp + contact, data = wine, link = "logit")
#' print(fit)
#'
#' # Fit with t-link (fixed df)
#' fit_t <- clm_stan(rating ~ temp, data = wine, link = "tlink",
#'                   link_param = list(df = 8))
#'
#' # Fit with GEV link (estimate xi)
#' fit_gev <- clm_stan(rating ~ temp, data = wine, link = "gev",
#'                     link_param = list(xi = "estimate"))
#' }
clm_stan <- function(formula,
                     data,
                     link = "logit",
                     base = "logit",
                     threshold = "flexible",
                     link_param = NULL,
                     prior = NULL,
                     chains = 4,
                     iter = 2000,
                     warmup = NULL,
                     ...) {
  # ===========================================================================
  # Step 0: Set warmup default if NULL
  # ===========================================================================

  if (is.null(warmup)) {
    warmup <- floor(iter / 2)
  }

  # Validate iter > warmup
  if (iter <= warmup) {
    stop(sprintf(
      "`iter` (%d) must be greater than `warmup` (%d).\n  Consider using the default warmup (floor(iter/2)) or increasing iter.",
      iter, warmup
    ))
  }

  # ===========================================================================
  # Step 1: Input validation
  # ===========================================================================

  # Validate link function
  validate_link(link)

  # Parse formula to get K (number of categories)
  parsed <- parse_clm_formula(formula, data)
  y <- parsed$response
  if (!is.factor(y)) {
    y <- factor(y)
  }

  K <- nlevels(y)

  # Validate threshold structure
  validate_threshold(threshold, K)

  # ===========================================================================
  # Step 2: Convert prior format if needed
  # ===========================================================================

  # clmstan supports two prior specification APIs:

  # 1. Legacy API: clm_prior(beta_sd = 2.5, c_sd = 10, ...)
  # 2. Distribution-based API: prior(normal(0, 2.5), class = "b")
  #
  # Internally, we use the legacy format for Stan data preparation.
  # Convert distribution-based priors to legacy format if needed.
  if (!is.null(prior)) {
    if (inherits(prior, c("clm_prior_spec", "clm_prior_list"))) {
      prior <- convert_prior_spec_to_legacy(prior)
    }
  }

  # ===========================================================================
  # Step 3: Select Stan model (base = fixed params, full = estimate params)
  # ===========================================================================

  # Check if any link parameter requires estimation
  full <- needs_full_model(link_param)

  # Get Stan model name
  model_name <- get_model_name(threshold, full)

  # ===========================================================================
  # Step 4: Prepare data for Stan
  # ===========================================================================

  stan_data <- prepare_stan_data_dispatch(
    formula = formula,
    data = data,
    link = link,
    base = base,
    link_param = link_param,
    threshold = threshold,
    full = full,
    prior = prior
  )

  # ===========================================================================
  # Step 5: Get Stan model and run sampling
  # ===========================================================================

  # Get the pre-compiled Stan model from the package
  model <- instantiate::stan_package_model(
    name = model_name,
    package = "clmstan"
  )

  # Set include_paths for Stan's include directives
  #
  # WORKAROUND: The instantiate package does not set include_paths on
  # the CmdStanModel object after compilation. Without this, cmdstanr's
  # stanc call fails with "file not found" errors when resolving
  # include directives in Stan files (e.g., functions/clm_common.stan).
  # This workaround accesses a private R6 field and may break if cmdstanr
  # changes its internal structure. See: INSTANTIATE_ISSUE.md
  stan_dir <- system.file("bin", "stan", package = "clmstan")
  tryCatch(
    {
      model$.__enclos_env__$private$include_paths_ <- stan_dir
    },
    error = function(e) {
      warning(
        "Failed to set include_paths on CmdStanModel. ",
        "This may be due to a cmdstanr version change. ",
        "If sampling fails, please report this issue to the clmstan maintainer.\n",
        "Original error: ", conditionMessage(e)
      )
    }
  )

  # Run MCMC sampling
  fit <- model$sample(
    data = stan_data,
    chains = chains,
    iter_sampling = iter - warmup,
    iter_warmup = warmup,
    ...
  )

  # ===========================================================================
  # Step 6: Create result object
  # ===========================================================================

  result <- new_clmstan(
    fit = fit,
    formula = formula,
    data = data,
    link = link,
    base = if (link == "sp") base else NA_character_,
    threshold = threshold,
    link_param = link_param,
    full = full,
    K = stan_data$K,
    N = stan_data$N,
    P = stan_data$P
  )

  result
}
