#' clmstan: Cumulative Link Models with CmdStanR
#'
#' @description
#' Fit cumulative link models (CLMs) for ordinal categorical data using
#' CmdStanR. The package supports various link functions including standard
#' links (logit, probit, cloglog, loglog, cauchit) and flexible parametric
#' links (GEV, AEP, Symmetric Power, Aranda-Ordaz, log-gamma).
#'
#' Models are pre-compiled using the instantiate package for fast execution
#' without runtime compilation.
#'
#' @section Main functions:
#' * [clm_stan()] - Fit a cumulative link model
#' * [supported_links()] - List available link functions
#'
#' @section Methods:
#' * [print.clmstan()] - Print summary
#' * [summary.clmstan()] - Detailed summary
#' * [coef.clmstan()] - Extract coefficients
#' * [predict.clmstan()] - Predict categories or probabilities
#' * [plot.clmstan()] - Diagnostic plots
#' * [loo.clmstan()] - Leave-one-out cross-validation
#'
#' @keywords internal
#'
#' @importFrom instantiate stan_package_model
#' @importFrom stats model.frame model.matrix delete.response terms
#' @importFrom stats plogis pnorm pcauchy pt pgamma
#'
#' @references
#' **Flexible Link Functions:**
#'
#' Aranda-Ordaz, F. J. (1981). On two families of transformations to additivity
#' for binary response data. *Biometrika*, 68(2), 357-363.
#'
#' Li, D., Wang, X., & Dey, D. K. (2019). Power link functions in an ordinal
#' regression model with Gaussian process priors. *Environmetrics*, 30(6), e2564.
#'
#' Prentice, R. L. (1976). A generalization of the probit and logit methods
#' for dose response curves. *Biometrics*, 32(4), 761-768.
#'
#' Wang, X. & Dey, D. K. (2011). Generalized extreme value regression for
#' ordinal response data. *Environmental and Ecological Statistics*, 18(4), 619-634.
#'
#' Naranjo, L., Pérez, C. J., & Martín, J. (2015). Bayesian analysis of some
#' models that use the asymmetric exponential power distribution.
#' *Statistics and Computing*, 25(3), 497-514.
#'
#' @seealso
#' Useful links:
#' * [instantiate package](https://wlandau.github.io/instantiate/)
#' * [cmdstanr](https://mc-stan.org/cmdstanr/)
#'
"_PACKAGE"
