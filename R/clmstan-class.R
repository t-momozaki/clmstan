#' clmstan S3 Class
#'
#' @description
#' The `clmstan` class represents a fitted cumulative link model.
#' It contains the CmdStanR fit object and additional metadata.
#'
#' @section Slots:
#' \describe{
#'   \item{fit}{The CmdStanMCMC object from cmdstanr}
#'   \item{formula}{The model formula}
#'   \item{data}{The original data frame}
#'   \item{link}{The link function used}
#'   \item{base}{The base distribution (for SP link)}
#'   \item{threshold}{The threshold structure}
#'   \item{link_param}{Link parameter settings (for flexible links)}
#'   \item{full}{TRUE if link parameters were estimated (Bayesian inference),
#'     FALSE if they were fixed at user-specified values}
#'   \item{K}{Number of response categories (cached from data)}
#'   \item{N}{Number of observations (extracted from data for efficiency)}
#'   \item{P}{Number of predictors (extracted from design matrix)}
#' }
#'
#' @name clmstan-class
#' @family clmstan methods
NULL

#' Create a clmstan object (internal constructor)
#'
#' This is an internal constructor called by clm_stan(). Users should not
#' call this function directly.
#'
#' @param fit A CmdStanMCMC object
#' @param formula The model formula
#' @param data The original data
#' @param link The link function
#' @param base The base distribution (for SP link)
#' @param threshold The threshold structure
#' @param link_param Link parameter settings
#' @param full Whether full model was used
#' @param K Number of categories
#' @param N Number of observations
#' @param P Number of predictors
#'
#' @return An object of class "clmstan"
#' @keywords internal
new_clmstan <- function(fit, formula, data, link, base, threshold,
                        link_param = NULL, full = FALSE, K, N, P) {
  structure(
    list(
      fit = fit,
      formula = formula,
      data = data,
      link = link,
      base = base,
      threshold = threshold,
      link_param = link_param,
      full = full,
      K = K,
      N = N,
      P = P
    ),
    class = "clmstan"
  )
}

#' Check if object is clmstan
#'
#' @param x An object to test
#' @return TRUE if x is a clmstan object
#' @export
is.clmstan <- function(x) {
  inherits(x, "clmstan")
}
