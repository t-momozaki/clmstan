/**
 * Aranda-Ordaz asymmetric link function for cumulative link models
 *
 * Distribution: Aranda-Ordaz asymmetric (generalization of logit/cloglog)
 * CDF: F(x; lambda) = 1 - (1 + exp(x))^(-lambda)
 *
 * Special cases:
 * - lambda = 1: logit link (logistic CDF)
 * - lambda -> 0: approaches cloglog link (Gumbel max CDF)
 *
 * Reference:
 * Aranda-Ordaz, F. J. (1981). On two families of transformations to additivity
 * for binary response data. Biometrika, 68(2), 357-363.
 */

/**
 * Aranda-Ordaz asymmetric CDF
 * @param x Real value
 * @param lambda Shape parameter (lambda > 0)
 * @return F(x; lambda) = 1 - (1 + exp(x))^(-lambda)
 */
real aranda_ordaz_F(real x, real lambda) {
  // F(x) = 1 - (1 + exp(x))^(-lambda)
  //      = 1 - exp(-lambda * log(1 + exp(x)))
  // Use log1p_exp for numerical stability: log(1 + exp(x))
  return 1 - exp(-lambda * log1p_exp(x));
}

/**
 * Log of Aranda-Ordaz asymmetric CDF
 * @param x Real value
 * @param lambda Shape parameter (lambda > 0)
 * @return log(F(x; lambda))
 */
real aranda_ordaz_logF(real x, real lambda) {
  // log(F(x)) = log(1 - exp(-lambda * log1p_exp(x)))
  // Use log1m_exp for numerical stability: log(1 - exp(a))
  return log1m_exp(-lambda * log1p_exp(x));
}

/**
 * Log of complementary Aranda-Ordaz asymmetric CDF
 * @param x Real value
 * @param lambda Shape parameter (lambda > 0)
 * @return log(1 - F(x; lambda))
 */
real aranda_ordaz_log1mF(real x, real lambda) {
  // log(1 - F(x)) = log((1 + exp(x))^(-lambda))
  //               = -lambda * log(1 + exp(x))
  return -lambda * log1p_exp(x);
}
