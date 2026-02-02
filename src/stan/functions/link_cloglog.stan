/**
 * Complementary log-log link function for cumulative link models
 *
 * Distribution: Gumbel (maximum)
 * CDF: F(x) = 1 - exp(-exp(x))
 *
 * Also known as: cloglog, Gompertz link
 * Proportional hazards interpretation
 */

/**
 * Gumbel (max) CDF
 * @param x Real value
 * @return F(x) = 1 - exp(-exp(x))
 */
real cloglog_F(real x) {
  return inv_cloglog(x);
}

/**
 * Log of Gumbel (max) CDF
 * Uses log1m_exp for numerical stability
 * @param x Real value
 * @return log(1 - exp(-exp(x)))
 */
real cloglog_logF(real x) {
  return log1m_exp(-exp(x));
}

/**
 * Log of complementary Gumbel (max) CDF
 * @param x Real value
 * @return log(exp(-exp(x))) = -exp(x)
 */
real cloglog_log1mF(real x) {
  return -exp(x);
}
