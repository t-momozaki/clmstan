/**
 * Log-log link function for cumulative link models
 *
 * Distribution: Gumbel (minimum)
 * CDF: F(x) = exp(-exp(-x))
 *
 * Relationship to cloglog: loglog_F(x) = 1 - cloglog_F(-x)
 */

/**
 * Gumbel (min) CDF
 * @param x Real value
 * @return F(x) = exp(-exp(-x))
 */
real loglog_F(real x) {
  return exp(-exp(-x));
}

/**
 * Log of Gumbel (min) CDF
 * @param x Real value
 * @return log(exp(-exp(-x))) = -exp(-x)
 */
real loglog_logF(real x) {
  return -exp(-x);
}

/**
 * Log of complementary Gumbel (min) CDF
 * Uses log1m_exp for numerical stability
 * @param x Real value
 * @return log(1 - exp(-exp(-x)))
 */
real loglog_log1mF(real x) {
  return log1m_exp(-exp(-x));
}
