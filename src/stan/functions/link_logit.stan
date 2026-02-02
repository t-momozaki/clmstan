/**
 * Logit link function for cumulative link models
 *
 * Distribution: Standard Logistic
 * CDF: F(x) = 1 / (1 + exp(-x))
 */

/**
 * Logistic CDF
 * @param x Real value
 * @return F(x) = 1 / (1 + exp(-x))
 */
real logit_F(real x) {
  return inv_logit(x);
}

/**
 * Log of Logistic CDF
 * @param x Real value
 * @return log(F(x))
 */
real logit_logF(real x) {
  return log_inv_logit(x);
}

/**
 * Log of complementary Logistic CDF
 * @param x Real value
 * @return log(1 - F(x))
 */
real logit_log1mF(real x) {
  return log1m_inv_logit(x);
}
