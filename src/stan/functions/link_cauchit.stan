/**
 * Cauchit link function for cumulative link models
 *
 * Distribution: Standard Cauchy
 * CDF: F(x) = 0.5 + arctan(x) / pi
 */

/**
 * Standard Cauchy CDF
 * @param x Real value
 * @return F(x) = 0.5 + arctan(x) / pi
 */
real cauchit_F(real x) {
  return cauchy_cdf(x | 0, 1);
}

/**
 * Log of Standard Cauchy CDF
 * @param x Real value
 * @return log(F(x))
 */
real cauchit_logF(real x) {
  return cauchy_lcdf(x | 0, 1);
}

/**
 * Log of complementary Standard Cauchy CDF
 * @param x Real value
 * @return log(1 - F(x))
 */
real cauchit_log1mF(real x) {
  return cauchy_lccdf(x | 0, 1);
}
