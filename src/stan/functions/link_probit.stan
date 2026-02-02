/**
 * Probit link function for cumulative link models
 *
 * Distribution: Standard Normal
 * CDF: F(x) = Phi(x)
 */

/**
 * Standard Normal CDF
 * @param x Real value
 * @return Phi(x)
 */
real probit_F(real x) {
  return Phi(x);
}

/**
 * Log of Standard Normal CDF
 * @param x Real value
 * @return log(Phi(x))
 */
real probit_logF(real x) {
  return std_normal_lcdf(x);
}

/**
 * Log of complementary Standard Normal CDF
 * @param x Real value
 * @return log(1 - Phi(x))
 */
real probit_log1mF(real x) {
  return std_normal_lccdf(x);
}
