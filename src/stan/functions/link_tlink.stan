/**
 * Student-t link function for cumulative link models
 *
 * Distribution: Standard Student-t with df degrees of freedom
 * CDF: F(x; df) = T_df(x)
 *
 * Special cases:
 * - df -> infinity: approaches probit (normal)
 * - df > 30: nearly indistinguishable from probit
 * - df = 1: Cauchy (cauchit)
 * - df < 3: increasingly heavy tails
 */

/**
 * Standard Student-t CDF
 * @param x Real value
 * @param df Degrees of freedom (positive real)
 * @return T_df(x)
 */
real tlink_F(real x, real df) {
  return student_t_cdf(x | df, 0, 1);
}

/**
 * Log of Standard Student-t CDF
 * @param x Real value
 * @param df Degrees of freedom (positive real)
 * @return log(T_df(x))
 */
real tlink_logF(real x, real df) {
  return student_t_lcdf(x | df, 0, 1);
}

/**
 * Log of complementary Standard Student-t CDF
 * @param x Real value
 * @param df Degrees of freedom (positive real)
 * @return log(1 - T_df(x))
 */
real tlink_log1mF(real x, real df) {
  return student_t_lccdf(x | df, 0, 1);
}
