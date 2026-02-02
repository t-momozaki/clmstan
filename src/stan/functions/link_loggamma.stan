/**
 * Log-gamma link function for cumulative link models
 *
 * Distribution: Log-gamma (generalization of probit/Gumbel)
 * CDF:
 * - F(x; lambda) = gamma_cdf(exp(x) | lambda, lambda)      if lambda > 0
 * - F(x; lambda) = Phi(x)                                   if lambda = 0
 * - F(x; lambda) = 1 - gamma_cdf(exp(-x) | -lambda, -lambda) if lambda < 0
 *
 * Special cases:
 * - lambda = 0: approaches probit link (normal CDF)
 * - lambda = 1: approaches cloglog-like behavior
 * - lambda = -1: approaches loglog-like behavior
 *
 * The log-gamma family provides a continuous bridge between symmetric
 * (probit-like) and asymmetric (Gumbel-like) link functions.
 *
 * Reference:
 * Prentice, R. L. (1976). A generalization of the probit and logit methods
 * for dose response curves. Biometrics, 32(4), 761-768.
 */

/**
 * Log-gamma CDF
 * @param x Real value
 * @param lambda Shape parameter (real)
 * @return F(x; lambda)
 */
real loggamma_F(real x, real lambda) {
  if (abs(lambda) < 1e-10) {
    // lambda = 0: probit link
    return Phi(x);
  } else if (lambda > 0) {
    // F(x) = gamma_cdf(exp(x) | lambda, lambda)
    return gamma_cdf(exp(x) | lambda, lambda);
  } else {
    // lambda < 0: F(x) = 1 - gamma_cdf(exp(-x) | -lambda, -lambda)
    return 1 - gamma_cdf(exp(-x) | -lambda, -lambda);
  }
}

/**
 * Log of Log-gamma CDF
 * @param x Real value
 * @param lambda Shape parameter (real)
 * @return log(F(x; lambda))
 */
real loggamma_logF(real x, real lambda) {
  if (abs(lambda) < 1e-10) {
    // lambda = 0: probit
    return std_normal_lcdf(x);
  } else if (lambda > 0) {
    // log(gamma_cdf(exp(x) | lambda, lambda))
    return gamma_lcdf(exp(x) | lambda, lambda);
  } else {
    // lambda < 0: log(1 - gamma_cdf(exp(-x) | -lambda, -lambda))
    return gamma_lccdf(exp(-x) | -lambda, -lambda);
  }
}

/**
 * Log of complementary Log-gamma CDF
 * @param x Real value
 * @param lambda Shape parameter (real)
 * @return log(1 - F(x; lambda))
 */
real loggamma_log1mF(real x, real lambda) {
  if (abs(lambda) < 1e-10) {
    // lambda = 0: probit
    return std_normal_lccdf(x);
  } else if (lambda > 0) {
    // log(1 - gamma_cdf(exp(x) | lambda, lambda))
    return gamma_lccdf(exp(x) | lambda, lambda);
  } else {
    // lambda < 0: log(gamma_cdf(exp(-x) | -lambda, -lambda))
    return gamma_lcdf(exp(-x) | -lambda, -lambda);
  }
}
