/**
 * Asymmetric Exponential Power (AEP) link function for cumulative link models
 *
 * Distribution: AEP (standardized: mu=0, sigma=1, alpha=0.5 fixed)
 *
 * The AEP distribution generalizes the normal distribution with independent
 * control over left and right tail behavior through shape parameters theta1 and theta2.
 *
 * CDF (standardized, alpha=0.5):
 * - F(x; theta1, theta2) for x <= 0:
 *     = 0.5 * (1 - P(1/theta1, u1))
 *     where u1 = (|x| * 2 * Gamma(1 + 1/theta1))^theta1
 *
 * - F(x; theta1, theta2) for x > 0:
 *     = 0.5 + 0.5 * P(1/theta2, u2)
 *     where u2 = (x * 2 * Gamma(1 + 1/theta2))^theta2
 *
 * where P(a, x) = gamma_cdf(x | a, 1) is the regularized lower incomplete gamma function.
 *
 * Special cases:
 * - theta1 = theta2: symmetric distribution
 * - theta = 2: Gaussian kernel (but NOT probit due to different scaling)
 * - theta = 1: Laplace (double exponential) kernel
 * - theta < 2: heavy tails (leptokurtic)
 * - theta > 2: light tails (platykurtic)
 *
 * Note: alpha is fixed at 0.5 for identifiability in cumulative link models.
 * This makes the distribution symmetric about x=0, with different tail shapes.
 *
 * Reference:
 * Naranjo, L., Pérez, C. J., & Martín, J. (2015). Bayesian analysis of some
 * models that use the asymmetric exponential power distribution.
 * Statistics and Computing, 25(3), 497-514.
 */

/**
 * AEP CDF (standardized, alpha=0.5 fixed)
 * @param x Real value
 * @param theta1 Left tail shape parameter (theta1 > 0)
 * @param theta2 Right tail shape parameter (theta2 > 0)
 * @return F(x; theta1, theta2)
 */
real aep_F(real x, real theta1, real theta2) {
  if (x <= 0) {
    // Left tail
    real a1 = 1.0 / theta1;
    real gamma1 = tgamma(1 + a1);  // Gamma(1 + 1/theta1)
    real z1 = abs(x) * 2 * gamma1;
    real u1 = pow(z1, theta1);
    // F(x) = 0.5 * (1 - gamma_cdf(u1 | 1/theta1, 1))
    return 0.5 * (1 - gamma_cdf(u1 | a1, 1));
  } else {
    // Right tail
    real a2 = 1.0 / theta2;
    real gamma2 = tgamma(1 + a2);  // Gamma(1 + 1/theta2)
    real z2 = x * 2 * gamma2;
    real u2 = pow(z2, theta2);
    // F(x) = 0.5 + 0.5 * gamma_cdf(u2 | 1/theta2, 1)
    return 0.5 + 0.5 * gamma_cdf(u2 | a2, 1);
  }
}

/**
 * Log of AEP CDF (standardized, alpha=0.5 fixed)
 * @param x Real value
 * @param theta1 Left tail shape parameter (theta1 > 0)
 * @param theta2 Right tail shape parameter (theta2 > 0)
 * @return log(F(x; theta1, theta2))
 */
real aep_logF(real x, real theta1, real theta2) {
  if (x <= 0) {
    // Left tail: log(0.5 * (1 - gamma_cdf(u1 | a1, 1)))
    real a1 = 1.0 / theta1;
    real gamma1 = tgamma(1 + a1);
    real z1 = abs(x) * 2 * gamma1;
    real u1 = pow(z1, theta1);
    // Use log1m_exp for numerical stability when gamma_cdf is close to 1
    real log_F1 = gamma_lccdf(u1 | a1, 1);  // log(1 - gamma_cdf)
    return log(0.5) + log_F1;
  } else {
    // Right tail: log(0.5 + 0.5 * gamma_cdf(u2 | a2, 1))
    real a2 = 1.0 / theta2;
    real gamma2 = tgamma(1 + a2);
    real z2 = x * 2 * gamma2;
    real u2 = pow(z2, theta2);
    real p = gamma_cdf(u2 | a2, 1);
    // log(0.5 * (1 + p)) = log(0.5) + log1p(p)
    // But p can be close to 0, so: log(0.5 + 0.5*p) = log(0.5) + log(1 + p)
    return log(0.5 + 0.5 * p);
  }
}

/**
 * Log of complementary AEP CDF (standardized, alpha=0.5 fixed)
 * @param x Real value
 * @param theta1 Left tail shape parameter (theta1 > 0)
 * @param theta2 Right tail shape parameter (theta2 > 0)
 * @return log(1 - F(x; theta1, theta2))
 */
real aep_log1mF(real x, real theta1, real theta2) {
  if (x <= 0) {
    // Left tail: log(1 - 0.5 * (1 - gamma_cdf(u1 | a1, 1)))
    //          = log(0.5 + 0.5 * gamma_cdf(u1 | a1, 1))
    real a1 = 1.0 / theta1;
    real gamma1 = tgamma(1 + a1);
    real z1 = abs(x) * 2 * gamma1;
    real u1 = pow(z1, theta1);
    real p = gamma_cdf(u1 | a1, 1);
    return log(0.5 + 0.5 * p);
  } else {
    // Right tail: log(1 - (0.5 + 0.5 * gamma_cdf(u2 | a2, 1)))
    //           = log(0.5 * (1 - gamma_cdf(u2 | a2, 1)))
    real a2 = 1.0 / theta2;
    real gamma2 = tgamma(1 + a2);
    real z2 = x * 2 * gamma2;
    real u2 = pow(z2, theta2);
    real log_1mF2 = gamma_lccdf(u2 | a2, 1);  // log(1 - gamma_cdf)
    return log(0.5) + log_1mF2;
  }
}
