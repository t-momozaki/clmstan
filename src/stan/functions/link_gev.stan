/**
 * Generalized Extreme Value (GEV) link function for cumulative link models
 *
 * Distribution: Generalized Extreme Value (standardized: mu=0, sigma=1)
 * CDF: F(x; xi) = exp(-(1 + xi*x)^(-1/xi))  for 1 + xi*x > 0
 *
 * Special cases:
 * - xi = 0: Gumbel (minimum) = loglog link, F(x) = exp(-exp(-x))
 * - xi < 0: Weibull type (light tail, bounded above at x = -1/xi)
 * - xi > 0: Frechet type (heavy tail, useful for outlier-prone data)
 *
 * Support:
 * - xi > 0: x > -1/xi
 * - xi = 0: x in (-inf, +inf)
 * - xi < 0: x < -1/xi
 *
 * Reference:
 * Wang, X. & Dey, D. K. (2011). Generalized extreme value regression for
 * ordinal response data. Environmental and Ecological Statistics, 18(4), 619-634.
 */

/**
 * GEV CDF (standardized)
 * @param x Real value
 * @param xi Shape parameter (real)
 * @return F(x; xi) = exp(-(1 + xi*x)^(-1/xi))
 */
real gev_F(real x, real xi) {
  if (abs(xi) < 1e-10) {
    // Gumbel case (xi -> 0): exp(-exp(-x))
    return exp(-exp(-x));
  } else {
    real t = 1 + xi * x;
    if (t <= 0) {
      // Outside support
      return xi > 0 ? 0.0 : 1.0;
    }
    return exp(-pow(t, -1.0 / xi));
  }
}

/**
 * Log of GEV CDF (standardized)
 * @param x Real value
 * @param xi Shape parameter (real)
 * @return log(F(x; xi))
 */
real gev_logF(real x, real xi) {
  if (abs(xi) < 1e-10) {
    // Gumbel case: log(exp(-exp(-x))) = -exp(-x)
    return -exp(-x);
  } else {
    real t = 1 + xi * x;
    if (t <= 0) {
      // Outside support
      return xi > 0 ? negative_infinity() : 0.0;
    }
    return -pow(t, -1.0 / xi);
  }
}

/**
 * Log of complementary GEV CDF (standardized)
 * @param x Real value
 * @param xi Shape parameter (real)
 * @return log(1 - F(x; xi))
 */
real gev_log1mF(real x, real xi) {
  if (abs(xi) < 1e-10) {
    // Gumbel case: log(1 - exp(-exp(-x)))
    return log1m_exp(-exp(-x));
  } else {
    real t = 1 + xi * x;
    if (t <= 0) {
      // Outside support
      return xi > 0 ? 0.0 : negative_infinity();
    }
    // log(1 - exp(-t^(-1/xi)))
    return log1m_exp(-pow(t, -1.0 / xi));
  }
}
