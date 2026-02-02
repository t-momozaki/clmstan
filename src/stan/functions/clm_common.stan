/**
 * Common functions for Cumulative Link Models (CLM)
 *
 * This file contains shared dispatcher and helper functions used by all CLM Stan models:
 * - Unified CDF dispatcher functions (unified_F, unified_logF, unified_log1mF)
 * - Log probability mass function (clm_lpmf)
 * - Random number generator (clm_rng)
 *
 * IMPORTANT: Link functions must be included BEFORE this file.
 *
 * Usage in Stan model files:
 *   functions {
 *     // First include all link functions
 *     #include functions/link_logit.stan
 *     #include functions/link_probit.stan
 *     #include functions/link_cloglog.stan
 *     #include functions/link_loglog.stan
 *     #include functions/link_cauchit.stan
 *     #include functions/link_tlink.stan
 *     #include functions/link_aranda_ordaz.stan
 *     #include functions/link_sp.stan
 *     #include functions/link_loggamma.stan
 *     #include functions/link_gev.stan
 *     #include functions/link_aep.stan
 *     // Then include common functions
 *     #include functions/clm_common.stan
 *   }
 */

/**
 * Unified CDF dispatcher
 * Returns F(x) for the specified link function
 *
 * @param x Argument value
 * @param link_type Link function type (1-11)
 * @param df Degrees of freedom (tlink, sp with t-base)
 * @param lambda Shape parameter (aranda_ordaz, log_gamma)
 * @param xi Shape parameter (gev)
 * @param r Power parameter (sp)
 * @param base_type Base distribution for SP (1-6)
 * @param theta1 First shape parameter (aep)
 * @param theta2 Second shape parameter (aep)
 * @return F(x) - CDF value
 */
real unified_F(real x, int link_type,
               real df, real lambda, real xi,
               real r, int base_type,
               real theta1, real theta2) {
  if (link_type == 1) {
    return logit_F(x);
  } else if (link_type == 2) {
    return probit_F(x);
  } else if (link_type == 3) {
    return cloglog_F(x);
  } else if (link_type == 4) {
    return loglog_F(x);
  } else if (link_type == 5) {
    return cauchit_F(x);
  } else if (link_type == 6) {
    return tlink_F(x, df);
  } else if (link_type == 7) {
    return aranda_ordaz_F(x, lambda);
  } else if (link_type == 8) {
    return sp_F(x, r, base_type, df);
  } else if (link_type == 9) {
    return loggamma_F(x, lambda);
  } else if (link_type == 10) {
    return gev_F(x, xi);
  } else {
    // link_type == 11: aep
    return aep_F(x, theta1, theta2);
  }
}

/**
 * Unified log-CDF dispatcher
 * Returns log(F(x)) for numerical stability
 *
 * @param x Argument value
 * @param link_type Link function type (1-11)
 * @param df, lambda, xi, r, base_type, theta1, theta2: Link parameters
 * @return log(F(x)) - log-CDF value
 */
real unified_logF(real x, int link_type,
                  real df, real lambda, real xi,
                  real r, int base_type,
                  real theta1, real theta2) {
  if (link_type == 1) {
    return logit_logF(x);
  } else if (link_type == 2) {
    return probit_logF(x);
  } else if (link_type == 3) {
    return cloglog_logF(x);
  } else if (link_type == 4) {
    return loglog_logF(x);
  } else if (link_type == 5) {
    return cauchit_logF(x);
  } else if (link_type == 6) {
    return tlink_logF(x, df);
  } else if (link_type == 7) {
    return aranda_ordaz_logF(x, lambda);
  } else if (link_type == 8) {
    return sp_logF(x, r, base_type, df);
  } else if (link_type == 9) {
    return loggamma_logF(x, lambda);
  } else if (link_type == 10) {
    return gev_logF(x, xi);
  } else {
    return aep_logF(x, theta1, theta2);
  }
}

/**
 * Unified log complementary CDF dispatcher
 * Returns log(1 - F(x)) for numerical stability
 *
 * @param x Argument value
 * @param link_type Link function type (1-11)
 * @param df, lambda, xi, r, base_type, theta1, theta2: Link parameters
 * @return log(1 - F(x)) - log-CCDF value
 */
real unified_log1mF(real x, int link_type,
                    real df, real lambda, real xi,
                    real r, int base_type,
                    real theta1, real theta2) {
  if (link_type == 1) {
    return logit_log1mF(x);
  } else if (link_type == 2) {
    return probit_log1mF(x);
  } else if (link_type == 3) {
    return cloglog_log1mF(x);
  } else if (link_type == 4) {
    return loglog_log1mF(x);
  } else if (link_type == 5) {
    return cauchit_log1mF(x);
  } else if (link_type == 6) {
    return tlink_log1mF(x, df);
  } else if (link_type == 7) {
    return aranda_ordaz_log1mF(x, lambda);
  } else if (link_type == 8) {
    return sp_log1mF(x, r, base_type, df);
  } else if (link_type == 9) {
    return loggamma_log1mF(x, lambda);
  } else if (link_type == 10) {
    return gev_log1mF(x, xi);
  } else {
    return aep_log1mF(x, theta1, theta2);
  }
}

/**
 * Log probability mass function for a single observation
 * Numerically stable implementation
 *
 * @param y Observed category (1 to K)
 * @param K Number of categories
 * @param c Cutpoints vector (length K-1)
 * @param eta Linear predictor
 * @param link_type Link function type (1-11)
 * @param df, lambda, xi, r, base_type, theta1, theta2: Link parameters
 * @return log P(Y = y)
 */
real clm_lpmf(int y, int K, vector c, real eta, int link_type,
              real df, real lambda, real xi,
              real r, int base_type,
              real theta1, real theta2) {
  if (y == 1) {
    // P(Y = 1) = F(c[1] - eta)
    return unified_logF(c[1] - eta, link_type,
                        df, lambda, xi, r, base_type, theta1, theta2);
  } else if (y == K) {
    // P(Y = K) = 1 - F(c[K-1] - eta)
    return unified_log1mF(c[K-1] - eta, link_type,
                          df, lambda, xi, r, base_type, theta1, theta2);
  } else {
    // P(Y = y) = F(c[y] - eta) - F(c[y-1] - eta)
    // Use log_diff_exp for numerical stability
    real lcdf_upper = unified_logF(c[y] - eta, link_type,
                                   df, lambda, xi, r, base_type, theta1, theta2);
    real lcdf_lower = unified_logF(c[y-1] - eta, link_type,
                                   df, lambda, xi, r, base_type, theta1, theta2);
    return log_diff_exp(lcdf_upper, lcdf_lower);
  }
}

/**
 * Random number generator for posterior predictive checks
 *
 * @param K Number of categories
 * @param c Cutpoints vector
 * @param eta Linear predictor
 * @param link_type Link function type
 * @param df, lambda, xi, r, base_type, theta1, theta2: Link parameters
 * @return Sampled category (1 to K)
 */
int clm_rng(int K, vector c, real eta, int link_type,
            real df, real lambda, real xi,
            real r, int base_type,
            real theta1, real theta2) {
  vector[K] probs;
  real prev_cdf = 0;

  for (k in 1:(K-1)) {
    real current_cdf = unified_F(c[k] - eta, link_type,
                                 df, lambda, xi, r, base_type, theta1, theta2);
    probs[k] = current_cdf - prev_cdf;
    prev_cdf = current_cdf;
  }
  probs[K] = 1 - prev_cdf;

  // Handle numerical issues: CDF subtraction can produce small negative values
  // due to floating-point errors, especially at extreme eta values.
  for (k in 1:K) {
    if (probs[k] < 0) probs[k] = 0;
  }
  probs = probs / sum(probs);  // Renormalize to ensure sum = 1

  return categorical_rng(probs);
}
