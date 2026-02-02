/**
 * Symmetric Power (SP) link function for cumulative link models
 *
 * CDF (Li et al., 2019, Equation 5):
 * F_sp(x, r) = F_0^r(x/r)           if 0 < r <= 1
 * F_sp(x, r) = 1 - F_0^{1/r}(-r*x)  if r > 1
 *
 * where F_0 is a symmetric base distribution CDF.
 *
 * Skewness control:
 * - r = 1: equals base distribution F_0
 * - 0 < r < 1: positively skewed
 * - r > 1: negatively skewed
 *
 * Base distribution types (base_type):
 * - 1: logit (logistic)
 * - 2: probit (normal)
 * - 3: cauchit (Cauchy)
 * - 4: tlink (Student-t, requires df parameter)
 *
 * Note: Li et al. (2019) require F_0 to have a PDF "symmetric about 0".
 * Only symmetric distributions are supported as bases.
 *
 * Reference:
 * Li, D., Wang, X., & Dey, D. K. (2019). Power link functions in an ordinal
 * regression model with Gaussian process priors. Environmetrics, 30(6), e2564.
 */

/**
 * Helper: Get base distribution CDF value
 * Note: Using _F suffix to avoid Stan interpreting as probability function
 */
real sp_base_F(real x, int base_type, real df) {
  if (base_type == 1) {
    // logit (logistic)
    return inv_logit(x);
  } else if (base_type == 2) {
    // probit (normal)
    return Phi(x);
  } else if (base_type == 3) {
    // cauchit (Cauchy)
    return cauchy_cdf(x | 0, 1);
  } else {
    // base_type == 4: tlink (Student-t)
    return student_t_cdf(x | df, 0, 1);
  }
}

/**
 * Helper: Get base distribution log-CDF value
 * Note: Using _logF suffix to avoid Stan interpreting as probability function
 */
real sp_base_logF(real x, int base_type, real df) {
  if (base_type == 1) {
    // logit
    return log_inv_logit(x);
  } else if (base_type == 2) {
    // probit
    return std_normal_lcdf(x);
  } else if (base_type == 3) {
    // cauchit
    return cauchy_lcdf(x | 0, 1);
  } else {
    // tlink
    return student_t_lcdf(x | df, 0, 1);
  }
}

/**
 * Helper: Get base distribution log-CCDF value
 * Note: Using _log1mF suffix to avoid Stan interpreting as probability function
 */
real sp_base_log1mF(real x, int base_type, real df) {
  if (base_type == 1) {
    // logit
    return log1m_inv_logit(x);
  } else if (base_type == 2) {
    // probit
    return std_normal_lccdf(x);
  } else if (base_type == 3) {
    // cauchit
    return cauchy_lccdf(x | 0, 1);
  } else {
    // tlink
    return student_t_lccdf(x | df, 0, 1);
  }
}

/**
 * SP CDF
 * @param x Real value
 * @param r Power parameter (r > 0)
 * @param base_type Base distribution type (1-4: logit, probit, cauchit, tlink)
 * @param df Degrees of freedom for tlink (ignored for other bases)
 * @return F_sp(x; r)
 */
real sp_F(real x, real r, int base_type, real df) {
  if (r <= 1) {
    // F_sp = F_0^r(x/r)
    real F0 = sp_base_F(x / r, base_type, df);
    return pow(F0, r);
  } else {
    // F_sp = 1 - F_0^{1/r}(-r*x)
    real F0 = sp_base_F(-r * x, base_type, df);
    return 1 - pow(F0, 1.0 / r);
  }
}

/**
 * Log of SP CDF
 * @param x Real value
 * @param r Power parameter (r > 0)
 * @param base_type Base distribution type (1-4)
 * @param df Degrees of freedom for tlink
 * @return log(F_sp(x; r))
 */
real sp_logF(real x, real r, int base_type, real df) {
  if (r <= 1) {
    // log F_sp = r * log F_0(x/r)
    return r * sp_base_logF(x / r, base_type, df);
  } else {
    // log F_sp = log(1 - F_0^{1/r}(-r*x))
    //          = log1m_exp((1/r) * log F_0(-r*x))
    real log_F0 = sp_base_logF(-r * x, base_type, df);
    return log1m_exp(log_F0 / r);
  }
}

/**
 * Log of complementary SP CDF
 * @param x Real value
 * @param r Power parameter (r > 0)
 * @param base_type Base distribution type (1-4)
 * @param df Degrees of freedom for tlink
 * @return log(1 - F_sp(x; r))
 */
real sp_log1mF(real x, real r, int base_type, real df) {
  if (r <= 1) {
    // log(1 - F_sp) = log(1 - F_0^r(x/r))
    //              = log1m_exp(r * log F_0(x/r))
    real log_F0 = sp_base_logF(x / r, base_type, df);
    return log1m_exp(r * log_F0);
  } else {
    // log(1 - F_sp) = log(F_0^{1/r}(-r*x))
    //              = (1/r) * log F_0(-r*x)
    return sp_base_logF(-r * x, base_type, df) / r;
  }
}
