/**
 * Cumulative Link Model (CLM) - Symmetric Thresholds
 *
 * This model constrains thresholds to be symmetric around 0:
 *   c_k = -c_{K-k} for all k
 *
 * Examples:
 *   K=4 (3 thresholds): c = (-a, 0, a)
 *   K=5 (4 thresholds): c = (-b, -a, a, b)
 *   K=6 (5 thresholds): c = (-b, -a, 0, a, b)
 *
 * Supports 11 link functions with fixed parameters for flexible links.
 * For Bayesian inference of link parameters, use clm_symmetric_full.stan.
 *
 * RELATED FILES (update together when modifying data/parameters/model blocks):
 *   - clm_base.stan             (flexible thresholds)
 *   - clm_equidistant.stan      (equidistant thresholds)
 *   - clm_full.stan             (link parameter inference)
 *   - clm_equidistant_full.stan
 *   - clm_symmetric_full.stan
 *   - functions/clm_common.stan (shared functions - updates affect all models)
 *
 * Model:
 *   P(Y <= k | x) = F(c_k - eta)
 *   P(Y = k | x) = F(c_k - eta) - F(c_{k-1} - eta)
 *
 * where:
 *   F: CDF corresponding to the link function
 *   c_k: cutpoints (thresholds), c_0 = -Inf, c_K = +Inf
 *   eta = X * beta: linear predictor (no intercept during sampling)
 *
 * Symmetric thresholds:
 *   Only positive thresholds c_pos are estimated
 *   Negative thresholds are mirrors: c[k] = -c_pos[half - k + 1]
 *   Middle threshold (if K-1 odd) is fixed at 0
 *
 * No post-transformation needed:
 *   Thresholds are already centered at 0 by symmetry
 *   beta0 = 0 (intercept is implicitly 0)
 *
 * Link functions (link_type):
 *   1: logit, 2: probit, 3: cloglog, 4: loglog, 5: cauchit
 *   6: tlink, 7: aranda_ordaz, 8: sp, 9: log_gamma, 10: gev, 11: aep
 */

functions {
  // Include all link functions first
  #include functions/link_logit.stan
  #include functions/link_probit.stan
  #include functions/link_cloglog.stan
  #include functions/link_loglog.stan
  #include functions/link_cauchit.stan
  #include functions/link_tlink.stan
  #include functions/link_aranda_ordaz.stan
  #include functions/link_sp.stan
  #include functions/link_loggamma.stan
  #include functions/link_gev.stan
  #include functions/link_aep.stan
  // Then include common functions (dispatchers, clm_lpmf, clm_rng)
  #include functions/clm_common.stan
}

data {
  // Data dimensions
  int<lower=3> K;                          // Number of ordinal categories (>= 3 for symmetric)
  int<lower=1> N;                          // Number of observations
  int<lower=0> P;                          // Number of predictors (excluding intercept)

  // Response and predictors
  array[N] int<lower=1, upper=K> y;        // Ordinal response
  matrix[N, P] X;                          // Design matrix (without intercept column)

  // Link function specification
  int<lower=1, upper=11> link_type;        // Link function type

  // Link parameters (used only for flexible links)
  real<lower=0> df;                        // tlink, sp(t-base)
  real lambda;                             // aranda_ordaz (>0), log_gamma
  real xi;                                 // gev
  real<lower=0> r;                         // sp
  int<lower=1, upper=6> base_type;         // sp base distribution
  real<lower=0> theta1;                    // aep
  real<lower=0> theta2;                    // aep

  // Prior hyperparameters for beta
  // Type: 1=normal, 2=student_t, 3=cauchy, 4=flat (improper uniform)
  int<lower=1, upper=4> prior_beta_type;
  real prior_beta_mu;                      // location (unused for flat)
  real<lower=0> prior_beta_sd;             // scale (unused for flat)
  real<lower=0> prior_beta_df;             // df for student_t (unused for others)

  // Prior hyperparameters for symmetric thresholds
  // cpos prior: Type: 1=normal, 2=student_t, 3=cauchy, 4=flat (improper uniform)
  // Note: c_pos is positive_ordered, so these act as half-normal/half-t/half-cauchy
  int<lower=1, upper=4> prior_cpos_type;
  real<lower=0> prior_cpos_sd;             // scale (unused for flat)
  real<lower=0> prior_cpos_df;             // df for student_t (unused for others)
}

transformed data {
  // Number of positive thresholds to estimate
  int half = (K - 1) / 2;
  // Is there a middle threshold at 0? (when K-1 is odd)
  int has_middle = (K - 1) % 2;
}

parameters {
  // Regression coefficients (may be empty if P = 0)
  vector[P] beta;

  // Positive thresholds only (positive_ordered ensures 0 < c_pos[1] < c_pos[2] < ...)
  positive_ordered[half] c_pos;
}

transformed parameters {
  // Linear predictor (no intercept during sampling)
  vector[N] eta;
  if (P > 0) {
    eta = X * beta;
  } else {
    eta = rep_vector(0, N);
  }

  // Construct symmetric cutpoints
  // For K=5 (4 thresholds): c = [-c_pos[2], -c_pos[1], c_pos[1], c_pos[2]]
  // For K=4 (3 thresholds): c = [-c_pos[1], 0, c_pos[1]]
  vector[K-1] c;
  for (k in 1:half) {
    c[k] = -c_pos[half - k + 1];        // Negative thresholds (from most negative to least)
    c[K - k] = c_pos[half - k + 1];     // Positive thresholds (from least positive to most)
  }
  if (has_middle == 1) {
    c[half + 1] = 0;  // Middle threshold at 0
  }
}

model {
  // Priors for regression coefficients
  if (P > 0) {
    if (prior_beta_type == 1) {
      beta ~ normal(prior_beta_mu, prior_beta_sd);
    } else if (prior_beta_type == 2) {
      beta ~ student_t(prior_beta_df, prior_beta_mu, prior_beta_sd);
    } else if (prior_beta_type == 3) {
      beta ~ cauchy(prior_beta_mu, prior_beta_sd);
    }
    // prior_beta_type == 4: flat, no prior statement
  }

  // Priors for symmetric thresholds (positive_ordered truncates to positive)
  // Acts as half-normal/half-t/half-cauchy due to positive constraint
  if (prior_cpos_type == 1) {
    c_pos ~ normal(0, prior_cpos_sd);
  } else if (prior_cpos_type == 2) {
    c_pos ~ student_t(prior_cpos_df, 0, prior_cpos_sd);
  } else if (prior_cpos_type == 3) {
    c_pos ~ cauchy(0, prior_cpos_sd);
  }
  // prior_cpos_type == 4: flat (improper uniform on positive reals), no prior statement needed

  // Likelihood
  for (n in 1:N) {
    target += clm_lpmf(y[n] | K, c, eta[n], link_type,
                       df, lambda, xi, r, base_type, theta1, theta2);
  }
}

generated quantities {
  // No post-transformation needed for symmetric thresholds
  // The thresholds are already centered at 0 by construction
  vector[K-1] c_transformed = c;
  real beta0 = 0;  // Intercept is 0 by symmetry

  // Log-likelihood for LOO-CV
  vector[N] log_lik;

  // Posterior predictive samples
  array[N] int<lower=1, upper=K> y_rep;

  for (n in 1:N) {
    log_lik[n] = clm_lpmf(y[n] | K, c, eta[n], link_type,
                          df, lambda, xi, r, base_type, theta1, theta2);
    y_rep[n] = clm_rng(K, c, eta[n], link_type,
                       df, lambda, xi, r, base_type, theta1, theta2);
  }
}
