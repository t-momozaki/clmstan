/**
 * Cumulative Link Model (CLM) - Equidistant Thresholds
 *
 * This model constrains thresholds to be equally spaced:
 *   c_k = c_1 + (k - 1) * d
 *
 * Supports 11 link functions with fixed parameters for flexible links.
 * For Bayesian inference of link parameters, use clm_equidistant_full.stan.
 *
 * RELATED FILES (update together when modifying data/parameters/model blocks):
 *   - clm_base.stan             (flexible thresholds)
 *   - clm_symmetric.stan        (symmetric thresholds)
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
 * Equidistant thresholds:
 *   c_k = c_1 + (k - 1) * d, where d > 0 is the spacing
 *
 * Post-transformation (generated quantities):
 *   c'_k = c_k - c_1 (so c'_1 = 0)
 *   beta0 = -c_1 (intercept)
 *   Result: c' = (0, d, 2d, 3d, ...)
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
  int<lower=2> K;                          // Number of ordinal categories
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

  // Prior hyperparameters for equidistant thresholds
  // c1 prior: Type: 1=normal, 2=student_t, 3=cauchy, 4=flat (improper uniform)
  int<lower=1, upper=4> prior_c1_type;
  real prior_c1_mu;                        // location (unused for flat)
  real<lower=0> prior_c1_sd;               // scale (unused for flat)
  real<lower=0> prior_c1_df;               // df for student_t (unused for others)
  // d prior: gamma distribution (d > 0)
  real<lower=0> prior_d_alpha;             // Gamma shape for d prior
  real<lower=0> prior_d_beta;              // Gamma rate for d prior
}

transformed data {
  // Note: For K=2, there's only one threshold c[1], so d is not identifiable
  // (no second threshold to determine the interval).
}

parameters {
  // Regression coefficients (may be empty if P = 0)
  vector[P] beta;

  // Equidistant threshold parameters
  real c1;                // First threshold
  real<lower=0> d;        // Spacing between thresholds
}

transformed parameters {
  // Linear predictor (no intercept during sampling)
  vector[N] eta;
  if (P > 0) {
    eta = X * beta;
  } else {
    eta = rep_vector(0, N);
  }

  // Construct equidistant cutpoints: c_k = c1 + (k-1) * d
  vector[K-1] c;
  for (k in 1:(K-1)) {
    c[k] = c1 + (k - 1) * d;
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

  // Priors for equidistant thresholds
  // c1: first threshold (location parameter)
  if (prior_c1_type == 1) {
    c1 ~ normal(prior_c1_mu, prior_c1_sd);
  } else if (prior_c1_type == 2) {
    c1 ~ student_t(prior_c1_df, prior_c1_mu, prior_c1_sd);
  } else if (prior_c1_type == 3) {
    c1 ~ cauchy(prior_c1_mu, prior_c1_sd);
  }
  // prior_c1_type == 4: flat (improper uniform), no prior statement needed

  // d: interval between thresholds (must be positive)
  d ~ gamma(prior_d_alpha, prior_d_beta);

  // Likelihood
  for (n in 1:N) {
    target += clm_lpmf(y[n] | K, c, eta[n], link_type,
                       df, lambda, xi, r, base_type, theta1, theta2);
  }
}

generated quantities {
  // Post-transformation: c'_k = c_k - c_1 (so c'_1 = 0), beta0 = -c_1
  // For equidistant: c'_k = (k-1) * d, i.e., c' = (0, d, 2d, ...)
  vector[K-1] c_transformed;
  real beta0;

  // Log-likelihood for LOO-CV
  vector[N] log_lik;

  // Posterior predictive samples
  array[N] int<lower=1, upper=K> y_rep;

  // Compute transformed cutpoints and intercept
  beta0 = -c[1];
  for (k in 1:(K-1)) {
    c_transformed[k] = c[k] - c[1];
  }
  // c_transformed = (0, d, 2d, ..., (K-2)*d) by construction

  for (n in 1:N) {
    log_lik[n] = clm_lpmf(y[n] | K, c, eta[n], link_type,
                          df, lambda, xi, r, base_type, theta1, theta2);
    y_rep[n] = clm_rng(K, c, eta[n], link_type,
                       df, lambda, xi, r, base_type, theta1, theta2);
  }
}
