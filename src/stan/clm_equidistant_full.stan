/**
 * Cumulative Link Model (CLM) - Equidistant Thresholds with Link Parameter Inference
 *
 * Combines equidistant threshold structure with Bayesian inference of link parameters.
 *
 * Equidistant thresholds: c_k = c_1 + (k - 1) * d
 *
 * RELATED FILES (update together when modifying data/parameters/model blocks):
 *   - clm_base.stan             (flexible thresholds, fixed link params)
 *   - clm_equidistant.stan      (equidistant, fixed link params)
 *   - clm_symmetric.stan        (symmetric thresholds)
 *   - clm_full.stan             (flexible thresholds, link inference)
 *   - clm_symmetric_full.stan
 *   - functions/clm_common.stan (shared functions - updates affect all models)
 *
 * Model:
 *   P(Y <= k | x) = F(c_k - eta)
 *   P(Y = k | x) = F(c_k - eta) - F(c_{k-1} - eta)
 *
 * Link functions (link_type):
 *   1: logit, 2: probit, 3: cloglog, 4: loglog, 5: cauchit
 *   6: tlink, 7: aranda_ordaz, 8: sp, 9: log_gamma, 10: gev, 11: aep
 *
 * Estimable link parameters:
 *   - df: tlink, sp(t-base) [constraint: > 0, prior: gamma]
 *   - lambda: aranda_ordaz [> 0, gamma], log_gamma [unconstrained, normal]
 *   - xi: gev [unconstrained, normal]
 *   - r: sp [> 0, gamma]
 *   - theta1, theta2: aep [> 0, gamma]
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

  // SP base distribution (always fixed, not estimated)
  int<lower=1, upper=6> base_type;         // SP base distribution

  // Estimation flags (0 = fixed, 1 = estimate)
  int<lower=0, upper=1> estimate_df;
  int<lower=0, upper=1> estimate_lambda;
  int<lower=0, upper=1> estimate_xi;
  int<lower=0, upper=1> estimate_r;
  int<lower=0, upper=1> estimate_theta1;
  int<lower=0, upper=1> estimate_theta2;

  // Fixed values (used when estimate_* = 0)
  real<lower=0> df_fixed;
  real lambda_fixed;
  real xi_fixed;
  real<lower=0> r_fixed;
  real<lower=0> theta1_fixed;
  real<lower=0> theta2_fixed;

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

  // Prior hyperparameters for link parameters
  real<lower=0> prior_df_alpha;
  real<lower=0> prior_df_beta;
  real<lower=0> prior_lambda_ao_alpha;
  real<lower=0> prior_lambda_ao_beta;
  real prior_lambda_lg_mu;
  real<lower=0> prior_lambda_lg_sd;
  real prior_xi_mu;
  real<lower=0> prior_xi_sd;
  real<lower=0> prior_r_alpha;
  real<lower=0> prior_r_beta;
  real<lower=0> prior_theta1_alpha;
  real<lower=0> prior_theta1_beta;
  real<lower=0> prior_theta2_alpha;
  real<lower=0> prior_theta2_beta;
}

transformed data {
  int use_lambda_ao = (link_type == 7) ? 1 : 0;
  int use_lambda_lg = (link_type == 9) ? 1 : 0;
}

parameters {
  // Regression coefficients (may be empty if P = 0)
  vector[P] beta;

  // Equidistant threshold parameters
  real c1;                // First threshold
  real<lower=0> d;        // Spacing between thresholds

  // Conditional link parameters
  array[estimate_df] real<lower=0> df_raw;
  array[estimate_lambda * use_lambda_ao] real<lower=0> lambda_ao_raw;
  array[estimate_lambda * use_lambda_lg] real lambda_lg_raw;
  array[estimate_xi] real xi_raw;
  array[estimate_r] real<lower=0> r_raw;
  array[estimate_theta1] real<lower=0> theta1_raw;
  array[estimate_theta2] real<lower=0> theta2_raw;
}

transformed parameters {
  // Linear predictor
  vector[N] eta;
  if (P > 0) {
    eta = X * beta;
  } else {
    eta = rep_vector(0, N);
  }

  // Construct equidistant cutpoints
  vector[K-1] c;
  for (k in 1:(K-1)) {
    c[k] = c1 + (k - 1) * d;
  }

  // Unified link parameters
  real df = estimate_df ? df_raw[1] : df_fixed;

  real lambda;
  if (link_type == 7) {
    lambda = estimate_lambda ? lambda_ao_raw[1] : lambda_fixed;
  } else if (link_type == 9) {
    lambda = estimate_lambda ? lambda_lg_raw[1] : lambda_fixed;
  } else {
    lambda = lambda_fixed;
  }

  real xi = estimate_xi ? xi_raw[1] : xi_fixed;
  real r = estimate_r ? r_raw[1] : r_fixed;
  real theta1 = estimate_theta1 ? theta1_raw[1] : theta1_fixed;
  real theta2 = estimate_theta2 ? theta2_raw[1] : theta2_fixed;
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

  // Priors for link parameters (only if being estimated)
  if (estimate_df) {
    df_raw[1] ~ gamma(prior_df_alpha, prior_df_beta);
  }
  if (estimate_lambda && link_type == 7) {
    lambda_ao_raw[1] ~ gamma(prior_lambda_ao_alpha, prior_lambda_ao_beta);
  }
  if (estimate_lambda && link_type == 9) {
    lambda_lg_raw[1] ~ normal(prior_lambda_lg_mu, prior_lambda_lg_sd);
  }
  if (estimate_xi) {
    xi_raw[1] ~ normal(prior_xi_mu, prior_xi_sd);
  }
  if (estimate_r) {
    r_raw[1] ~ gamma(prior_r_alpha, prior_r_beta);
  }
  if (estimate_theta1) {
    theta1_raw[1] ~ gamma(prior_theta1_alpha, prior_theta1_beta);
  }
  if (estimate_theta2) {
    theta2_raw[1] ~ gamma(prior_theta2_alpha, prior_theta2_beta);
  }

  // Likelihood
  for (n in 1:N) {
    target += clm_lpmf(y[n] | K, c, eta[n], link_type,
                       df, lambda, xi, r, base_type, theta1, theta2);
  }
}

generated quantities {
  // Post-transformation
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

  for (n in 1:N) {
    log_lik[n] = clm_lpmf(y[n] | K, c, eta[n], link_type,
                          df, lambda, xi, r, base_type, theta1, theta2);
    y_rep[n] = clm_rng(K, c, eta[n], link_type,
                       df, lambda, xi, r, base_type, theta1, theta2);
  }
}
