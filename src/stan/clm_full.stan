/**
 * Cumulative Link Model (CLM) - Flexible Model with Link Parameter Inference
 *
 * Extends clm_base.stan to support Bayesian inference of link parameters.
 * Each link parameter can be either fixed or estimated.
 *
 * RELATED FILES (update together when modifying data/parameters/model blocks):
 *   - clm_base.stan             (flexible thresholds, fixed link params)
 *   - clm_equidistant.stan      (equidistant thresholds)
 *   - clm_symmetric.stan        (symmetric thresholds)
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
 * Identification constraint:
 *   No intercept + post-transformation approach (same as ordinal::clm)
 *   - Sampling: Standard ordered[K-1] cutpoints, no intercept
 *   - Post-processing: Transform to c'_k = c_k - c_1 (so c'_1 = 0)
 *                      and beta0 = -c_1 (intercept)
 *
 * Link functions (link_type):
 *   1: logit (logistic)
 *   2: probit (normal)
 *   3: cloglog (complementary log-log, Gumbel max)
 *   4: loglog (log-log, Gumbel min)
 *   5: cauchit (Cauchy)
 *   6: tlink (Student-t, requires df)
 *   7: aranda_ordaz (Aranda-Ordaz asymmetric, requires lambda > 0)
 *   8: sp (Symmetric Power, requires r, base_type, df)
 *   9: log_gamma (Log-gamma, requires lambda)
 *   10: gev (Generalized Extreme Value, requires xi)
 *   11: aep (Asymmetric Exponential Power, requires theta1, theta2)
 *
 * Estimable link parameters:
 *   - df: tlink, sp(t-base) [constraint: > 0, prior: gamma]
 *   - lambda: aranda_ordaz [constraint: > 0, prior: gamma]
 *             log_gamma [unconstrained, prior: normal]
 *   - xi: gev [unconstrained, prior: normal]
 *   - r: sp [constraint: > 0, prior: gamma]
 *   - theta1, theta2: aep [constraint: > 0, prior: gamma]
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
  int<lower=0, upper=1> estimate_df;       // For tlink, sp(t-base)
  int<lower=0, upper=1> estimate_lambda;   // For aranda_ordaz, log_gamma
  int<lower=0, upper=1> estimate_xi;       // For gev
  int<lower=0, upper=1> estimate_r;        // For sp
  int<lower=0, upper=1> estimate_theta1;   // For aep
  int<lower=0, upper=1> estimate_theta2;   // For aep

  // Fixed values (used when estimate_* = 0)
  real<lower=0> df_fixed;                  // tlink, sp(t-base): must be > 0
  real lambda_fixed;                       // aranda_ordaz (>0), log_gamma (any)
  real xi_fixed;                           // gev
  real<lower=0> r_fixed;                   // sp: must be > 0
  real<lower=0> theta1_fixed;              // aep: must be > 0
  real<lower=0> theta2_fixed;              // aep: must be > 0

  // Prior hyperparameters for beta
  // Type: 1=normal, 2=student_t, 3=cauchy, 4=flat (improper uniform)
  int<lower=1, upper=4> prior_beta_type;
  real prior_beta_mu;                      // location (unused for flat)
  real<lower=0> prior_beta_sd;             // scale (unused for flat)
  real<lower=0> prior_beta_df;             // df for student_t (unused for others)

  // Prior hyperparameters for cutpoints (thresholds)
  // Type: 1=normal, 2=student_t, 3=cauchy, 4=flat (improper uniform)
  int<lower=1, upper=4> prior_c_type;
  real prior_c_mu;                         // location (unused for flat)
  real<lower=0> prior_c_sd;                // scale (unused for flat)
  real<lower=0> prior_c_df;                // df for student_t (unused for others)

  // Prior hyperparameters for link parameters
  // df: gamma(alpha, beta) prior
  real<lower=0> prior_df_alpha;
  real<lower=0> prior_df_beta;

  // lambda for aranda_ordaz: gamma(alpha, beta) prior (constrained > 0)
  real<lower=0> prior_lambda_ao_alpha;
  real<lower=0> prior_lambda_ao_beta;

  // lambda for log_gamma: normal(mu, sd) prior (unconstrained)
  real prior_lambda_lg_mu;
  real<lower=0> prior_lambda_lg_sd;

  // xi: normal(mu, sd) prior
  real prior_xi_mu;
  real<lower=0> prior_xi_sd;

  // r: gamma(alpha, beta) prior
  real<lower=0> prior_r_alpha;
  real<lower=0> prior_r_beta;

  // theta1, theta2: gamma(alpha, beta) priors
  real<lower=0> prior_theta1_alpha;
  real<lower=0> prior_theta1_beta;
  real<lower=0> prior_theta2_alpha;
  real<lower=0> prior_theta2_beta;
}

transformed data {
  // Determine which lambda parameterization to use based on link_type
  // link_type == 7: aranda_ordaz (lambda > 0)
  // link_type == 9: log_gamma (lambda unconstrained)
  int use_lambda_ao = (link_type == 7) ? 1 : 0;
  int use_lambda_lg = (link_type == 9) ? 1 : 0;
}

parameters {
  // Regression coefficients (may be empty if P = 0)
  vector[P] beta;

  // Cutpoints c_1, ..., c_{K-1} (standard ordered type)
  ordered[K-1] c;

  // Conditional link parameters (only declared if being estimated)
  // Stan doesn't support optional parameters, so we use the array[N] trick:
  // when estimate_X = 0, array[0] creates an empty array (no parameters).
  // When estimate_X = 1, array[1] creates a single-element array.

  // df for tlink, sp(t-base)
  array[estimate_df] real<lower=0> df_raw;

  // lambda for aranda_ordaz (constrained > 0)
  array[estimate_lambda * use_lambda_ao] real<lower=0> lambda_ao_raw;

  // lambda for log_gamma (unconstrained)
  array[estimate_lambda * use_lambda_lg] real lambda_lg_raw;

  // xi for gev (unconstrained)
  array[estimate_xi] real xi_raw;

  // r for sp (constrained > 0)
  array[estimate_r] real<lower=0> r_raw;

  // theta1, theta2 for aep (constrained > 0)
  array[estimate_theta1] real<lower=0> theta1_raw;
  array[estimate_theta2] real<lower=0> theta2_raw;
}

transformed parameters {
  // Linear predictor (no intercept during sampling)
  vector[N] eta;
  if (P > 0) {
    eta = X * beta;
  } else {
    eta = rep_vector(0, N);
  }

  // Unified link parameters
  // Each parameter is either from the raw array (if estimated) or from fixed value
  real df = estimate_df ? df_raw[1] : df_fixed;

  real lambda;
  if (link_type == 7) {
    // aranda_ordaz
    lambda = estimate_lambda ? lambda_ao_raw[1] : lambda_fixed;
  } else if (link_type == 9) {
    // log_gamma
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
      // Normal prior
      beta ~ normal(prior_beta_mu, prior_beta_sd);
    } else if (prior_beta_type == 2) {
      // Student-t prior (heavy tails)
      beta ~ student_t(prior_beta_df, prior_beta_mu, prior_beta_sd);
    } else if (prior_beta_type == 3) {
      // Cauchy prior (very heavy tails, equivalent to student_t with df=1)
      beta ~ cauchy(prior_beta_mu, prior_beta_sd);
    }
    // prior_beta_type == 4: flat (improper uniform), no prior statement needed
  }

  // Priors for cutpoints (thresholds)
  if (prior_c_type == 1) {
    c ~ normal(prior_c_mu, prior_c_sd);
  } else if (prior_c_type == 2) {
    c ~ student_t(prior_c_df, prior_c_mu, prior_c_sd);
  } else if (prior_c_type == 3) {
    c ~ cauchy(prior_c_mu, prior_c_sd);
  }
  // prior_c_type == 4: flat (improper uniform), no prior statement needed

  // Priors for link parameters (only if being estimated)
  if (estimate_df) {
    df_raw[1] ~ gamma(prior_df_alpha, prior_df_beta);
  }

  if (estimate_lambda && link_type == 7) {
    // aranda_ordaz: gamma prior
    lambda_ao_raw[1] ~ gamma(prior_lambda_ao_alpha, prior_lambda_ao_beta);
  }

  if (estimate_lambda && link_type == 9) {
    // log_gamma: normal prior
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
  // Post-transformation: c'_k = c_k - c_1 (so c'_1 = 0), beta0 = -c_1
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
  // c_transformed[1] = 0 by construction

  for (n in 1:N) {
    log_lik[n] = clm_lpmf(y[n] | K, c, eta[n], link_type,
                          df, lambda, xi, r, base_type, theta1, theta2);
    y_rep[n] = clm_rng(K, c, eta[n], link_type,
                       df, lambda, xi, r, base_type, theta1, theta2);
  }
}
