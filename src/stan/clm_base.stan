/**
 * Cumulative Link Model (CLM) - Base Model
 *
 * Supports 11 link functions with fixed parameters for flexible links.
 * For Bayesian inference of link parameters, use clm_full.stan.
 *
 * RELATED FILES (update together when modifying data/parameters/model blocks):
 *   - clm_equidistant.stan      (equidistant thresholds)
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
 *   7: aranda_ordaz (Aranda-Ordaz asymmetric, requires lambda)
 *   8: sp (Symmetric Power, requires r, base_type, df)
 *   9: log_gamma (Log-gamma, requires lambda)
 *   10: gev (Generalized Extreme Value, requires xi)
 *   11: aep (Asymmetric Exponential Power, requires theta1, theta2)
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

  // Prior hyperparameters for cutpoints (thresholds)
  // Type: 1=normal, 2=student_t, 3=cauchy, 4=flat (improper uniform)
  int<lower=1, upper=4> prior_c_type;
  real prior_c_mu;                         // location (unused for flat)
  real<lower=0> prior_c_sd;                // scale (unused for flat)
  real<lower=0> prior_c_df;                // df for student_t (unused for others)
}

transformed data {
  // Check K >= 2 is ensured by constraint
  // For K = 2, we have only c[1] = 0 and no c_raw parameters
}

parameters {
  // Regression coefficients (may be empty if P = 0)
  vector[P] beta;

  // Cutpoints c_1, ..., c_{K-1}
  // Stan's ordered type ensures c[1] < c[2] < ... < c[K-1] via internal transform
  ordered[K-1] c;
}

transformed parameters {
  // Linear predictor (no intercept during sampling)
  vector[N] eta;
  // P = 0 means intercept-only model; eta is all zeros (thresholds absorb it)
  if (P > 0) {
    eta = X * beta;
  } else {
    eta = rep_vector(0, N);
  }
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
