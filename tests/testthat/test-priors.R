# =============================================================================
# Tests for prior specification functions
# =============================================================================

# =============================================================================
# Legacy API: clm_prior()
# =============================================================================

test_that("clm_prior creates object with correct class", {
  prior <- clm_prior()
  expect_s3_class(prior, "clm_prior")
})

test_that("clm_prior with no arguments returns empty object", {
  prior <- clm_prior()
  expect_equal(length(prior), 0)
})

test_that("clm_prior stores specified values", {
  prior <- clm_prior(beta_sd = 1, c_sd = 5)
  expect_equal(prior$beta_sd, 1)
  expect_equal(prior$c_sd, 5)
})

test_that("clm_prior rejects invalid values", {
  # SD must be positive

expect_error(clm_prior(beta_sd = 0), "must be positive")
  expect_error(clm_prior(beta_sd = -1), "must be positive")
  expect_error(clm_prior(c_sd = 0), "must be positive")

  # Non-numeric values
  expect_error(clm_prior(beta_sd = "1"), "must be a single numeric")
  expect_error(clm_prior(beta_sd = c(1, 2)), "must be a single numeric")
})

test_that("clm_prior accepts all parameter types", {
  # Basic priors
  prior1 <- clm_prior(beta_sd = 2)
  expect_equal(prior1$beta_sd, 2)

  # Flexible threshold
  prior2 <- clm_prior(c_sd = 5)
  expect_equal(prior2$c_sd, 5)

  # Equidistant threshold
  prior3 <- clm_prior(c1_mu = -1, c1_sd = 5, d_alpha = 1, d_beta = 1)
  expect_equal(prior3$c1_mu, -1)
  expect_equal(prior3$d_alpha, 1)

  # Symmetric threshold
  prior4 <- clm_prior(cpos_sd = 3)
  expect_equal(prior4$cpos_sd, 3)

  # Link parameters
  prior5 <- clm_prior(df_alpha = 3, df_beta = 0.2)
  expect_equal(prior5$df_alpha, 3)
  expect_equal(prior5$df_beta, 0.2)

  prior6 <- clm_prior(xi_mu = 0.5, xi_sd = 0.3)
  expect_equal(prior6$xi_mu, 0.5)
  expect_equal(prior6$xi_sd, 0.3)
})

test_that("print.clm_prior works for empty prior", {
  prior <- clm_prior()
  expect_output(print(prior), "using all defaults")
})

test_that("print.clm_prior shows specified values", {
  prior <- clm_prior(beta_sd = 1, xi_mu = 0.5)
  expect_output(print(prior), "beta_sd = 1")
  expect_output(print(prior), "xi_mu = 0.5")
})

test_that("get_default_priors returns correct defaults for flexible threshold", {
  defaults <- get_default_priors("flexible")
  expect_equal(defaults$beta_sd, 2.5)
  expect_equal(defaults$c_sd, 10)
})

test_that("get_default_priors returns correct defaults for equidistant threshold", {
  defaults <- get_default_priors("equidistant")
  expect_equal(defaults$beta_sd, 2.5)
  expect_equal(defaults$c1_mu, 0)
  expect_equal(defaults$c1_sd, 10)
  expect_equal(defaults$d_alpha, 2)
  expect_equal(defaults$d_beta, 0.5)
})

test_that("get_default_priors returns correct defaults for symmetric threshold", {
  defaults <- get_default_priors("symmetric")
  expect_equal(defaults$beta_sd, 2.5)
  expect_equal(defaults$cpos_sd, 5)
})

test_that("get_default_priors includes link parameter defaults", {
  defaults <- get_default_priors("flexible")
  expect_equal(defaults$df_alpha, 2)
  expect_equal(defaults$df_beta, 0.1)
  expect_equal(defaults$xi_mu, 0)
  expect_equal(defaults$xi_sd, 2)
  expect_equal(defaults$theta1_alpha, 2)
  expect_equal(defaults$theta1_beta, 1)
})

test_that("apply_priors_to_stan_data uses defaults when prior is NULL", {
  # Mock stan_data
  stan_data <- list(
    prior_beta_sd = 999,
    prior_c_sd = 999
  )

  result <- apply_priors_to_stan_data(stan_data, NULL, "flexible", FALSE)

  expect_equal(result$prior_beta_sd, 2.5)  # default
  expect_equal(result$prior_c_sd, 10)      # default
})

test_that("apply_priors_to_stan_data applies user priors", {
  stan_data <- list(
    prior_beta_sd = 999,
    prior_c_sd = 999
  )
  prior <- clm_prior(beta_sd = 1, c_sd = 5)

  result <- apply_priors_to_stan_data(stan_data, prior, "flexible", FALSE)

  expect_equal(result$prior_beta_sd, 1)
  expect_equal(result$prior_c_sd, 5)
})

test_that("apply_priors_to_stan_data handles equidistant threshold", {
  stan_data <- list(
    prior_beta_sd = 999,
    prior_c1_mu = 999,
    prior_c1_sd = 999,
    prior_d_alpha = 999,
    prior_d_beta = 999
  )
  prior <- clm_prior(d_alpha = 1, d_beta = 2)

  result <- apply_priors_to_stan_data(stan_data, prior, "equidistant", FALSE)

  expect_equal(result$prior_beta_sd, 2.5)   # default
  expect_equal(result$prior_c1_mu, 0)       # default
  expect_equal(result$prior_d_alpha, 1)     # user
  expect_equal(result$prior_d_beta, 2)      # user
})

test_that("apply_priors_to_stan_data handles link parameters in full model", {
  stan_data <- list(
    prior_beta_sd = 999,
    prior_c_sd = 999,
    prior_xi_mu = 999,
    prior_xi_sd = 999
  )
  prior <- clm_prior(xi_mu = 0.5, xi_sd = 0.3)

  result <- apply_priors_to_stan_data(stan_data, prior, "flexible", full = TRUE)

  expect_equal(result$prior_xi_mu, 0.5)
  expect_equal(result$prior_xi_sd, 0.3)
})

test_that("validate_prior warns for incompatible threshold priors", {
  # Equidistant priors with flexible threshold
  prior1 <- clm_prior(d_alpha = 1)
  expect_warning(validate_prior(prior1, "flexible", "logit", NULL), "equidistant")

  # Symmetric priors with flexible threshold
  prior2 <- clm_prior(cpos_sd = 3)
  expect_warning(validate_prior(prior2, "flexible", "logit", NULL), "symmetric")

  # Flexible priors with equidistant threshold
  prior3 <- clm_prior(c_sd = 5)
  expect_warning(validate_prior(prior3, "equidistant", "logit", NULL), "flexible")
})

test_that("validate_prior warns for link priors without estimation", {
  # xi prior set but not estimating xi
  prior <- clm_prior(xi_mu = 0.5)
  expect_warning(
    validate_prior(prior, "flexible", "gev", list(xi = 0)),
    "not being estimated"
  )

  # No warning when estimating
  expect_silent(
    validate_prior(prior, "flexible", "gev", list(xi = "estimate"))
  )
})

test_that("validate_prior passes for valid combinations", {
  # Empty prior
  expect_silent(validate_prior(NULL, "flexible", "logit", NULL))
  expect_silent(validate_prior(clm_prior(), "flexible", "logit", NULL))

  # Matching threshold type
  expect_silent(validate_prior(clm_prior(c_sd = 5), "flexible", "logit", NULL))
  expect_silent(validate_prior(clm_prior(d_alpha = 1), "equidistant", "logit", NULL))
  expect_silent(validate_prior(clm_prior(cpos_sd = 3), "symmetric", "logit", NULL))
})

# =============================================================================
# Distribution-based Prior API tests
# =============================================================================

# Distribution helper tests
test_that("normal() creates correct distribution object", {
  dist <- normal(0, 2.5)
  expect_s3_class(dist, "clm_dist")
  expect_equal(dist$dist, "normal")
  expect_equal(dist$mu, 0)
  expect_equal(dist$sigma, 2.5)
})

test_that("normal() uses correct defaults", {
  dist <- normal()
  expect_equal(dist$mu, 0)
  expect_equal(dist$sigma, 1)
})

test_that("normal() validates inputs", {
  expect_error(normal(sigma = -1), "positive")
  expect_error(normal(sigma = 0), "positive")
  expect_error(normal(mu = "a"), "numeric")
  expect_error(normal(sigma = c(1, 2)), "single")
})

test_that("gamma() creates correct distribution object", {
  dist <- gamma(2, 0.1)
  expect_s3_class(dist, "clm_dist")
  expect_equal(dist$dist, "gamma")
  expect_equal(dist$alpha, 2)
  expect_equal(dist$beta, 0.1)
})

test_that("gamma() validates inputs", {
  expect_error(gamma(-1, 1), "positive")
  expect_error(gamma(1, -1), "positive")
  expect_error(gamma(0, 1), "positive")
  expect_error(gamma(1, 0), "positive")
})

test_that("gamma() requires both arguments (no defaults)", {
  expect_error(gamma(), "missing")
  expect_error(gamma(2), "missing")
})

test_that("gamma() rejects non-numeric inputs", {
  expect_error(gamma("a", 1), "numeric")
  expect_error(gamma(1, "b"), "numeric")
  expect_error(gamma(c(1, 2), 1), "single")
  expect_error(gamma(1, c(1, 2)), "single")
})

test_that("student_t() creates correct distribution object", {
  dist <- student_t(3, 0, 2.5)
  expect_s3_class(dist, "clm_dist")
  expect_equal(dist$dist, "student_t")
  expect_equal(dist$df, 3)
  expect_equal(dist$mu, 0)
  expect_equal(dist$sigma, 2.5)
})

test_that("student_t() uses correct defaults", {
  dist <- student_t()
  expect_equal(dist$df, 3)
  expect_equal(dist$mu, 0)
  expect_equal(dist$sigma, 1)
})

test_that("student_t() validates inputs", {
  expect_error(student_t(df = -1), "positive")
  expect_error(student_t(df = 0), "positive")
  expect_error(student_t(sigma = -1), "positive")
  expect_error(student_t(sigma = 0), "positive")
  expect_error(student_t(df = "a"), "numeric")
  expect_error(student_t(mu = "a"), "numeric")
  expect_error(student_t(df = c(1, 2)), "single")
})

test_that("cauchy() creates correct distribution object", {
  dist <- cauchy(0, 2.5)
  expect_s3_class(dist, "clm_dist")
  expect_equal(dist$dist, "cauchy")
  expect_equal(dist$mu, 0)
  expect_equal(dist$sigma, 2.5)
})

test_that("cauchy() uses correct defaults", {
  dist <- cauchy()
  expect_equal(dist$mu, 0)
  expect_equal(dist$sigma, 1)
})

test_that("cauchy() validates inputs", {
  expect_error(cauchy(sigma = -1), "positive")
  expect_error(cauchy(sigma = 0), "positive")
  expect_error(cauchy(mu = "a"), "numeric")
  expect_error(cauchy(sigma = c(1, 2)), "single")
})

# prior() function tests
test_that("prior() creates clm_prior_spec object", {
  p <- prior(normal(0, 2.5), class = "b")
  expect_s3_class(p, "clm_prior_spec")
  expect_equal(p$class, "b")
  expect_s3_class(p$prior, "clm_dist")
})

test_that("prior() validates distribution object", {
  expect_error(prior("not a dist"), "must be a distribution object")
  expect_error(prior(list(x = 1)), "must be a distribution object")
})

test_that("prior() validates class argument", {
  expect_error(prior(normal(0, 1), class = "invalid"), "Invalid class")
})

test_that("prior() validates class-distribution compatibility", {
  # Gamma for b class should fail
  expect_error(prior(gamma(2, 1), class = "b"), "requires normal")

  # Normal for df class should fail
  expect_error(prior(normal(0, 1), class = "df"), "requires gamma")
})

test_that("prior() rejects gamma for normal-family classes", {
  # All normal-family classes should reject gamma distribution
  expect_error(prior(gamma(2, 1), class = "b"), "requires normal")
  expect_error(prior(gamma(2, 1), class = "Intercept"), "requires normal")
  expect_error(prior(gamma(2, 1), class = "c1"), "requires normal")
  expect_error(prior(gamma(2, 1), class = "cpos"), "requires normal")
  expect_error(prior(gamma(2, 1), class = "lambda_lg"), "requires normal")
  expect_error(prior(gamma(2, 1), class = "xi"), "requires normal")
})

test_that("prior() rejects normal for gamma-family classes", {
  # All gamma-family classes should reject normal distribution
  expect_error(prior(normal(0, 1), class = "d"), "requires gamma")
  expect_error(prior(normal(0, 1), class = "df"), "requires gamma")
  expect_error(prior(normal(0, 1), class = "lambda_ao"), "requires gamma")
  expect_error(prior(normal(0, 1), class = "r"), "requires gamma")
  expect_error(prior(normal(0, 1), class = "theta1"), "requires gamma")
  expect_error(prior(normal(0, 1), class = "theta2"), "requires gamma")
})

test_that("prior() rejects student_t for gamma-family classes", {
  # All gamma-family classes should reject student_t distribution
  expect_error(prior(student_t(3, 0, 1), class = "d"), "requires gamma")
  expect_error(prior(student_t(3, 0, 1), class = "df"), "requires gamma")
  expect_error(prior(student_t(3, 0, 1), class = "lambda_ao"), "requires gamma")
  expect_error(prior(student_t(3, 0, 1), class = "r"), "requires gamma")
  expect_error(prior(student_t(3, 0, 1), class = "theta1"), "requires gamma")
  expect_error(prior(student_t(3, 0, 1), class = "theta2"), "requires gamma")
})

test_that("prior() rejects cauchy for gamma-family classes", {
  # All gamma-family classes should reject cauchy distribution
  expect_error(prior(cauchy(0, 1), class = "d"), "requires gamma")
  expect_error(prior(cauchy(0, 1), class = "df"), "requires gamma")
  expect_error(prior(cauchy(0, 1), class = "lambda_ao"), "requires gamma")
  expect_error(prior(cauchy(0, 1), class = "r"), "requires gamma")
  expect_error(prior(cauchy(0, 1), class = "theta1"), "requires gamma")
  expect_error(prior(cauchy(0, 1), class = "theta2"), "requires gamma")
})

test_that("prior() accepts valid combinations", {

  # Normal-family classes (accept normal, student_t, cauchy)
  expect_s3_class(prior(normal(0, 2.5), class = "b"), "clm_prior_spec")
  expect_s3_class(prior(student_t(3, 0, 2.5), class = "b"), "clm_prior_spec")
  expect_s3_class(prior(cauchy(0, 2.5), class = "b"), "clm_prior_spec")
  expect_s3_class(prior(normal(0, 10), class = "Intercept"), "clm_prior_spec")
  expect_s3_class(prior(normal(-1, 5), class = "c1"), "clm_prior_spec")
  expect_s3_class(prior(normal(0, 3), class = "cpos"), "clm_prior_spec")
  expect_s3_class(prior(normal(0, 1), class = "lambda_lg"), "clm_prior_spec")
  expect_s3_class(prior(normal(0, 0.5), class = "xi"), "clm_prior_spec")

  # Gamma classes (only accept gamma)
  expect_s3_class(prior(gamma(2, 0.5), class = "d"), "clm_prior_spec")
  expect_s3_class(prior(gamma(2, 0.1), class = "df"), "clm_prior_spec")
  expect_s3_class(prior(gamma(0.5, 0.5), class = "lambda_ao"), "clm_prior_spec")
  expect_s3_class(prior(gamma(0.5, 0.5), class = "r"), "clm_prior_spec")
  expect_s3_class(prior(gamma(2, 1), class = "theta1"), "clm_prior_spec")
  expect_s3_class(prior(gamma(2, 1), class = "theta2"), "clm_prior_spec")
})

# c.clm_prior_spec tests
test_that("c() combines multiple priors", {
  priors <- c(
    prior(normal(0, 2.5), class = "b"),
    prior(normal(0, 10), class = "Intercept")
  )
  expect_s3_class(priors, "clm_prior_list")
  expect_length(priors, 2)
})

test_that("c() validates all arguments are clm_prior_spec", {
  expect_error(
    c(prior(normal(0, 1), class = "b"), "not a prior"),
    "must be clm_prior_spec"
  )
})

# Print methods
test_that("print.clm_dist works correctly", {
  expect_output(print(normal(0, 2.5)), "normal\\(0, 2.5\\)")
  expect_output(print(gamma(2, 0.1)), "gamma\\(2, 0.1\\)")
  expect_output(print(student_t(3, 0, 2.5)), "student_t\\(3, 0, 2.5\\)")
  expect_output(print(cauchy(0, 2.5)), "cauchy\\(0, 2.5\\)")
})

test_that("print.clm_prior_spec works correctly", {
  p <- prior(normal(0, 2.5), class = "b")
  expect_output(print(p), "Prior:.*normal")
  expect_output(print(p), "Class:.*b")
})

test_that("print.clm_prior_spec shows coef when set", {
  p <- prior(normal(0, 2.5), class = "b", coef = "temp")
  expect_output(print(p), "Prior:.*normal")
  expect_output(print(p), "Class:.*b")
  expect_output(print(p), "Coef:.*temp")
})

test_that("print.clm_prior_list works correctly", {
  priors <- c(
    prior(normal(0, 2.5), class = "b"),
    prior(gamma(2, 0.1), class = "df")
  )
  expect_output(print(priors), "Prior specifications")
  expect_output(print(priors), "b:.*normal")
  expect_output(print(priors), "df:.*gamma")
})

# Conversion tests
test_that("convert_prior_spec_to_legacy works for single prior", {
  p <- prior(normal(0, 2.5), class = "b")
  legacy <- convert_prior_spec_to_legacy(p)
  expect_s3_class(legacy, "clm_prior")
  expect_equal(legacy$beta_sd, 2.5)
})

test_that("convert_prior_spec_to_legacy works for prior list", {
  priors <- c(
    prior(normal(0, 2.5), class = "b"),
    prior(normal(0, 10), class = "Intercept"),
    prior(gamma(2, 0.1), class = "df")
  )
  legacy <- convert_prior_spec_to_legacy(priors)
  expect_s3_class(legacy, "clm_prior")
  expect_equal(legacy$beta_sd, 2.5)
  expect_equal(legacy$c_sd, 10)
  expect_equal(legacy$df_alpha, 2)
  expect_equal(legacy$df_beta, 0.1)
})

test_that("convert_prior_spec_to_legacy passes through clm_prior", {
  original <- clm_prior(beta_sd = 1)
  result <- convert_prior_spec_to_legacy(original)
  expect_identical(result, original)
})

test_that("convert_prior_spec_to_legacy handles all classes", {
  # Test each class
  expect_equal(convert_prior_spec_to_legacy(prior(normal(0, 1), class = "b"))$beta_sd, 1)
  expect_equal(convert_prior_spec_to_legacy(prior(normal(0, 5), class = "Intercept"))$c_sd, 5)
  expect_equal(convert_prior_spec_to_legacy(prior(normal(-1, 2), class = "c1"))$c1_mu, -1)
  expect_equal(convert_prior_spec_to_legacy(prior(normal(-1, 2), class = "c1"))$c1_sd, 2)
  expect_equal(convert_prior_spec_to_legacy(prior(gamma(1, 2), class = "d"))$d_alpha, 1)
  expect_equal(convert_prior_spec_to_legacy(prior(normal(0, 3), class = "cpos"))$cpos_sd, 3)
  expect_equal(convert_prior_spec_to_legacy(prior(gamma(2, 0.1), class = "df"))$df_alpha, 2)
  expect_equal(convert_prior_spec_to_legacy(prior(gamma(0.5, 0.5), class = "lambda_ao"))$lambda_ao_alpha, 0.5)
  expect_equal(convert_prior_spec_to_legacy(prior(normal(0, 1), class = "lambda_lg"))$lambda_lg_mu, 0)
  expect_equal(convert_prior_spec_to_legacy(prior(normal(0, 0.5), class = "xi"))$xi_sd, 0.5)
  expect_equal(convert_prior_spec_to_legacy(prior(gamma(0.5, 0.5), class = "r"))$r_alpha, 0.5)
  expect_equal(convert_prior_spec_to_legacy(prior(gamma(2, 1), class = "theta1"))$theta1_alpha, 2)
  expect_equal(convert_prior_spec_to_legacy(prior(gamma(3, 1), class = "theta2"))$theta2_alpha, 3)
})

test_that("convert_prior_spec_to_legacy handles student_t for normal-family classes", {
  # student_t should use sigma as SD (df is ignored in current Stan model)
  expect_equal(convert_prior_spec_to_legacy(prior(student_t(3, 0, 2.5), class = "b"))$beta_sd, 2.5)
  expect_equal(convert_prior_spec_to_legacy(prior(student_t(5, 0, 10), class = "Intercept"))$c_sd, 10)
})

test_that("convert_prior_spec_to_legacy handles cauchy for normal-family classes", {
  # cauchy should use sigma as SD
  expect_equal(convert_prior_spec_to_legacy(prior(cauchy(0, 2.5), class = "b"))$beta_sd, 2.5)
  expect_equal(convert_prior_spec_to_legacy(prior(cauchy(0, 10), class = "Intercept"))$c_sd, 10)
})

test_that("convert_prior_spec_to_legacy errors for unknown format", {
  expect_error(convert_prior_spec_to_legacy(list(x = 1)), "Unknown prior format")
  expect_error(convert_prior_spec_to_legacy("invalid"), "Unknown prior format")
})

# Backward compatibility
test_that("clm_prior() still works (backward compatibility)", {
  prior <- clm_prior(beta_sd = 2.5, c_sd = 10)
  expect_s3_class(prior, "clm_prior")
  expect_equal(prior$beta_sd, 2.5)
  expect_equal(prior$c_sd, 10)
})

# =============================================================================
# flat() distribution and extended beta prior parameters
# =============================================================================

test_that("flat() creates valid distribution object", {
  d <- flat()
  expect_s3_class(d, "clm_dist")
  expect_equal(d$dist, "flat")
})

test_that("flat() works with prior() for class 'b'", {
  p <- prior(flat(), class = "b")
  expect_s3_class(p, "clm_prior_spec")
  expect_equal(p$class, "b")
  expect_equal(p$prior$dist, "flat")
})

test_that("flat() works with threshold classes", {
  # flat() should work for threshold classes (Intercept, c1, cpos)
  expect_s3_class(prior(flat(), class = "Intercept"), "clm_prior_spec")
  expect_s3_class(prior(flat(), class = "c1"), "clm_prior_spec")
  expect_s3_class(prior(flat(), class = "cpos"), "clm_prior_spec")
})

test_that("flat() is rejected for link parameter classes", {
  # flat() should NOT work for link parameter classes
  expect_error(prior(flat(), class = "lambda_lg"), "requires normal")
  expect_error(prior(flat(), class = "xi"), "requires normal")
})

test_that("format_dist handles flat", {
  d <- flat()
  expect_equal(format_dist(d), "flat()")
})

test_that("map_class_to_params extracts all parameters for normal", {
  result <- map_class_to_params("b", normal(1, 2.5))
  expect_equal(result$beta_type, PRIOR_TYPES$normal)
  expect_equal(result$beta_mu, 1)
  expect_equal(result$beta_sd, 2.5)
  expect_true("beta_df" %in% names(result))
})

test_that("map_class_to_params extracts all parameters for student_t", {
  result <- map_class_to_params("b", student_t(5, 1, 2.5))
  expect_equal(result$beta_type, PRIOR_TYPES$student_t)
  expect_equal(result$beta_mu, 1)
  expect_equal(result$beta_sd, 2.5)
  expect_equal(result$beta_df, 5)
})

test_that("map_class_to_params extracts all parameters for cauchy", {
  result <- map_class_to_params("b", cauchy(1, 2.5))
  expect_equal(result$beta_type, PRIOR_TYPES$cauchy)
  expect_equal(result$beta_mu, 1)
  expect_equal(result$beta_sd, 2.5)
})

test_that("map_class_to_params handles flat correctly", {
  result <- map_class_to_params("b", flat())
  expect_equal(result$beta_type, PRIOR_TYPES$flat)
  expect_true(all(c("beta_mu", "beta_sd", "beta_df") %in% names(result)))
})

test_that("convert_prior_spec_to_legacy preserves mu for normal", {
  legacy <- convert_prior_spec_to_legacy(prior(normal(1, 2.5), class = "b"))
  expect_equal(legacy$beta_mu, 1)
  expect_equal(legacy$beta_sd, 2.5)
  expect_equal(legacy$beta_type, PRIOR_TYPES$normal)
})

test_that("convert_prior_spec_to_legacy preserves df for student_t", {
  legacy <- convert_prior_spec_to_legacy(prior(student_t(5, 1, 2.5), class = "b"))
  expect_equal(legacy$beta_df, 5)
  expect_equal(legacy$beta_mu, 1)
  expect_equal(legacy$beta_sd, 2.5)
  expect_equal(legacy$beta_type, PRIOR_TYPES$student_t)
})

test_that("convert_prior_spec_to_legacy handles flat", {
  legacy <- convert_prior_spec_to_legacy(prior(flat(), class = "b"))
  expect_equal(legacy$beta_type, PRIOR_TYPES$flat)
})

test_that("get_default_priors includes beta_type, beta_mu, beta_df", {
  defaults <- get_default_priors("flexible")
  expect_equal(defaults$beta_type, PRIOR_TYPES$normal)
  expect_equal(defaults$beta_mu, 0)
  expect_equal(defaults$beta_sd, 2.5)
  expect_true("beta_df" %in% names(defaults))
})

# =============================================================================
# Phase 5.3.5: Threshold prior type/df parameters
# =============================================================================

test_that("get_default_priors includes threshold type/df for flexible", {
  defaults <- get_default_priors("flexible")
  expect_equal(defaults$c_type, PRIOR_TYPES$normal)
  expect_equal(defaults$c_mu, 0)
  expect_equal(defaults$c_sd, 10)
  expect_true("c_df" %in% names(defaults))
})

test_that("get_default_priors includes threshold type/df for equidistant", {
  defaults <- get_default_priors("equidistant")
  expect_equal(defaults$c1_type, PRIOR_TYPES$normal)
  expect_equal(defaults$c1_mu, 0)
  expect_equal(defaults$c1_sd, 10)
  expect_true("c1_df" %in% names(defaults))
})

test_that("get_default_priors includes threshold type/df for symmetric", {
  defaults <- get_default_priors("symmetric")
  expect_equal(defaults$cpos_type, PRIOR_TYPES$normal)
  expect_equal(defaults$cpos_sd, 5)
  expect_true("cpos_df" %in% names(defaults))
})

test_that("map_class_to_params extracts all parameters for Intercept (normal)", {
  result <- map_class_to_params("Intercept", normal(1, 5))
  expect_equal(result$c_type, PRIOR_TYPES$normal)
  expect_equal(result$c_mu, 1)
  expect_equal(result$c_sd, 5)
  expect_true("c_df" %in% names(result))
})

test_that("map_class_to_params extracts all parameters for Intercept (student_t)", {
  result <- map_class_to_params("Intercept", student_t(5, 1, 5))
  expect_equal(result$c_type, PRIOR_TYPES$student_t)
  expect_equal(result$c_mu, 1)
  expect_equal(result$c_sd, 5)
  expect_equal(result$c_df, 5)
})

test_that("map_class_to_params extracts all parameters for Intercept (cauchy)", {
  result <- map_class_to_params("Intercept", cauchy(1, 5))
  expect_equal(result$c_type, PRIOR_TYPES$cauchy)
  expect_equal(result$c_mu, 1)
  expect_equal(result$c_sd, 5)
})

test_that("map_class_to_params extracts all parameters for Intercept (flat)", {
  result <- map_class_to_params("Intercept", flat())
  expect_equal(result$c_type, PRIOR_TYPES$flat)
  expect_true(all(c("c_mu", "c_sd", "c_df") %in% names(result)))
})

test_that("map_class_to_params extracts all parameters for c1 (normal)", {
  result <- map_class_to_params("c1", normal(-1, 5))
  expect_equal(result$c1_type, PRIOR_TYPES$normal)
  expect_equal(result$c1_mu, -1)
  expect_equal(result$c1_sd, 5)
  expect_true("c1_df" %in% names(result))
})

test_that("map_class_to_params extracts all parameters for c1 (student_t)", {
  result <- map_class_to_params("c1", student_t(7, -1, 5))
  expect_equal(result$c1_type, PRIOR_TYPES$student_t)
  expect_equal(result$c1_mu, -1)
  expect_equal(result$c1_sd, 5)
  expect_equal(result$c1_df, 7)
})

test_that("map_class_to_params extracts all parameters for c1 (flat)", {
  result <- map_class_to_params("c1", flat())
  expect_equal(result$c1_type, PRIOR_TYPES$flat)
  expect_true(all(c("c1_mu", "c1_sd", "c1_df") %in% names(result)))
})

test_that("map_class_to_params extracts all parameters for cpos (normal)", {
  result <- map_class_to_params("cpos", normal(0, 3))
  expect_equal(result$cpos_type, PRIOR_TYPES$normal)
  expect_equal(result$cpos_sd, 3)
  expect_true("cpos_df" %in% names(result))
})

test_that("map_class_to_params extracts all parameters for cpos (student_t)", {
  result <- map_class_to_params("cpos", student_t(5, 0, 3))
  expect_equal(result$cpos_type, PRIOR_TYPES$student_t)
  expect_equal(result$cpos_sd, 3)
  expect_equal(result$cpos_df, 5)
})

test_that("map_class_to_params extracts all parameters for cpos (flat)", {
  result <- map_class_to_params("cpos", flat())
  expect_equal(result$cpos_type, PRIOR_TYPES$flat)
  expect_true(all(c("cpos_sd", "cpos_df") %in% names(result)))
})

test_that("convert_prior_spec_to_legacy handles Intercept with student_t", {
  legacy <- convert_prior_spec_to_legacy(prior(student_t(5, 1, 10), class = "Intercept"))
  expect_equal(legacy$c_type, PRIOR_TYPES$student_t)
  expect_equal(legacy$c_mu, 1)
  expect_equal(legacy$c_sd, 10)
  expect_equal(legacy$c_df, 5)
})

test_that("convert_prior_spec_to_legacy handles c1 with flat", {
  legacy <- convert_prior_spec_to_legacy(prior(flat(), class = "c1"))
  expect_equal(legacy$c1_type, PRIOR_TYPES$flat)
})

test_that("convert_prior_spec_to_legacy handles cpos with cauchy", {
  legacy <- convert_prior_spec_to_legacy(prior(cauchy(0, 3), class = "cpos"))
  expect_equal(legacy$cpos_type, PRIOR_TYPES$cauchy)
  expect_equal(legacy$cpos_sd, 3)
})

test_that("apply_priors_to_stan_data handles flexible threshold type parameters", {
  stan_data <- list(
    prior_c_type = 999,
    prior_c_mu = 999,
    prior_c_sd = 999,
    prior_c_df = 999
  )
  prior <- structure(list(
    c_type = PRIOR_TYPES$student_t,
    c_mu = 1,
    c_sd = 5,
    c_df = 7
  ), class = "clm_prior")

  result <- apply_priors_to_stan_data(stan_data, prior, "flexible", FALSE)

  expect_equal(result$prior_c_type, PRIOR_TYPES$student_t)
  expect_equal(result$prior_c_mu, 1)
  expect_equal(result$prior_c_sd, 5)
  expect_equal(result$prior_c_df, 7)
})

test_that("apply_priors_to_stan_data handles equidistant threshold type parameters", {
  stan_data <- list(
    prior_c1_type = 999,
    prior_c1_mu = 999,
    prior_c1_sd = 999,
    prior_c1_df = 999
  )
  prior <- structure(list(
    c1_type = PRIOR_TYPES$cauchy,
    c1_mu = -1,
    c1_sd = 5,
    c1_df = 7
  ), class = "clm_prior")

  result <- apply_priors_to_stan_data(stan_data, prior, "equidistant", FALSE)

  expect_equal(result$prior_c1_type, PRIOR_TYPES$cauchy)
  expect_equal(result$prior_c1_mu, -1)
  expect_equal(result$prior_c1_sd, 5)
})

test_that("apply_priors_to_stan_data handles symmetric threshold type parameters", {
  stan_data <- list(
    prior_cpos_type = 999,
    prior_cpos_sd = 999,
    prior_cpos_df = 999
  )
  prior <- structure(list(
    cpos_type = PRIOR_TYPES$flat,
    cpos_sd = 3,
    cpos_df = 7
  ), class = "clm_prior")

  result <- apply_priors_to_stan_data(stan_data, prior, "symmetric", FALSE)

  expect_equal(result$prior_cpos_type, PRIOR_TYPES$flat)
})
