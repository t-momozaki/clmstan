# =============================================================================
# Tests for R implementations of link function CDFs
# =============================================================================

test_that("standard link CDFs match R built-in implementations", {
  x <- seq(-3, 3, by = 0.5)

  # logit: should match plogis

  expect_equal(clm_cdf(x, "logit", list()), plogis(x))

  # probit: should match pnorm
  expect_equal(clm_cdf(x, "probit", list()), pnorm(x))

  # cauchit: should match pcauchy
  expect_equal(clm_cdf(x, "cauchit", list()), pcauchy(x))
})

test_that("cloglog and loglog CDFs are correct", {
  x <- seq(-3, 3, by = 0.5)

  # cloglog: F(x) = 1 - exp(-exp(x))
  expected_cloglog <- 1 - exp(-exp(x))
  expect_equal(clm_cdf(x, "cloglog", list()), expected_cloglog)

  # loglog: F(x) = exp(-exp(-x))
  expected_loglog <- exp(-exp(-x))
  expect_equal(clm_cdf(x, "loglog", list()), expected_loglog)

  # loglog and cloglog are reflections of each other
  expect_equal(clm_cdf(x, "loglog", list()), 1 - clm_cdf(-x, "cloglog", list()))
})

test_that("tlink CDF matches pt()", {
  x <- seq(-3, 3, by = 0.5)

  # tlink with various df values
  for (df in c(1, 3, 8, 30, 100)) {
    expect_equal(
      clm_cdf(x, "tlink", list(df = df)),
      pt(x, df = df),
      info = paste("df =", df)
    )
  }
})

test_that("tlink with large df approaches probit", {
  x <- seq(-3, 3, by = 0.5)

  # As df -> Inf, t-distribution -> normal
  expect_equal(
    clm_cdf(x, "tlink", list(df = 1000)),
    pnorm(x),
    tolerance = 0.01
  )
})

test_that("aranda_ordaz special cases are correct", {
  x <- seq(-3, 3, by = 0.5)

  # lambda = 1: should equal logit
  expect_equal(
    clm_cdf(x, "aranda_ordaz", list(lambda = 1)),
    clm_cdf(x, "logit", list()),
    tolerance = 1e-10
  )

  # Note: as lambda -> 0, aranda_ordaz approaches cloglog asymptotically

  # but convergence is slow. We just verify it's a valid CDF here.
  ao_small <- clm_cdf(x, "aranda_ordaz", list(lambda = 0.1))
  expect_true(all(ao_small >= 0))
  expect_true(all(ao_small <= 1))
})

test_that("aranda_ordaz rejects invalid lambda", {
  expect_error(aranda_ordaz_cdf(0, lambda = 0))
  expect_error(aranda_ordaz_cdf(0, lambda = -1))
})

test_that("sp link with r=1 equals base distribution", {
  x <- seq(-3, 3, by = 0.5)

  # r = 1 should give base distribution
  # Only symmetric distributions are valid SP bases (Li et al., 2019)
  for (base in c("logit", "probit", "cauchit")) {
    expect_equal(
      clm_cdf(x, "sp", list(r = 1, base = base)),
      clm_cdf(x, base, list()),
      tolerance = 1e-10,
      info = paste("base =", base)
    )
  }

  # r = 1 with tlink base
  expect_equal(
    clm_cdf(x, "sp", list(r = 1, base = "tlink", df = 8)),
    clm_cdf(x, "tlink", list(df = 8)),
    tolerance = 1e-10
  )
})

test_that("sp link r < 1 and r > 1 give different results", {
  x <- seq(-3, 3, by = 0.5)

  sp_half <- clm_cdf(x, "sp", list(r = 0.5, base = "logit"))
  sp_one <- clm_cdf(x, "sp", list(r = 1, base = "logit"))
  sp_two <- clm_cdf(x, "sp", list(r = 2, base = "logit"))

  # r = 0.5 and r = 2 should be different from r = 1

  expect_false(all(abs(sp_half - sp_one) < 1e-10))
  expect_false(all(abs(sp_two - sp_one) < 1e-10))
})

test_that("sp link rejects invalid r", {
  expect_error(sp_cdf(0, r = 0, base = "logit"))
  expect_error(sp_cdf(0, r = -1, base = "logit"))
})

test_that("log_gamma special cases are correct", {
  x <- seq(-3, 3, by = 0.5)

  # lambda = 0: should equal probit
  expect_equal(
    clm_cdf(x, "log_gamma", list(lambda = 0)),
    clm_cdf(x, "probit", list()),
    tolerance = 1e-10
  )

  # lambda very close to 0: should be close to probit
  expect_equal(
    clm_cdf(x, "log_gamma", list(lambda = 1e-12)),
    clm_cdf(x, "probit", list()),
    tolerance = 1e-6
  )
})

test_that("log_gamma positive and negative lambda give different results", {
  x <- seq(-3, 3, by = 0.5)

  lg_pos <- clm_cdf(x, "log_gamma", list(lambda = 1))
  lg_zero <- clm_cdf(x, "log_gamma", list(lambda = 0))
  lg_neg <- clm_cdf(x, "log_gamma", list(lambda = -1))

  # Different lambda values should give different results
  expect_false(all(abs(lg_pos - lg_zero) < 1e-10))
  expect_false(all(abs(lg_neg - lg_zero) < 1e-10))
})

test_that("gev with xi=0 equals loglog (Gumbel)", {
  x <- seq(-3, 3, by = 0.5)

  # xi = 0: Gumbel distribution = loglog
  expect_equal(
    clm_cdf(x, "gev", list(xi = 0)),
    clm_cdf(x, "loglog", list()),
    tolerance = 1e-10
  )

  # xi very close to 0: should be close to loglog
  expect_equal(
    clm_cdf(x, "gev", list(xi = 1e-12)),
    clm_cdf(x, "loglog", list()),
    tolerance = 1e-6
  )
})

test_that("gev handles support constraints correctly", {
  # xi > 0 (Frechet): x > -1/xi
  # For xi = 0.5: x > -2 is required
  expect_equal(clm_cdf(-3, "gev", list(xi = 0.5)), 0)  # Outside support
  expect_true(clm_cdf(-1, "gev", list(xi = 0.5)) > 0)  # Inside support


  # xi < 0 (Weibull): x < -1/xi
  # For xi = -0.5: x < 2 is required
  expect_equal(clm_cdf(3, "gev", list(xi = -0.5)), 1)  # Outside support
  expect_true(clm_cdf(1, "gev", list(xi = -0.5)) < 1)  # Inside support
})

test_that("aep with theta1=theta2=2 is a valid symmetric CDF", {
  x <- seq(-3, 3, by = 0.5)

  # theta1 = theta2 = 2 gives a symmetric distribution
  # Note: While links.R documentation says it "equals probit", the AEP

  # is actually a different distribution family. We verify symmetry instead.
  aep_result <- clm_cdf(x, "aep", list(theta1 = 2, theta2 = 2))

  # Should be valid probabilities
  expect_true(all(aep_result >= 0))
  expect_true(all(aep_result <= 1))

  # Should be symmetric: F(x) + F(-x) = 1
  expect_equal(
    aep_result + clm_cdf(-x, "aep", list(theta1 = 2, theta2 = 2)),
    rep(1, length(x)),
    tolerance = 1e-10
  )

  # F(0) should equal 0.5
  expect_equal(clm_cdf(0, "aep", list(theta1 = 2, theta2 = 2)), 0.5)
})

test_that("aep is symmetric when theta1 = theta2", {
  x <- seq(-3, 3, by = 0.5)

  for (theta in c(0.5, 1, 2, 3)) {
    aep_result <- clm_cdf(x, "aep", list(theta1 = theta, theta2 = theta))

    # F(x) + F(-x) should equal 1 for symmetric distribution
    expect_equal(
      aep_result + clm_cdf(-x, "aep", list(theta1 = theta, theta2 = theta)),
      rep(1, length(x)),
      tolerance = 1e-10,
      info = paste("theta =", theta)
    )
  }
})

test_that("aep is asymmetric when theta1 != theta2", {
  x <- seq(-3, 3, by = 0.5)

  # Asymmetric case
  aep_asym <- clm_cdf(x, "aep", list(theta1 = 0.5, theta2 = 2))
  aep_asym_rev <- clm_cdf(-x, "aep", list(theta1 = 0.5, theta2 = 2))

  # F(x) + F(-x) should NOT equal 1 for asymmetric distribution
  expect_false(all(abs(aep_asym + aep_asym_rev - 1) < 1e-10))
})

test_that("aep rejects invalid theta parameters", {
  expect_error(aep_cdf(0, theta1 = 0, theta2 = 1))
  expect_error(aep_cdf(0, theta1 = 1, theta2 = 0))
  expect_error(aep_cdf(0, theta1 = -1, theta2 = 1))
})

test_that("clm_cdf returns valid probabilities", {
  x <- seq(-5, 5, by = 0.5)

  for (link in c("logit", "probit", "cloglog", "loglog", "cauchit")) {
    result <- clm_cdf(x, link, list())
    expect_true(all(result >= 0), info = paste(link, "should have F(x) >= 0"))
    expect_true(all(result <= 1), info = paste(link, "should have F(x) <= 1"))
  }

  # Flexible links with default parameters
  expect_true(all(clm_cdf(x, "tlink", list(df = 8)) >= 0))
  expect_true(all(clm_cdf(x, "tlink", list(df = 8)) <= 1))

  expect_true(all(clm_cdf(x, "aranda_ordaz", list(lambda = 1)) >= 0))
  expect_true(all(clm_cdf(x, "aranda_ordaz", list(lambda = 1)) <= 1))

  expect_true(all(clm_cdf(x, "sp", list(r = 1, base = "logit")) >= 0))
  expect_true(all(clm_cdf(x, "sp", list(r = 1, base = "logit")) <= 1))

  expect_true(all(clm_cdf(x, "log_gamma", list(lambda = 0)) >= 0))
  expect_true(all(clm_cdf(x, "log_gamma", list(lambda = 0)) <= 1))

  expect_true(all(clm_cdf(x, "aep", list(theta1 = 2, theta2 = 2)) >= 0))
  expect_true(all(clm_cdf(x, "aep", list(theta1 = 2, theta2 = 2)) <= 1))
})

test_that("clm_cdf is monotonically increasing", {
  x <- seq(-5, 5, by = 0.1)

  for (link in c("logit", "probit", "cloglog", "loglog", "cauchit")) {
    result <- clm_cdf(x, link, list())
    diffs <- diff(result)
    expect_true(all(diffs >= 0), info = paste(link, "should be monotonic"))
  }
})

test_that("clm_cdf rejects unknown link", {
  expect_error(clm_cdf(0, "unknown_link", list()))
})

test_that("%||% operator works correctly", {
  expect_equal(NULL %||% 5, 5)
  expect_equal(3 %||% 5, 3)
  expect_equal(0 %||% 5, 0)
  expect_equal(NA %||% 5, NA)
})
