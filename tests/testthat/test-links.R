# =============================================================================
# Tests for link function utilities
# =============================================================================

# =============================================================================
# supported_links() tests
# =============================================================================

test_that("supported_links returns character vector", {
  links <- supported_links()
  expect_type(links, "character")
  expect_true(length(links) > 0)
})

test_that("validate_link accepts valid links", {
  for (link in supported_links()) {
    expect_true(validate_link(link))
  }
})

test_that("validate_link rejects invalid links", {
  expect_error(validate_link("invalid_link"))
  expect_error(validate_link(""))
  expect_error(validate_link("LOGIT"))  # case sensitive
})

test_that("all expected links are supported", {
  expected <- c("logit", "probit", "cloglog", "loglog", "cauchit",
                "gev", "aep", "sp", "aranda_ordaz", "log_gamma")
  links <- supported_links()
  for (link in expected) {
    expect_true(link %in% links, info = paste("Missing link:", link))
  }
})

# =============================================================================
# Internal link function tests
# =============================================================================

test_that("is_flexible_link returns FALSE for standard links", {
  standard_links <- c("logit", "probit", "cloglog", "loglog", "cauchit")
  for (link in standard_links) {
    expect_false(is_flexible_link(link),
                 info = paste("Standard link should return FALSE:", link))
  }
})

test_that("is_flexible_link returns TRUE for flexible links", {
  flexible_links <- c("tlink", "aranda_ordaz", "gev", "sp", "log_gamma", "aep")
  for (link in flexible_links) {
    expect_true(is_flexible_link(link),
                info = paste("Flexible link should return TRUE:", link))
  }
})

test_that("get_link_params returns correct parameters", {
  # Standard links: empty vector
  expect_equal(get_link_params("logit"), character(0))
  expect_equal(get_link_params("probit"), character(0))
  expect_equal(get_link_params("cloglog"), character(0))
  expect_equal(get_link_params("loglog"), character(0))
  expect_equal(get_link_params("cauchit"), character(0))

  # Flexible links: specific parameters

  expect_equal(get_link_params("tlink"), "df")
  expect_equal(get_link_params("aranda_ordaz"), "lambda")
  expect_equal(get_link_params("gev"), "xi")
  expect_equal(get_link_params("sp"), c("r", "base"))
  expect_equal(get_link_params("log_gamma"), "lambda")
  expect_equal(get_link_params("aep"), c("theta1", "theta2"))
})

test_that("get_default_link_prior returns correct priors", {
  # tlink: df ~ gamma(2, 0.1)
  prior <- get_default_link_prior("tlink", "df")
  expect_equal(prior$family, "gamma")
  expect_equal(prior$args, c(2, 0.1))

  # aranda_ordaz: lambda ~ gamma(0.5, 0.5)
  prior <- get_default_link_prior("aranda_ordaz", "lambda")
  expect_equal(prior$family, "gamma")
  expect_equal(prior$args, c(0.5, 0.5))

  # gev: xi ~ normal(0, 2)
  prior <- get_default_link_prior("gev", "xi")
  expect_equal(prior$family, "normal")
  expect_equal(prior$args, c(0, 2))

  # sp: r ~ gamma(0.5, 0.5)
  prior <- get_default_link_prior("sp", "r")
  expect_equal(prior$family, "gamma")
  expect_equal(prior$args, c(0.5, 0.5))

  # log_gamma: lambda ~ normal(0, 1)
  prior <- get_default_link_prior("log_gamma", "lambda")
  expect_equal(prior$family, "normal")
  expect_equal(prior$args, c(0, 1))

  # aep: theta1 ~ gamma(2, 1), theta2 ~ gamma(2, 1)
  prior1 <- get_default_link_prior("aep", "theta1")
  expect_equal(prior1$family, "gamma")
  expect_equal(prior1$args, c(2, 1))

  prior2 <- get_default_link_prior("aep", "theta2")
  expect_equal(prior2$family, "gamma")
  expect_equal(prior2$args, c(2, 1))
})

test_that("get_default_link_prior returns NULL for invalid inputs", {
  # Standard links have no priors
  expect_null(get_default_link_prior("logit", "df"))
  expect_null(get_default_link_prior("probit", "param"))

  # Invalid parameter for flexible link

  expect_null(get_default_link_prior("tlink", "invalid_param"))
  expect_null(get_default_link_prior("aep", "df"))
})

test_that("get_link_type returns correct integer codes", {
  # Standard links: 1-5
  expect_equal(get_link_type("logit"), 1L)
  expect_equal(get_link_type("probit"), 2L)
  expect_equal(get_link_type("cloglog"), 3L)
  expect_equal(get_link_type("loglog"), 4L)
  expect_equal(get_link_type("cauchit"), 5L)

  # Flexible links: 6-11
  expect_equal(get_link_type("tlink"), 6L)
  expect_equal(get_link_type("aranda_ordaz"), 7L)
  expect_equal(get_link_type("sp"), 8L)
  expect_equal(get_link_type("log_gamma"), 9L)
  expect_equal(get_link_type("gev"), 10L)
  expect_equal(get_link_type("aep"), 11L)
})

test_that("get_link_type throws error for invalid link", {
  expect_error(get_link_type("invalid_link"), "Unknown link function")
})

test_that("get_sp_base_type returns correct integer codes", {
  # Only symmetric distributions are valid SP bases (Li et al., 2019)
  expect_equal(get_sp_base_type("logit"), 1L)
  expect_equal(get_sp_base_type("probit"), 2L)
  expect_equal(get_sp_base_type("cauchit"), 3L)
  expect_equal(get_sp_base_type("tlink"), 4L)
})

test_that("get_sp_base_type throws error for invalid base", {
  expect_error(get_sp_base_type("invalid_base"), "Unknown base distribution")
  # Asymmetric links are not valid SP bases (Li et al., 2019 requires symmetric)
  expect_error(get_sp_base_type("cloglog"), "Unknown base distribution")
  expect_error(get_sp_base_type("loglog"), "Unknown base distribution")
  # Flexible links (except tlink) are not valid base distributions
  expect_error(get_sp_base_type("aep"), "Unknown base distribution")
  expect_error(get_sp_base_type("gev"), "Unknown base distribution")
})
