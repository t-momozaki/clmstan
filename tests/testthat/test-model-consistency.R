# =============================================================================
# Tests for model consistency across all Stan files
# =============================================================================
#
# These tests ensure that when one Stan model is modified,
# related models are also updated consistently.

# =============================================================================
# Helper functions
# =============================================================================

# Extract data block variables from Stan file
extract_data_vars <- function(stan_file) {
  if (!file.exists(stan_file)) {
    skip(paste("Stan file not found:", stan_file))
  }

  content <- readLines(stan_file, warn = FALSE)
  content_str <- paste(content, collapse = "\n")

  # Extract data block

  data_match <- regmatches(
    content_str,
    regexpr("data\\s*\\{[^}]+\\}", content_str, perl = TRUE)
  )

  if (length(data_match) == 0) {
    return(character(0))
  }

  # Extract variable names from data block

  # Simpler approach: find variable names that appear before semicolons
  # Pattern: word followed by semicolon (with possible spaces and comments)
  var_pattern <- "([a-zA-Z_][a-zA-Z0-9_]*)\\s*;"
  matches <- gregexpr(var_pattern, data_match, perl = TRUE)
  vars <- regmatches(data_match, matches)[[1]]

  # Extract just the variable names (remove the semicolon)
  gsub(";", "", gsub("\\s+", "", vars))
}

# Get Stan file paths
get_stan_path <- function(filename) {
  file.path(
    system.file("stan", package = "clmstan"),
    filename
  )
}

# Paths relative to package root (for development)
dev_stan_path <- function(filename) {
  file.path(
    "..", "..", "src", "stan",
    filename
  )
}

test_that("all models include clm_common.stan", {
  stan_files <- c(
    "clm_base.stan",
    "clm_equidistant.stan",
    "clm_symmetric.stan",
    "clm_full.stan",
    "clm_equidistant_full.stan",
    "clm_symmetric_full.stan"
  )

  for (filename in stan_files) {
    path <- dev_stan_path(filename)
    if (file.exists(path)) {
      content <- readLines(path, warn = FALSE)
      has_include <- any(grepl("#include functions/clm_common.stan", content))
      expect_true(
        has_include,
        info = paste(filename, "should include clm_common.stan")
      )
    }
  }
})

test_that("all models have RELATED FILES comment", {
  stan_files <- c(
    "clm_base.stan",
    "clm_equidistant.stan",
    "clm_symmetric.stan",
    "clm_full.stan",
    "clm_equidistant_full.stan",
    "clm_symmetric_full.stan"
  )

  for (filename in stan_files) {
    path <- dev_stan_path(filename)
    if (file.exists(path)) {
      content <- readLines(path, warn = FALSE)
      has_related <- any(grepl("RELATED FILES", content))
      expect_true(
        has_related,
        info = paste(filename, "should have RELATED FILES comment")
      )
    }
  }
})

test_that("base models have consistent common data variables", {
  # Variables that should be in ALL base models (non-flexible)
  common_vars <- c("K", "N", "P", "y", "X", "link_type")

  base_files <- c(
    "clm_base.stan",
    "clm_equidistant.stan",
    "clm_symmetric.stan"
  )

  for (filename in base_files) {
    path <- dev_stan_path(filename)
    if (file.exists(path)) {
      vars <- extract_data_vars(path)
      for (v in common_vars) {
        expect_true(
          v %in% vars,
          info = paste(filename, "should have data variable:", v)
        )
      }
    }
  }
})

test_that("full models have consistent estimation flags", {
  # Estimation flags that should be in ALL full models
  estimation_flags <- c(
    "estimate_df",
    "estimate_lambda",
    "estimate_xi",
    "estimate_r",
    "estimate_theta1",
    "estimate_theta2"
  )

  full_files <- c(
    "clm_full.stan",
    "clm_equidistant_full.stan",
    "clm_symmetric_full.stan"
  )

  for (filename in full_files) {
    path <- dev_stan_path(filename)
    if (file.exists(path)) {
      vars <- extract_data_vars(path)
      for (flag in estimation_flags) {
        expect_true(
          flag %in% vars,
          info = paste(filename, "should have estimation flag:", flag)
        )
      }
    }
  }
})

test_that("all models have consistent link parameter variables", {
  # Link parameters that should be in all models
  link_params <- c("df", "lambda", "xi", "r", "base_type", "theta1", "theta2")

  # For base models, these are direct data variables
  base_files <- c(
    "clm_base.stan",
    "clm_equidistant.stan",
    "clm_symmetric.stan"
  )

  for (filename in base_files) {
    path <- dev_stan_path(filename)
    if (file.exists(path)) {
      content <- paste(readLines(path, warn = FALSE), collapse = "\n")
      for (param in link_params) {
        # Check that the parameter is mentioned in data block
        has_param <- grepl(paste0("\\b", param, "\\b"), content)
        expect_true(
          has_param,
          info = paste(filename, "should reference link parameter:", param)
        )
      }
    }
  }
})

test_that("R prepare functions match Stan data requirements", {
  # Check that prepare_stan_data returns expected variables
  skip_if_not_installed("clmstan")

  # Create mock data
  data <- data.frame(
    y = factor(c(1, 2, 3, 2, 1, 3, 2, 3, 1, 2)),
    x = rnorm(10)
  )

  # Test basic prepare function
  stan_data <- prepare_stan_data(y ~ x, data)

  expected_vars <- c("K", "N", "P", "y", "X", "link_type",
                     "df", "lambda", "xi", "r", "base_type", "theta1", "theta2",
                     "prior_beta_type", "prior_beta_mu", "prior_beta_sd", "prior_beta_df",
                     "prior_c_sd")

  for (v in expected_vars) {
    expect_true(
      v %in% names(stan_data),
      info = paste("prepare_stan_data should return:", v)
    )
  }
})

test_that("Stan data includes beta prior type parameters", {
  skip_if_not_installed("clmstan")

  data <- data.frame(
    y = factor(c(1, 2, 3, 2, 1)),
    x = rnorm(5)
  )

  stan_data <- prepare_stan_data(y ~ x, data)

  # Check beta prior parameters exist
  expect_true("prior_beta_type" %in% names(stan_data))
  expect_true("prior_beta_mu" %in% names(stan_data))
  expect_true("prior_beta_sd" %in% names(stan_data))
  expect_true("prior_beta_df" %in% names(stan_data))

  # Check default values
  expect_equal(stan_data$prior_beta_type, PRIOR_TYPES$normal)
  expect_equal(stan_data$prior_beta_mu, 0)
  expect_equal(stan_data$prior_beta_sd, 2.5)
})
