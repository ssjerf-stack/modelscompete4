# tests/testthat/test_verification.R
# Verification tests for modelscompete4 package fixes

library(testthat)
library(lavaan)
library(modelscompete4)

# ----------------------------------------------------------------------
# Test: extract_latent_parameters works with both standardized and unstandardized
# ----------------------------------------------------------------------
test_that("extract_latent_parameters works with standardized = TRUE", {

  # Fit a simple CFA model
  model <- "F1 =~ x1 + x2 + x3 + x4"
  fit <- cfa(model, data = HolzingerSwineford1939, std.lv = TRUE)

  # Unstandardized loadings
  loadings <- extract_latent_parameters(fit, type = "loadings", standardized = FALSE)
  expect_true("est" %in% colnames(loadings))
  expect_true(nrow(loadings) > 0)

  # Standardized loadings – be flexible about column names
  std_loadings <- extract_latent_parameters(fit, type = "loadings", standardized = TRUE)
  has_any_est <- any(c("est", "std.all", "est.std") %in% colnames(std_loadings))
  expect_true(has_any_est)
  expect_true(nrow(std_loadings) > 0)
})

# ----------------------------------------------------------------------
# Test: print.modelscompete4 produces output
# ----------------------------------------------------------------------
test_that("print.modelscompete4 produces output", {

  # Define two nested models as syntax strings in a NAMED list
  model_list <- list(
    "OneFactor"   = "g =~ x1 + x2 + x3 + x4",
    "TwoFactor"   = "
      visual  =~ x1 + x2 + x3
      textual =~ x4 + x5 + x6
    "
  )

  # Run comparison – this is the correct interface
  result <- compare_models(model_list, data = HolzingerSwineford1939)

  # The print method should output something (not be silent)
  expect_output(print(result))
})

# ----------------------------------------------------------------------
# Test: bootstrap_lavaan_comparison is called correctly
# ----------------------------------------------------------------------
test_that("bootstrap_lavaan_comparison is called correctly", {

  # Fit two models (bootstrap function expects fitted lavaan objects)
  model1 <- "F1 =~ x1 + x2 + x3 + x4"
  model2 <- "F1 =~ x1 + x2 + x3 + x4 + x5 + x6"
  fit1   <- cfa(model1, data = HolzingerSwineford1939, std.lv = TRUE)
  fit2   <- cfa(model2, data = HolzingerSwineford1939, std.lv = TRUE)

  # Run with a small number of bootstrap draws (should not crash)
  result <- try(bootstrap_lavaan_comparison(fit1, fit2, R = 10), silent = TRUE)
  expect_false(inherits(result, "try-error"))

  # Returned object should have the expected class
  expect_s3_class(result, "bootstrap_lavaan_comparison")
})
