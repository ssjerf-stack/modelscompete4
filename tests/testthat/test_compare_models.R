library(testthat)
library(lavaan)
library(modelscompete4)

test_that("compare_models returns correct class", {
  # Use HolzingerSwineford1939 dataset from lavaan
  HS_data <- HolzingerSwineford1939

  # Define models as syntax strings in a NAMED list
  model_list <- list(
    "OneFactor" = 'visual  =~ x1 + x2 + x3',
    "TwoFactor" = '
      visual  =~ x1 + x2 + x3
      textual =~ x4 + x5 + x6
    '
  )

  result <- compare_models(model_list, data = HS_data)
  expect_s3_class(result, "modelscompete4")
  expect_equal(length(result$fit_list), 2)
})

test_that("model_names are correctly assigned", {
  HS_data <- HolzingerSwineford1939

  model_list <- list(
    "Two-Factor" = '
      visual  =~ x1 + x2 + x3
      textual =~ x4 + x5 + x6
    ',
    "One-Factor" = 'g =~ x1 + x2 + x3 + x4 + x5 + x6'
  )

  result <- compare_models(model_list, data = HS_data)
  expect_equal(names(result$fit_list), c("Two-Factor", "One-Factor"))
})

# Add similar corrections for all other tests...
