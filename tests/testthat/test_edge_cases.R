library(testthat)
library(lavaan)
library(modelscompete4)

test_that("single model comparison works", {
  HS_data <- HolzingerSwineford1939

  # Even for single model, need to pass as named list
  model_list <- list(
    "SingleModel" = 'visual =~ x1 + x2 + x3'
  )

  # This should fail because compare_models needs at least 2 models
  expect_error(compare_models(model_list, data = HS_data))
})

test_that("many models can be compared", {
  HS_data <- HolzingerSwineford1939

  model_list <- list(
    "M1" = 'visual =~ x1',
    "M2" = 'visual =~ x1 + x2',
    "M3" = 'visual =~ x1 + x2 + x3',
    "M4" = 'visual =~ x1 + x2 + x3 + x4'
  )

  result <- compare_models(model_list, data = HS_data)
  expect_equal(length(result$fit_list), 4)
})
