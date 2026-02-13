#' Compare Non-Nested Models
#'
#' @description Internal function for comparing non-nested SEM models
#' @param models List of lavaan model objects
#' @param test_type Type of test: "vuong" or "bootstrap"
#' @param alpha Significance level
#' @param bootstrap_iterations Number of bootstrap samples
#' @param parallel Use parallel processing
#'
#' @return List containing test results
#' @noRd
compare_non_nested_models <- function(models, test_type = "vuong", alpha = 0.05,
                                      bootstrap_iterations = 1000, parallel = FALSE) {

  n_models <- length(models)
  model_names <- names(models)

  # Initialize results matrices
  p_values <- matrix(NA, n_models, n_models)
  test_statistics <- matrix(NA, n_models, n_models)
  rownames(p_values) <- colnames(p_values) <- model_names
  rownames(test_statistics) <- colnames(test_statistics) <- model_names

  # Load required package
  if (!requireNamespace("nonnest2", quietly = TRUE)) {
    stop("Package 'nonnest2' is required for Vuong tests. Please install it.")
  }

  # Perform pairwise comparisons
  if (test_type == "vuong") {
    cat("Performing Vuong tests for non-nested models...\n")

    for (i in 1:(n_models - 1)) {
      for (j in (i + 1):n_models) {
        tryCatch({
          # Suppress warnings during test
          test_result <- suppressWarnings(
            nonnest2::vuongtest(models[[i]], models[[j]])
          )

          test_statistics[i, j] <- test_statistics[j, i] <- test_result$LRTstat
          p_values[i, j] <- p_values[j, i] <- test_result$p_LRT$B

        }, error = function(e) {
          warning(sprintf("Error comparing %s vs %s: %s",
                          model_names[i], model_names[j], e$message))
        })
      }
    }

    # Interpret results
    interpretation <- interpret_vuong_results(p_values, test_statistics, alpha, model_names)

    return(list(
      test_method = "vuong",
      p_values = p_values,
      test_statistics = test_statistics,
      interpretation = interpretation,
      alpha = alpha
    ))

  } else if (test_type == "bootstrap") {
    cat("Performing bootstrap comparison... (This may take a while)\n")

    # Bootstrap comparison would go here
    # This is a placeholder - actual bootstrap implementation would be more complex
    warning("Bootstrap method not fully implemented. Using Vuong test instead.")
    return(compare_non_nested_models(models, "vuong", alpha, bootstrap_iterations, parallel))

  } else {
    stop(sprintf("Test type '%s' not supported for non-nested models", test_type))
  }
}

#' Interpret Vuong Test Results
#' @noRd
interpret_vuong_results <- function(p_values, test_statistics, alpha, model_names) {
  n_models <- length(model_names)
  interpretations <- list()

  for (i in 1:(n_models - 1)) {
    for (j in (i + 1):n_models) {
      p_val <- p_values[i, j]
      stat <- test_statistics[i, j]

      if (!is.na(p_val)) {
        if (p_val < alpha) {
          if (stat > 0) {
            interpretations[[paste(model_names[i], "vs", model_names[j])]] <-
              sprintf("%s fits significantly better than %s (p = %.4f)",
                      model_names[i], model_names[j], p_val)
          } else {
            interpretations[[paste(model_names[i], "vs", model_names[j])]] <-
              sprintf("%s fits significantly better than %s (p = %.4f)",
                      model_names[j], model_names[i], p_val)
          }
        } else {
          interpretations[[paste(model_names[i], "vs", model_names[j])]] <-
            sprintf("No significant difference between %s and %s (p = %.4f)",
                    model_names[i], model_names[j], p_val)
        }
      }
    }
  }

  return(interpretations)
}
