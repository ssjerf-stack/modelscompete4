#' Compare Nested Models
#'
#' @description Internal function for comparing nested SEM models
#' @param models List of lavaan model objects
#' @param test_type Type of test: "lrt" (likelihood ratio test)
#' @param alpha Significance level
#'
#' @return List containing test results
#' @noRd
compare_nested_models <- function(models, test_type = "lrt", alpha = 0.05) {

  n_models <- length(models)

  if (test_type != "lrt") {
    stop("Only likelihood ratio test (LRT) is supported for nested models")
  }

  # Check if models are nested
  # This is simplified - actual nesting check would be more complex
  df_values <- sapply(models, function(x) fitMeasures(x)["df"])

  # Sort models by complexity (more parameters = smaller df)
  sorted_indices <- order(df_values, decreasing = TRUE)
  models <- models[sorted_indices]
  model_names <- names(models)

  # Perform likelihood ratio tests
  lrt_results <- data.frame(
    Comparison = character(),
    ChiSq_diff = numeric(),
    df_diff = numeric(),
    p_value = numeric(),
    Significance = character(),
    stringsAsFactors = FALSE
  )

  for (i in 1:(n_models - 1)) {
    for (j in (i + 1):n_models) {
      # Check if model j is nested within model i
      df_i <- fitMeasures(models[[i]])["df"]
      df_j <- fitMeasures(models[[j]])["df"]

      if (df_i > df_j) {  # Model i has more degrees of freedom = simpler model
        tryCatch({
          # Perform LRT
          lrt <- lavaan::lavTestLRT(models[[j]], models[[i]])

          chi_sq_diff <- lrt[2, "Chisq diff"]
          df_diff <- lrt[2, "Df diff"]
          p_val <- lrt[2, "Pr(>Chisq)"]

          significance <- ifelse(p_val < alpha, "Significant", "Not significant")

          lrt_results <- rbind(lrt_results, data.frame(
            Comparison = sprintf("%s vs %s", model_names[j], model_names[i]),
            ChiSq_diff = chi_sq_diff,
            df_diff = df_diff,
            p_value = p_val,
            Significance = significance,
            stringsAsFactors = FALSE
          ))
        }, error = function(e) {
          warning(sprintf("Error comparing %s and %s: %s",
                          model_names[j], model_names[i], e$message))
        })
      }
    }
  }

  return(list(
    test_method = "lrt",
    lrt_results = lrt_results,
    alpha = alpha
  ))
}
