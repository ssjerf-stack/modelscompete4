#' Print Method for modelscompete4_advanced Objects
#
#' @param x A modelscompete4_advanced object
#' @param ... Additional arguments passed to print
#' @export
print.modelscompete4_advanced <- function(x, ...) {
  cat("\n=== MODEL COMPARISON RESULTS ===\n\n")
  
  # Print basic information
  cat("Models Compared:\n")
  for (i in seq_along(x$model_names)) {
    cat(sprintf(" %d. %s (%s)\n", i, x$model_names[i], x$model_types[i]))
  }
  
  cat(sprintf("\nComparison Type: %s\n", x$comparison_type))
  cat(sprintf("Test Method: %s\n", x$test_method))
  
  # Print fit indices table
  if (!is.null(x$fit_indices)) {
    cat("\n=== FIT INDICES ===\n")
    print(x$fit_indices, row.names = FALSE)
  }
  
  # Print chi-square difference tests if available
  if (!is.null(x$diff_tests)) {
    cat("\n=== CHI-SQUARE DIFFERENCE TESTS ===\n")
    for (test_name in names(x$diff_tests)) {
      test <- x$diff_tests[[test_name]]
      if (!is.null(test$error)) {
        cat(sprintf("\n%s: %s\n", test_name, test$error))
      } else {
        cat(sprintf("\n%s:\n", test_name))
        cat(sprintf("  chi-square difference = %.3f, df = %d, p = %.4f\n",
                    test$chisq_diff, test$df_diff, test$pvalue))
        # Add significance stars
        sig <- ifelse(test$pvalue < 0.001, "***",
                      ifelse(test$pvalue < 0.01, "**",
                             ifelse(test$pvalue < 0.05, "*", "ns")))
        cat(sprintf("  Significance: %s\n", sig))
      }
    }
  }
  
  # Add summary statement
  if (!is.null(x$fit_indices) && "AIC" %in% names(x$fit_indices)) {
    best_idx <- which.min(x$fit_indices$AIC)
    best_model <- x$fit_indices$Model[best_idx]
    cat(sprintf("\n=== SUMMARY ===\n"))
    cat(sprintf("Best model by AIC: %s\n", best_model))
  }
  
  invisible(x)
}
