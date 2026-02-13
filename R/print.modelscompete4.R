#' Print Method for modelscompete4
#'
#' @param x modelscompete4 object
#' @param ... Additional arguments
#' @export

print.modelscompete4 <- function(x, ...) {
  cat("Advanced Model Comparison with Latent Variable Support (modelscompete4)\n")
  cat(rep("=", 70), "\n", sep = "")

  # Print model types
  cat("\nMODEL TYPES:\n")
  cat(rep("-", 30), "\n", sep = "")
  for (i in 1:length(x$models)) {
    cat(sprintf("Model %d: %s\n", i, x$model_types[i]))
  }

  # Print comparison results
  cat("\nCOMPARISON RESULTS:\n")
  cat(rep("-", 30), "\n", sep = "")

  # Create comparison table
  if (!is.null(x$latent_comparison$fit_matrix)) {
    print(round(x$latent_comparison$fit_matrix, 3))

    # Print best models
    cat("\nBEST MODELS BY CRITERION:\n")
    for (crit in names(x$latent_comparison$best_by_criterion)) {
      best <- x$latent_comparison$best_by_criterion[crit]
      if (!is.na(best)) {
        cat(sprintf("  %s: Model %d\n", crit, best))
      }
    }
  }

  invisible(x)
}
