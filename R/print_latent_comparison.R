#' Print Method for Latent Comparison Objects
#'
#' Prints a summary of latent model comparison results.
#'
#' @param x An object of class 'latent_comparison'
#' @param digits Number of digits to display (default: 3)
#' @param ... Additional arguments passed to print method
#'
#' @return Invisibly returns the input object
#' @export
#' @method print latent_comparison
print.latent_comparison <- function(x, digits = 3, ...) {
  cat("Latent Model Comparison\n")
  cat("=====================\n\n")
  cat("Number of models:", x$n_models, "\n")
  cat("Models nested:", x$nested, "\n")
  cat("Comparison method:", x$method, "\n\n")
  cat("Fit Indices Comparison:\n")
  cat("-----------------------\n")

  # Print fit comparison table
  if (nrow(x$fit_comparison) > 0) {
    # Round numeric columns for display
    fit_display <- x$fit_comparison
    numeric_cols <- sapply(fit_display, is.numeric)

    # Special handling for p-values
    if ("pvalue" %in% colnames(fit_display)) {
      # Format p-values properly
      fit_display$pvalue <- sapply(fit_display$pvalue, function(p) {
        if (is.na(p)) {
          return(NA)
        } else if (p < 0.001) {
          return("<0.001")
        } else if (p < 0.01) {
          return(sprintf("%.3f", p))
        } else {
          return(sprintf("%.3f", p))
        }
      })
    }

    # Round other numeric columns
    fit_display[numeric_cols] <- lapply(
      fit_display[numeric_cols],
      function(col) {
        if (is.numeric(col)) {
          return(round(col, digits = digits))
        } else {
          return(col)
        }
      }
    )

    print(fit_display, ...)
  } else {
    cat("No fit comparison data available.\n")
  }
  cat("\n")

  # Print difference tests if available
  if (!is.null(x$diff_tests) && length(x$diff_tests) > 0) {
    cat("Nested Model Comparisons (Chi-square difference tests):\n")
    cat("--------------------------------------------------------\n")
    for (test_name in names(x$diff_tests)) {
      cat("\n", test_name, ":\n", sep = "")
      print(x$diff_tests[[test_name]])
    }
  }

  invisible(x)
}
