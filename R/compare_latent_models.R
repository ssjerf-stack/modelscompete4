#' Compare Latent Variable Models
#'
#' @param ... lavaan model objects
#' @param nested logical, whether models are nested
#' @param fit_measures character vector of fit measures to extract
#' @param method comparison method
#' @return A latent_comparison object
#' @export
compare_latent_models <- function(..., nested = FALSE,
                                  fit_measures = c("chisq", "df", "pvalue",
                                                   "cfi", "tli", "rmsea", "srmr"),
                                  method = "default") {

  models <- list(...)

  # Check if any models were provided
  if (length(models) == 0) {
    stop("At least one model must be provided")
  }

  # Check all models are lavaan objects
  is_lavaan <- sapply(models, function(x) inherits(x, "lavaan"))
  if (!all(is_lavaan)) {
    non_lavaan <- which(!is_lavaan)
    warning("Models ", paste(non_lavaan, collapse = ", "),
            " are not lavaan objects and will be skipped")
    models <- models[is_lavaan]
  }

  if (length(models) == 0) {
    stop("No valid lavaan models provided")
  }

  # Create model names
  model_names <- character(length(models))
  for (i in seq_along(models)) {
    if (!is.null(models[[i]]@Model@model.type)) {
      model_names[i] <- paste("Model", i, "-", models[[i]]@Model@model.type)
    } else {
      model_names[i] <- paste("Model", i)
    }
  }

  # Extract fit measures for each model
  fit_comparison <- data.frame()
  for (i in seq_along(models)) {
    fit_values <- lavaan::fitMeasures(models[[i]], fit.measures = fit_measures)
    fit_row <- data.frame(
      Model = model_names[i],
      as.list(fit_values),
      stringsAsFactors = FALSE
    )
    fit_comparison <- rbind(fit_comparison, fit_row)
  }

  # Perform difference tests if nested
  diff_tests <- NULL
  if (nested && length(models) > 1) {
    diff_tests <- list()
    for (i in 2:length(models)) {
      test_name <- paste(model_names[i-1], "vs", model_names[i])
      test_result <- tryCatch({
        lavaan::anova(models[[i-1]], models[[i]])
      }, error = function(e) {
        list(error = e$message)
      })
      diff_tests[[test_name]] <- test_result
    }
  }

  # Create result object
  result <- list(
    fit_comparison = fit_comparison,
    diff_tests = diff_tests,
    nested = nested,
    method = method,
    n_models = length(models),
    fit_measures = fit_measures,
    models = models,
    model_names = model_names
  )

  class(result) <- "latent_comparison"
  return(result)
}

#' Print method for latent_comparison objects
#'
#' @param x latent_comparison object
#' @param ... additional arguments
#' @export
print.latent_comparison <- function(x, ...) {
  cat("Latent Model Comparison Results\n")
  cat("===============================\n\n")

  cat(sprintf("Number of models: %d\n", x$n_models))
  cat(sprintf("Models nested: %s\n", ifelse(x$nested, "Yes", "No")))
  cat(sprintf("Comparison method: %s\n\n", x$method))

  cat("Fit Indices:\n")
  print(x$fit_comparison, row.names = FALSE)
  cat("\n")

  if (!is.null(x$diff_tests) && length(x$diff_tests) > 0) {
    cat("Chi-Square Difference Tests:\n")
    for (test_name in names(x$diff_tests)) {
      cat(sprintf("\n%s:\n", test_name))
      print(x$diff_tests[[test_name]])
    }
  }

  invisible(x)
}
