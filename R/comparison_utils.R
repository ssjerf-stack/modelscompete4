# File: D:/R_Workspace/Pack4/modelscompete4/R/comparison_utils.R
# Utility functions for model comparison

#' @importFrom lavaan fitMeasures
#' @importFrom stats AIC BIC
NULL

#' Extract Fit Indices from Models
#' @noRd
extract_fit_indices <- function(models, criteria) {
  model_names <- names(models)
  n_models <- length(models)

  # Initialize results data frame
  results <- data.frame(Model = model_names)

  # Extract requested fit indices
  for (criterion in criteria) {
    values <- numeric(n_models)
    for (i in 1:n_models) {
      fm <- lavaan::fitMeasures(models[[i]])
      if (criterion %in% names(fm)) {
        values[i] <- fm[criterion]
      } else if (criterion == "AIC") {
        values[i] <- stats::AIC(models[[i]])
      } else if (criterion == "BIC") {
        values[i] <- stats::BIC(models[[i]])
      } else {
        values[i] <- NA
        warning(sprintf("Criterion '%s' not available for model %s",
                        criterion, model_names[i]))
      }
    }
    results[[criterion]] <- values
  }
  return(results)
}

#' Calculate AIC Weights
#' @noRd
calculate_aic_weights <- function(aic_values) {
  # Calculate delta AIC
  delta_aic <- aic_values - min(aic_values)
  # Calculate AIC weights
  aic_weights <- exp(-0.5 * delta_aic) / sum(exp(-0.5 * delta_aic))
  return(aic_weights)
}

#' Determine Best Model
#' @noRd
determine_best_model <- function(fit_indices, comparison_results) {
  model_names <- fit_indices$Model
  n_models <- length(model_names)

  # Initialize scoring
  scores <- rep(0, n_models)
  names(scores) <- model_names

  # Score based on each criterion
  if ("AIC" %in% names(fit_indices)) {
    best_aic <- model_names[which.min(fit_indices$AIC)]
    scores[best_aic] <- scores[best_aic] + 1
  }
  if ("BIC" %in% names(fit_indices)) {
    best_bic <- model_names[which.min(fit_indices$BIC)]
    scores[best_bic] <- scores[best_bic] + 1
  }
  if ("CFI" %in% names(fit_indices)) {
    best_cfi <- model_names[which.max(fit_indices$CFI)]
    scores[best_cfi] <- scores[best_cfi] + 1
  }
  if ("RMSEA" %in% names(fit_indices)) {
    best_rmsea <- model_names[which.min(fit_indices$RMSEA)]
    scores[best_rmsea] <- scores[best_rmsea] + 1
  }

  # Consider statistical test results
  if ("p_values" %in% names(comparison_results)) {
    p_matrix <- comparison_results$p_values
    alpha <- comparison_results$alpha
    for (i in 1:(n_models - 1)) {
      for (j in (i + 1):n_models) {
        if (!is.na(p_matrix[i, j]) && p_matrix[i, j] < alpha) {
          # Model i is significantly better than j
          scores[model_names[i]] <- scores[model_names[i]] + 1
        }
      }
    }
  }

  # Determine ranking
  ranking <- names(sort(scores, decreasing = TRUE))
  best_model <- ranking[1]

  # Determine which criteria favor the best model
  best_criteria <- character()
  if ("AIC" %in% names(fit_indices) &&
      best_model == model_names[which.min(fit_indices$AIC)]) {
    best_criteria <- c(best_criteria, "AIC")
  }
  if ("BIC" %in% names(fit_indices) &&
      best_model == model_names[which.min(fit_indices$BIC)]) {
    best_criteria <- c(best_criteria, "BIC")
  }
  if ("CFI" %in% names(fit_indices) &&
      best_model == model_names[which.max(fit_indices$CFI)]) {
    best_criteria <- c(best_criteria, "CFI")
  }
  if ("RMSEA" %in% names(fit_indices) &&
      best_model == model_names[which.min(fit_indices$RMSEA)]) {
    best_criteria <- c(best_criteria, "RMSEA")
  }

  return(list(
    best_model = best_model,
    criteria = best_criteria,
    ranking = ranking,
    scores = scores
  ))
}
