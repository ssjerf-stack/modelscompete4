compare_models_with_pvalues <- function(fit_list) {
  # Extract model names
  model_names <- names(fit_list)

  # Create comparison matrix
  n_models <- length(fit_list)
  p_value_matrix <- matrix(NA, n_models, n_models)
  rownames(p_value_matrix) <- colnames(p_value_matrix) <- model_names

  # Perform pairwise Vuong tests
  for (i in 1:(n_models-1)) {
    for (j in (i+1):n_models) {
      test_result <- suppressWarnings(nonnest2::vuongtest(fit_list[[i]], fit_list[[j]]))
      p_value_matrix[i, j] <- test_result$p_LRT$B
      p_value_matrix[j, i] <- test_result$p_LRT$B
    }
  }

  # Create summary
  summary_table <- data.frame(
    Model = model_names,
    AIC = sapply(fit_list, AIC),
    BIC = sapply(fit_list, BIC),
    CFI = sapply(fit_list, function(x) fitMeasures(x)["cfi"]),
    RMSEA = sapply(fit_list, function(x) fitMeasures(x)["rmsea"])
  )

  return(list(
    summary = summary_table,
    p_values = p_value_matrix,
    best_model = model_names[which.min(sapply(fit_list, AIC))]
  ))
}
