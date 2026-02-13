# ============================================================================
# TEST SCRIPT: Three Non-nested Latent Variable Model Comparison
# Advanced comparison of 3 competing factor structures
# ============================================================================

cat("=========================================================\n")
cat("THREE NON-NESTED MODEL COMPARISON ANALYSIS\n")
cat("=========================================================\n\n")

# ----------------------------------------------------------------------------
# 1. Load required packages and custom functions
# ----------------------------------------------------------------------------
cat("1. LOADING PACKAGES AND FUNCTIONS\n")
cat(rep("-", 50), "\n", sep = "")

# Load core packages
library(modelscompete4)
library(lavaan)
library(psych)  # For additional psychometric functions

# Load our custom fixes if needed
if (file.exists("fix_package_functions.R")) {
  source("fix_package_functions.R")
  cat("✓ Custom fixes loaded\n")
} else {
  cat("⚠ Custom fixes not found, using package functions directly\n")
}

cat("✓ Packages loaded successfully\n\n")

# ----------------------------------------------------------------------------
# 2. Data Preparation
# ----------------------------------------------------------------------------
cat("2. DATA PREPARATION\n")
cat(rep("-", 50), "\n", sep = "")

# Use classic Holzinger-Swineford dataset
data(HolzingerSwineford1939)

# Select variables for analysis
variables <- c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9")
HS_data <- HolzingerSwineford1939[, variables]

# Create standardized dataset
HS_std <- as.data.frame(scale(HS_data))

cat("Dataset Information:\n")
cat(sprintf("  Sample size: N = %d\n", nrow(HS_std)))
cat(sprintf("  Variables: %d observed indicators\n", ncol(HS_std)))
cat("  All variables standardized (M=0, SD=1)\n")

# Basic descriptives
cat("\nDescriptive Statistics:\n")
desc_stats <- data.frame(
  Variable = variables,
  Mean = round(colMeans(HS_std), 3),
  SD = round(apply(HS_std, 2, sd), 3),
  Min = round(apply(HS_std, 2, min), 3),
  Max = round(apply(HS_std, 2, max), 3)
)
print(desc_stats, row.names = FALSE)

cat("✓ Data prepared successfully\n\n")

# ----------------------------------------------------------------------------
# 3. Define THREE Non-nested Factor Models
# ----------------------------------------------------------------------------
cat("3. MODEL SPECIFICATION\n")
cat(rep("-", 50), "\n", sep = "")

# MODEL 1: Original Three-Factor Model (Holzinger & Swineford, 1939)
model_3factor <- '
  # Latent factors
  Visual =~ x1 + x2 + x3
  Textual =~ x4 + x5 + x6
  Speed =~ x7 + x8 + x9

  # Factor correlations (freely estimated)
  Visual ~~ Textual + Speed
  Textual ~~ Speed
'

# MODEL 2: Alternative Two-Factor Model (Cognitive vs Perceptual)
model_2factor <- '
  # Cognitive factor (verbal/numerical tasks)
  Cognitive =~ x4 + x5 + x6 + x7

  # Perceptual factor (visual/spatial tasks)
  Perceptual =~ x1 + x2 + x3 + x8 + x9

  # Allow factors to correlate
  Cognitive ~~ Perceptual
'

# MODEL 3: Hierarchical Model (General + Specific factors)
model_hierarchical <- '
  # Second-order general factor
  g =~ Visual + Textual + Speed

  # First-order specific factors
  Visual =~ x1 + x2 + x3
  Textual =~ x4 + x5 + x6
  Speed =~ x7 + x8 + x9

  # Fix general factor variance for identification
  g ~~ 1*g
'

cat("Three Non-nested Models Specified:\n")
cat("1. THREE-FACTOR: Visual, Textual, Speed (original)\n")
cat("2. TWO-FACTOR: Cognitive, Perceptual (alternative)\n")
cat("3. HIERARCHICAL: General factor + three specific factors\n")
cat("✓ All models are non-nested (different factor structures)\n\n")

# ----------------------------------------------------------------------------
# 4. Model Estimation
# ----------------------------------------------------------------------------
cat("4. MODEL ESTIMATION\n")
cat(rep("-", 50), "\n", sep = "")

cat("Fitting Model 1 (Three-Factor)...\n")
fit_3factor <- tryCatch({
  fit <- cfa(model_3factor, data = HS_std, std.lv = TRUE)
  cat(sprintf("  ✓ Converged: CFI=%.3f, RMSEA=%.3f\n",
              fitMeasures(fit)["cfi"], fitMeasures(fit)["rmsea"]))
  fit
}, error = function(e) {
  cat(sprintf("  ✗ Failed: %s\n", e$message))
  NULL
})

cat("\nFitting Model 2 (Two-Factor)...\n")
fit_2factor <- tryCatch({
  fit <- cfa(model_2factor, data = HS_std, std.lv = TRUE)
  cat(sprintf("  ✓ Converged: CFI=%.3f, RMSEA=%.3f\n",
              fitMeasures(fit)["cfi"], fitMeasures(fit)["rmsea"]))
  fit
}, error = function(e) {
  cat(sprintf("  ✗ Failed: %s\n", e$message))
  NULL
})

cat("\nFitting Model 3 (Hierarchical)...\n")
fit_hierarchical <- tryCatch({
  fit <- cfa(model_hierarchical, data = HS_std, std.lv = FALSE)
  cat(sprintf("  ✓ Converged: CFI=%.3f, RMSEA=%.3f\n",
              fitMeasures(fit)["cfi"], fitMeasures(fit)["rmsea"]))
  fit
}, error = function(e) {
  cat(sprintf("  ✗ Failed: %s\n", e$message))
  NULL
})

# Check if all models converged
models_converged <- c(!is.null(fit_3factor), !is.null(fit_2factor), !is.null(fit_hierarchical))
if (all(models_converged)) {
  cat("\n✓ All three models converged successfully\n")
} else {
  cat(sprintf("\n⚠ Warning: %d model(s) failed to converge\n",
              sum(!models_converged)))
}

# ----------------------------------------------------------------------------
# 5. Pairwise Model Comparisons (All combinations)
# ----------------------------------------------------------------------------
cat("\n5. PAIRWISE MODEL COMPARISONS\n")
cat(rep("-", 50), "\n", sep = "")

# Function to conduct and report pairwise comparison
compare_pair <- function(fit1, fit2, name1, name2) {
  cat(sprintf("\n%s vs %s:\n", name1, name2))

  comparison <- compare_latent_models(fit1, fit2)

  # Extract key information
  nested_status <- comparison$nested
  fit_table <- comparison$fit_comparison

  cat(sprintf("  Nested: %s\n", nested_status))
  cat(sprintf("  AIC difference: %.1f (lower is better)\n",
              fit_table$aic[1] - fit_table$aic[2]))
  cat(sprintf("  BIC difference: %.1f (lower is better)\n",
              fit_table$bic[1] - fit_table$bic[2]))
  cat(sprintf("  CFI difference: %.3f (higher is better)\n",
              fit_table$cfi[1] - fit_table$cfi[2]))

  # Determine which model is better
  better_by_aic <- ifelse(fit_table$aic[1] < fit_table$aic[2], name1, name2)
  better_by_cfi <- ifelse(fit_table$cfi[1] > fit_table$cfi[2], name1, name2)

  cat(sprintf("  Better by AIC: %s\n", better_by_aic))
  cat(sprintf("  Better by CFI: %s\n", better_by_cfi))

  return(comparison)
}

# Compare all pairs
if (!is.null(fit_3factor) && !is.null(fit_2factor)) {
  comp_3v2 <- compare_pair(fit_3factor, fit_2factor, "Three-Factor", "Two-Factor")
}

if (!is.null(fit_3factor) && !is.null(fit_hierarchical)) {
  comp_3vH <- compare_pair(fit_3factor, fit_hierarchical, "Three-Factor", "Hierarchical")
}

if (!is.null(fit_2factor) && !is.null(fit_hierarchical)) {
  comp_2vH <- compare_pair(fit_2factor, fit_hierarchical, "Two-Factor", "Hierarchical")
}

# ----------------------------------------------------------------------------
# 6. Three-Model Advanced Comparison
# ----------------------------------------------------------------------------
cat("\n6. THREE-MODEL ADVANCED COMPARISON\n")
cat(rep("-", 50), "\n", sep = "")

# Prepare model list
model_list <- list(
  "Three-Factor" = fit_3factor,
  "Two-Factor" = fit_2factor,
  "Hierarchical" = fit_hierarchical
)

# Remove any NULL models
model_list <- model_list[!sapply(model_list, is.null)]

if (length(model_list) >= 2) {
  cat(sprintf("Comparing %d models using advanced comparison...\n", length(model_list)))

  # Use either the fixed function or original
  if (exists("fixed_compare_models_advanced_lv")) {
    advanced_comp <- fixed_compare_models_advanced_lv(model_list)
  } else {
    advanced_comp <- compare_models_advanced_lv(model_list)
  }

  # Display comparison table
  if (!is.null(advanced_comp$comparison_table)) {
    cat("\nAdvanced Comparison Table:\n")
    print(advanced_comp$comparison_table)
  }

  # Display ranking if available
  if (!is.null(advanced_comp$model_ranking)) {
    cat("\nModel Ranking (based on AIC):\n")
    print(advanced_comp$model_ranking)

    # Determine best model
    best_model <- advanced_comp$model_ranking$Model[1]
    cat(sprintf("\n🏆 BEST MODEL: %s\n", best_model))
  }
} else {
  cat("⚠ Not enough converged models for advanced comparison\n")
}

# ----------------------------------------------------------------------------
# 7. Model Fit Summary Table
# ----------------------------------------------------------------------------
cat("\n7. MODEL FIT SUMMARY\n")
cat(rep("-", 50), "\n", sep = "")

# Create comprehensive fit summary
create_fit_summary <- function(fit_list) {
  summary_df <- data.frame(
    Model = names(fit_list),
    χ² = NA,
    df = NA,
    "χ²/df" = NA,
    CFI = NA,
    TLI = NA,
    RMSEA = NA,
    SRMR = NA,
    AIC = NA,
    BIC = NA,
    stringsAsFactors = FALSE
  )

  for (i in seq_along(fit_list)) {
    fit <- fit_list[[i]]
    if (!is.null(fit)) {
      measures <- fitMeasures(fit)
      summary_df$χ²[i] <- round(measures["chisq"], 2)
      summary_df$df[i] <- measures["df"]
      summary_df$"χ²/df"[i] <- round(measures["chisq"] / measures["df"], 2)
      summary_df$CFI[i] <- round(measures["cfi"], 3)
      summary_df$TLI[i] <- round(measures["tli"], 3)
      summary_df$RMSEA[i] <- round(measures["rmsea"], 3)
      summary_df$SRMR[i] <- round(measures["srmr"], 3)
      summary_df$AIC[i] <- round(AIC(fit), 1)
      summary_df$BIC[i] <- round(BIC(fit), 1)
    }
  }

  return(summary_df)
}

if (length(model_list) > 0) {
  fit_summary <- create_fit_summary(model_list)
  print(fit_summary, row.names = FALSE)

  # Interpretation guidelines
  cat("\nFit Index Interpretation Guidelines:\n")
  cat("  CFI/TLI: >0.90 = Acceptable, >0.95 = Good\n")
  cat("  RMSEA: <0.08 = Acceptable, <0.06 = Good\n")
  cat("  SRMR: <0.08 = Good\n")
  cat("  χ²/df: <3 = Good, <5 = Acceptable\n")
}

# ----------------------------------------------------------------------------
# 8. Bootstrap Comparison (Model 1 vs Model 2)
# ----------------------------------------------------------------------------
cat("\n8. BOOTSTRAP MODEL COMPARISON\n")
cat(rep("-", 50), "\n", sep = "")

if (!is.null(fit_3factor) && !is.null(fit_2factor)) {
  cat("Running bootstrap comparison (Three-Factor vs Two-Factor)...\n")
  cat("Note: This may take a few moments...\n")

  # Use smaller bootstrap for demonstration
  if (exists("fixed_bootstrap_lavaan_comparison")) {
    bootstrap_result <- fixed_bootstrap_lavaan_comparison(fit_3factor, fit_2factor, R = 200)
  } else {
    bootstrap_result <- bootstrap_lavaan_comparison(fit_3factor, fit_2factor)
  }

  if (!is.null(bootstrap_result$boot_results)) {
    cat("✓ Bootstrap completed successfully\n")

    # Extract bootstrap confidence intervals
    if (!is.null(bootstrap_result$boot_results$t0)) {
      diff_aic <- bootstrap_result$boot_results$t0[1]
      diff_bic <- bootstrap_result$boot_results$t0[2]

      cat(sprintf("\nOriginal Differences:\n"))
      cat(sprintf("  ΔAIC: %.1f (negative favors Three-Factor)\n", diff_aic))
      cat(sprintf("  ΔBIC: %.1f (negative favors Three-Factor)\n", diff_bic))

      # Interpretation
      if (diff_aic < -10) {
        cat("  Interpretation: Strong evidence for Three-Factor model (ΔAIC > 10)\n")
      } else if (diff_aic < -4) {
        cat("  Interpretation: Moderate evidence for Three-Factor model (ΔAIC > 4)\n")
      } else if (diff_aic < 0) {
        cat("  Interpretation: Weak evidence for Three-Factor model\n")
      } else {
        cat("  Interpretation: Evidence favors Two-Factor model\n")
      }
    }
  }
} else {
  cat("⚠ Bootstrap comparison requires both Three-Factor and Two-Factor models\n")
}

# ----------------------------------------------------------------------------
# 9. Visualization
# ----------------------------------------------------------------------------
cat("\n9. VISUALIZATION\n")
cat(rep("-", 50), "\n", sep = "")

# Create comparison plot
if (length(model_list) >= 2) {
  cat("Creating model comparison visualization...\n")

  # Select two models for plotting
  plot_models <- list(fit_3factor, fit_2factor)
  names(plot_models) <- c("Three-Factor", "Two-Factor")

  # Try to create plot
  tryCatch({
    if (exists("fixed_plot_latent_comparison")) {
      plot_result <- fixed_plot_latent_comparison(plot_models)
    } else {
      plot_result <- plot_latent_comparison(plot_models)
    }

    cat("✓ Plot created successfully\n")
  }, error = function(e) {
    cat(sprintf("⚠ Plot creation failed: %s\n", e$message))
    cat("Creating alternative visualization...\n")

    # Create simple bar plot
    png("three_model_comparison.png", width = 1000, height = 800)
    par(mfrow = c(2, 2))

    # Plot 1: CFI comparison
    cfis <- sapply(model_list, function(fit) fitMeasures(fit)["cfi"])
    barplot(cfis, main = "CFI Comparison", ylab = "CFI", ylim = c(0, 1),
            col = "lightblue", names.arg = names(cfis))
    abline(h = 0.90, col = "red", lty = 2)
    abline(h = 0.95, col = "green", lty = 2)

    # Plot 2: RMSEA comparison
    rmseas <- sapply(model_list, function(fit) fitMeasures(fit)["rmsea"])
    barplot(rmseas, main = "RMSEA Comparison", ylab = "RMSEA",
            col = ifelse(rmseas < 0.08, "lightgreen",
                         ifelse(rmseas < 0.10, "yellow", "lightcoral")),
            names.arg = names(rmseas))
    abline(h = 0.08, col = "red", lty = 2)
    abline(h = 0.06, col = "green", lty = 2)

    # Plot 3: AIC comparison
    aics <- sapply(model_list, AIC)
    barplot(aics, main = "AIC Comparison", ylab = "AIC",
            col = "lightblue", names.arg = names(aics))

    # Plot 4: BIC comparison
    bics <- sapply(model_list, BIC)
    barplot(bics, main = "BIC Comparison", ylab = "BIC",
            col = "lightblue", names.arg = names(bics))

    dev.off()
    cat("✓ Alternative plot saved as 'three_model_comparison.png'\n")
  })
} else {
  cat("⚠ Not enough models for visualization\n")
}

# ----------------------------------------------------------------------------
# 10. Save Results
# ----------------------------------------------------------------------------
cat("\n10. SAVING RESULTS\n")
cat(rep("-", 50), "\n", sep = "")

# Save all fits and comparisons
save_list <- ls()[sapply(ls(), function(x) inherits(get(x), "lavaan") ||
                           is.list(get(x)) && !is.null(get(x)$fit_comparison))]

if (length(save_list) > 0) {
  save(list = save_list, file = "three_model_comparison_results.RData")
  cat(sprintf("✓ Saved %d objects to 'three_model_comparison_results.RData'\n",
              length(save_list)))
}

# Save fit summary table
if (exists("fit_summary")) {
  write.csv(fit_summary, "three_model_fit_summary.csv", row.names = FALSE)
  cat("✓ Fit summary saved as 'three_model_fit_summary.csv'\n")
}

# Save workspace image
save.image("three_model_analysis_workspace.RData")
cat("✓ Complete workspace saved as 'three_model_analysis_workspace.RData'\n")

# ----------------------------------------------------------------------------
# 11. Final Report
# ----------------------------------------------------------------------------
cat("\n" , rep("=", 60), "\n", sep = "")
cat("THREE-MODEL COMPARISON ANALYSIS COMPLETE\n")
cat(rep("=", 60), "\n")

cat("\n📊 SUMMARY OF FINDINGS:\n")

if (exists("fit_summary")) {
  # Find best model by AIC
  best_by_aic <- fit_summary$Model[which.min(fit_summary$AIC)]
  best_aic_value <- min(fit_summary$AIC)

  # Find best model by CFI
  best_by_cfi <- fit_summary$Model[which.max(fit_summary$CFI)]
  best_cfi_value <- max(fit_summary$CFI)

  cat(sprintf("1. Best model by AIC: %s (AIC = %.1f)\n", best_by_aic, best_aic_value))
  cat(sprintf("2. Best model by CFI: %s (CFI = %.3f)\n", best_by_cfi, best_cfi_value))

  # Check if criteria agree
  if (best_by_aic == best_by_cfi) {
    cat("3. All criteria agree on the best model ✓\n")
  } else {
    cat("3. Different criteria suggest different models ⚠\n")
    cat("   Consider theoretical justification for model selection\n")
  }

  # Model fit evaluation
  cat("\n📈 MODEL FIT EVALUATION:\n")
  for (i in 1:nrow(fit_summary)) {
    model <- fit_summary$Model[i]
    cfi <- fit_summary$CFI[i]
    rmsea <- fit_summary$RMSEA[i]

    cfi_eval <- if (cfi >= 0.95) "Excellent" else if (cfi >= 0.90) "Acceptable" else "Poor"
    rmsea_eval <- if (rmsea < 0.06) "Excellent" else if (rmsea < 0.08) "Acceptable" else if (rmsea < 0.10) "Marginal" else "Poor"

    cat(sprintf("  %s: CFI=%.3f (%s), RMSEA=%.3f (%s)\n",
                model, cfi, cfi_eval, rmsea, rmsea_eval))
  }
}

cat("\n📁 OUTPUT FILES CREATED:\n")
cat("  1. three_model_comparison_results.RData (R objects)\n")
cat("  2. three_model_fit_summary.csv (fit statistics)\n")
cat("  3. three_model_analysis_workspace.RData (complete workspace)\n")
cat("  4. Three-model comparison plot (PNG file)\n")

cat("\n" , rep("=", 60), "\n", sep = "")
cat("RECOMMENDATIONS FOR RESEARCH REPORTING:\n")
cat(rep("=", 60), "\n")

cat("\n1. REPORT IN YOUR PAPER:\n")
cat("   • Include the fit summary table\n")
cat("   • Report AIC, BIC, CFI, RMSEA, and SRMR for each model\n")
cat("   • State which model was selected and why\n")
cat("   • Include model comparison plot if space allows\n")

cat("\n2. INTERPRETATION GUIDELINES:\n")
cat("   • ΔAIC > 10: Very strong evidence for better model\n")
cat("   • ΔAIC 4-10: Strong evidence\n")
cat("   • ΔAIC 0-4: Weak evidence\n")
cat("   • Consider parsimony (simpler models preferred)\n")

cat("\n3. LIMITATIONS TO NOTE:\n")
cat("   • All models are non-nested (cannot use χ² difference test)\n")
cat("   • Bootstrap confidence intervals provide stronger evidence\n")
cat("   • Consider cross-validation with independent sample\n")

cat("\n" , rep("=", 60), "\n", sep = "")
cat("ANALYSIS COMPLETE - READY FOR PUBLICATION\n")
cat(rep("=", 60), "\n")
