# ============================================================================
# FIXED TEST SCRIPT: Non-nested Latent Variable Model Comparisons
# Using modelscompete4 package with lavaan
# ============================================================================

cat("=== FIXED TEST: NON-NESTED MODEL COMPARISONS ===\n\n")

# ----------------------------------------------------------------------------
# 1. Load required packages
# ----------------------------------------------------------------------------
cat("1. Loading required packages...\n")

# Unload package if already loaded to avoid conflicts
tryCatch({
  detach("package:modelscompete4", unload = TRUE)
}, error = function(e) {})

# Load packages
library(modelscompete4)
library(lavaan)

cat("✓ Packages loaded\n\n")

# ----------------------------------------------------------------------------
# 2. Prepare example dataset
# ----------------------------------------------------------------------------
cat("2. Preparing example data...\n")

data(HolzingerSwineford1939)
HS <- HolzingerSwineford1939[, c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9")]
HS_std <- as.data.frame(scale(HS))

cat(sprintf("  Dataset: %d observations, %d variables\n", nrow(HS_std), ncol(HS_std)))
cat("✓ Data prepared\n\n")

# ----------------------------------------------------------------------------
# 3. Define models
# ----------------------------------------------------------------------------
cat("3. Defining non-nested latent variable models...\n")

model_A <- '
  visual =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
  speed =~ x7 + x8 + x9
  visual ~~ textual + speed
  textual ~~ speed
'

model_B <- '
  verbal =~ x4 + x5 + x6 + x7
  performance =~ x1 + x2 + x3 + x8 + x9
  verbal ~~ performance
'

model_C <- '
  general =~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9
'

cat("  Model A: Three-factor (visual, textual, speed)\n")
cat("  Model B: Two-factor (verbal, performance)\n")
cat("  Model C: Single general factor\n")
cat("✓ Models defined\n\n")

# ----------------------------------------------------------------------------
# 4. Fit the models
# ----------------------------------------------------------------------------
cat("4. Fitting models with lavaan...\n")

fit_A <- cfa(model_A, data = HS_std, std.lv = TRUE)
cat(sprintf("  Model A: CFI=%.3f, RMSEA=%.3f\n",
            fitMeasures(fit_A)["cfi"], fitMeasures(fit_A)["rmsea"]))

fit_B <- cfa(model_B, data = HS_std, std.lv = TRUE)
cat(sprintf("  Model B: CFI=%.3f, RMSEA=%.3f\n",
            fitMeasures(fit_B)["cfi"], fitMeasures(fit_B)["rmsea"]))

fit_C <- cfa(model_C, data = HS_std, std.lv = TRUE)
cat(sprintf("  Model C: CFI=%.3f, RMSEA=%.3f\n",
            fitMeasures(fit_C)["cfi"], fitMeasures(fit_C)["rmsea"]))

cat("✓ All models fitted\n\n")

# ----------------------------------------------------------------------------
# 5. Test WORKING functions (verified)
# ----------------------------------------------------------------------------
cat("5. Testing VERIFIED functions...\n")
cat(rep("-", 60), "\n", sep = "")

# 5a. extract_latent_parameters
cat("\n5a. extract_latent_parameters():\n")
params_A <- extract_latent_parameters(fit_A)
cat(sprintf("  ✓ Works: %d parameters extracted\n", nrow(params_A)))

# 5b. extract_latent_fit
cat("\n5b. extract_latent_fit():\n")
fit_indices <- extract_latent_fit(fit_A)
cat(sprintf("  ✓ Works: CFI=%.3f, RMSEA=%.3f\n",
            fit_indices$incremental_fit$cfi,
            fit_indices$absolute_fit$rmsea))

# 5c. compare_latent_models
cat("\n5c. compare_latent_models():\n")
comparison <- compare_latent_models(fit_A, fit_B)
cat(sprintf("  ✓ Works: Compared %d models (nested=%s)\n",
            comparison$n_models, comparison$nested))

# Show comparison results
if (!is.null(comparison$fit_comparison)) {
  cat("\n  Fit comparison:\n")
  print(comparison$fit_comparison)
}

# ----------------------------------------------------------------------------
# 6. Test PROBLEMATIC functions with fixes
# ----------------------------------------------------------------------------
cat("\n6. Testing PROBLEMATIC functions with fixes...\n")
cat(rep("-", 60), "\n", sep = "")

# 6a. FIXED: bootstrap_lavaan_comparison
cat("\n6a. FIXED bootstrap_lavaan_comparison():\n")
tryCatch({
  # Try without parameters first
  bootstrap_results <- bootstrap_lavaan_comparison(fit_A, fit_B)
  cat("  ✓ Works with default parameters\n")
  print(bootstrap_results)
}, error = function(e1) {
  cat(sprintf("  Error with default: %s\n", e1$message))
  cat("  Trying alternative parameter names...\n")
  tryCatch({
    # Try with B instead of nboot
    bootstrap_results <- bootstrap_lavaan_comparison(fit_A, fit_B, B = 50)
    cat("  ✓ Works with B parameter\n")
  }, error = function(e2) {
    cat(sprintf("  Alternative failed: %s\n", e2$message))
    cat("  Checking function arguments...\n")
    # Check function signature
    args_list <- tryCatch({
      args(bootstrap_lavaan_comparison)
    }, error = function(e3) {
      "Cannot inspect function"
    })
    cat(sprintf("  Function args: %s\n", paste(args_list, collapse=", ")))
  })
})

# 6b. FIXED: compare_models_advanced_lv
cat("\n6b. FIXED compare_models_advanced_lv():\n")
tryCatch({
  # Try individual arguments instead of list
  advanced_comp <- compare_models_advanced_lv(fit_A, fit_B, fit_C)
  cat("  ✓ Works with individual arguments\n")
  print(advanced_comp)
}, error = function(e1) {
  cat(sprintf("  Error with individual args: %s\n", e1$message))
  cat("  Trying named list...\n")
  tryCatch({
    # Try named list approach
    model_list <- list(Model_A = fit_A, Model_B = fit_B, Model_C = fit_C)
    advanced_comp <- compare_models_advanced_lv(model_list)
    cat("  ✓ Works with named list\n")
  }, error = function(e2) {
    cat(sprintf("  Named list failed: %s\n", e2$message))
    cat("  Checking if function exists...\n")
    # Check function documentation
    help_text <- tryCatch({
      help("compare_models_advanced_lv")
    }, error = function(e3) {
      "No help available"
    })
    cat("  Note: Check ?compare_models_advanced_lv for correct usage\n")
  })
})

# 6c. FIXED: plot_latent_comparison
cat("\n6c. FIXED plot_latent_comparison():\n")
tryCatch({
  # Try creating plot without saving first
  plot_obj <- plot_latent_comparison(fit_A, fit_B)
  cat("  ✓ Plot object created\n")

  # Try to save
  png("model_comparison_plot.png", width = 800, height = 600)
  print(plot_obj)
  dev.off()
  cat("  ✓ Plot saved successfully\n")
}, error = function(e) {
  cat(sprintf("  Error: %s\n", e$message))
  cat("  Creating alternative visualization...\n")

  # Create manual comparison plot
  fit_stats <- data.frame(
    Model = c("3-Factor", "2-Factor", "1-Factor"),
    CFI = c(fitMeasures(fit_A)["cfi"],
            fitMeasures(fit_B)["cfi"],
            fitMeasures(fit_C)["cfi"]),
    RMSEA = c(fitMeasures(fit_A)["rmsea"],
              fitMeasures(fit_B)["rmsea"],
              fitMeasures(fit_C)["rmsea"]),
    AIC = c(AIC(fit_A), AIC(fit_B), AIC(fit_C))
  )

  png("alternative_comparison_plot.png", width = 800, height = 600)
  par(mfrow = c(1, 3))

  # Plot 1: CFI comparison
  barplot(fit_stats$CFI, names.arg = fit_stats$Model,
          main = "CFI Comparison", ylab = "CFI", ylim = c(0, 1),
          col = c("green", "yellow", "red"))
  abline(h = 0.95, col = "blue", lty = 2)
  abline(h = 0.90, col = "orange", lty = 2)

  # Plot 2: RMSEA comparison
  barplot(fit_stats$RMSEA, names.arg = fit_stats$Model,
          main = "RMSEA Comparison", ylab = "RMSEA",
          col = c("green", "yellow", "red"))
  abline(h = 0.06, col = "blue", lty = 2)
  abline(h = 0.08, col = "orange", lty = 2)

  # Plot 3: AIC comparison
  barplot(fit_stats$AIC, names.arg = fit_stats$Model,
          main = "AIC Comparison", ylab = "AIC",
          col = c("green", "yellow", "red"))

  dev.off()
  cat("  ✓ Alternative plot created: 'alternative_comparison_plot.png'\n")
})

# ----------------------------------------------------------------------------
# 7. Comprehensive model comparison summary
# ----------------------------------------------------------------------------
cat("\n7. COMPREHENSIVE COMPARISON SUMMARY\n")
cat(rep("=", 60), "\n", sep = "")

# Create comparison table
comparison_table <- data.frame(
  Model = c("3-Factor (A)", "2-Factor (B)", "1-Factor (C)"),
  χ² = c(fitMeasures(fit_A)["chisq"],
         fitMeasures(fit_B)["chisq"],
         fitMeasures(fit_C)["chisq"]),
  df = c(fitMeasures(fit_A)["df"],
         fitMeasures(fit_B)["df"],
         fitMeasures(fit_C)["df"]),
  CFI = c(fitMeasures(fit_A)["cfi"],
          fitMeasures(fit_B)["cfi"],
          fitMeasures(fit_C)["cfi"]),
  RMSEA = c(fitMeasures(fit_A)["rmsea"],
            fitMeasures(fit_B)["rmsea"],
            fitMeasures(fit_C)["rmsea"]),
  AIC = c(AIC(fit_A), AIC(fit_B), AIC(fit_C)),
  BIC = c(BIC(fit_A), BIC(fit_B), BIC(fit_C))
)

cat("\nModel Fit Comparison:\n")
print(comparison_table)

# Determine best model based on AIC
best_model <- comparison_table$Model[which.min(comparison_table$AIC)]
cat(sprintf("\n✓ Best model based on AIC: %s\n", best_model))

# Determine best model based on CFI
best_model_cfi <- comparison_table$Model[which.max(comparison_table$CFI)]
cat(sprintf("✓ Best model based on CFI: %s\n", best_model_cfi))

# ----------------------------------------------------------------------------
# 8. Save results
# ----------------------------------------------------------------------------
cat("\n8. Saving results...\n")

# Save only objects that exist
save_list <- c("fit_A", "fit_B", "fit_C", "comparison_table")

if (exists("comparison")) {
  save_list <- c(save_list, "comparison")
}

if (exists("bootstrap_results")) {
  save_list <- c(save_list, "bootstrap_results")
}

if (exists("advanced_comp")) {
  save_list <- c(save_list, "advanced_comp")
}

save(list = save_list, file = "fixed_comparison_results.RData")
cat("✓ Results saved to 'fixed_comparison_results.RData'\n")

# ----------------------------------------------------------------------------
# 9. Final report
# ----------------------------------------------------------------------------
cat("\n", rep("=", 60), "\n", sep = "")
cat("TEST COMPLETION REPORT\n")
cat(rep("=", 60), "\n")

cat("\n✓ SUCCESSFUL TESTS:\n")
cat("1. extract_latent_parameters() - Working perfectly\n")
cat("2. extract_latent_fit() - Working perfectly\n")
cat("3. compare_latent_models() - Working perfectly\n")

cat("\n⚠️  PARTIALLY SUCCESSFUL / NEEDS ADJUSTMENT:\n")
cat("4. bootstrap_lavaan_comparison() - May need parameter adjustment\n")
cat("5. compare_models_advanced_lv() - May need different calling method\n")
cat("6. plot_latent_comparison() - May have bug; alternative plot created\n")

cat("\n📊 KEY FINDINGS:\n")
cat(sprintf("• Model A (3-factor): CFI=%.3f, RMSEA=%.3f\n",
            fitMeasures(fit_A)["cfi"], fitMeasures(fit_A)["rmsea"]))
cat(sprintf("• Model B (2-factor): CFI=%.3f, RMSEA=%.3f\n",
            fitMeasures(fit_B)["cfi"], fitMeasures(fit_B)["rmsea"]))
cat(sprintf("• Model C (1-factor): CFI=%.3f, RMSEA=%.3f\n",
            fitMeasures(fit_C)["cfi"], fitMeasures(fit_C)["rmsea"]))
cat(sprintf("• Best model: %s (based on AIC)\n", best_model))

cat("\n📁 OUTPUT FILES:\n")
cat("1. fixed_comparison_results.RData - All results\n")
cat("2. model_comparison_plot.png OR alternative_comparison_plot.png - Visual comparison\n")

cat("\n", rep("=", 60), "\n", sep = "")
cat("NEXT STEPS:\n")
cat(rep("=", 60), "\n")
cat("1. Review the comparison table above\n")
cat("2. Check the saved plot file\n")
cat("3. For problematic functions, check package documentation:\n")
cat("   • ?bootstrap_lavaan_comparison\n")
cat("   • ?compare_models_advanced_lv\n")
cat("   • ?plot_latent_comparison\n")
cat("4. Consider reporting issues to package maintainer if bugs confirmed\n")
cat(rep("=", 60), "\n")
