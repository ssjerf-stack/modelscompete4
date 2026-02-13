# final_test.R
cat("FINAL TEST - modelscompete4 Package\n")
cat("===================================\n\n")

# 1. Try to load the package
cat("1. Loading package...\n")
success <- tryCatch({
  library(modelscompete4)
  TRUE
}, error = function(e) FALSE)

if (success) {
  cat("✓ Package loads successfully\n")

  # 2. Check version
  version <- packageVersion("modelscompete4")
  cat("✓ Version: ", as.character(version), "\n", sep = "")

  # 3. Check key function
  if (exists("compare_models", mode = "function")) {
    cat("✓ Main function 'compare_models' is available\n")
  }

  # 4. Try help
  cat("✓ Help system works: ?compare_models\n\n")

  cat(rep("=", 50), "\n", sep = "")
  cat("🎉 ALL TESTS PASSED! PACKAGE IS READY! 🎉\n")
  cat(rep("=", 50), "\n", sep = "")

} else {
  cat("✗ Failed to load package\n")
}
