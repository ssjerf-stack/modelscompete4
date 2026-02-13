# Install and use usethis if not installed
if (!requireNamespace("usethis", quietly = TRUE)) {
  install.packages("usethis")
}
library(usethis)

# Navigate to package directory
setwd("D:/R_Workspace/Pack4/modelscompete4")

# Create the .Rproj file
usethis::use_rstudio()

cat("✓ RStudio project file created!\n")
cat("✓ You can now open modelscompete4.Rproj in RStudio\n")
