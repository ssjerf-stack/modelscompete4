# COMPREHENSIVE EXAMPLE: modelscompete4 with Latent Variables

library(modelscompete4)
library(lavaan)
library(semTools)

# Example 1: Confirmatory Factor Analysis (CFA)
cfa_model1 <- '
  # Latent variables
  f1 =~ x1 + x2 + x3
  f2 =~ x4 + x5 + x6
  f3 =~ x7 + x8 + x9
'

cfa_model2 <- '
  # Alternative CFA structure
  f1 =~ x1 + x2 + x3 + x4
  f2 =~ x5 + x6 + x7
  f3 =~ x8 + x9
'

# Fit models
data(HolzingerSwineford1939)
fit1 <- cfa(cfa_model1, data = HolzingerSwineford1939)
fit2 <- cfa(cfa_model2, data = HolzingerSwineford1939)

# Compare latent variable models
results <- compare_models_advanced_lv(fit1, fit2,
                                      criteria = c("AIC", "BIC", "CFI", "TLI",
                                                   "RMSEA", "SRMR"),
                                      bootstrap = TRUE,
                                      n_bootstrap = 500)

print(results)

# Example 2: Structural Equation Model (SEM)
sem_model1 <- '
  # Measurement model
  visual =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
  speed =~ x7 + x8 + x9

  # Structural model
  visual ~ textual + speed
'

sem_model2 <- '
  # Alternative SEM
  visual =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
  speed =~ x7 + x8 + x9

  # Different structural relationships
  visual ~ textual
  speed ~ visual + textual
'

fit_sem1 <- sem(sem_model1, data = HolzingerSwineford1939)
fit_sem2 <- sem(sem_model2, data = HolzingerSwineford1939)

# Compare SEM models
sem_results <- compare_models_advanced_lv(fit_sem1, fit_sem2,
                                          bootstrap = TRUE)
print(sem_results)
