#' Extract Latent Variable Fit Indices
#'
#' @param model lavaan model object
#' @return Comprehensive fit indices for latent variable model
#' @export

extract_latent_fit <- function(model) {
  if (!inherits(model, "lavaan")) {
    stop("Model must be of class 'lavaan'")
  }

  fit <- fitMeasures(model)

  # Categorize fit indices
  results <- list(
    absolute_fit = c(
      chisq = fit["chisq"],
      df = fit["df"],
      pvalue = fit["pvalue"],
      rmsea = fit["rmsea"],
      rmsea.ci.lower = fit["rmsea.ci.lower"],
      rmsea.ci.upper = fit["rmsea.ci.upper"],
      srmr = fit["srmr"]
    ),
    incremental_fit = c(
      cfi = fit["cfi"],
      tli = fit["tli"],
      nfi = fit["nfi"],
      ifi = fit["ifi"],
      rni = fit["rni"]
    ),
    information_criteria = c(
      aic = AIC(model),
      bic = BIC(model),
      abic = fit["abic"]
    ),
    other_indices = c(
      npar = fit["npar"],
      logl = fit["logl"],
      unrestricted.logl = fit["unrestricted.logl"]
    )
  )

  # Add fit interpretation
  results$interpretation <- list(
    rmsea_interpretation = ifelse(fit["rmsea"] < 0.05, "Excellent fit",
                                  ifelse(fit["rmsea"] < 0.08, "Good fit",
                                         ifelse(fit["rmsea"] < 0.10, "Mediocre fit", "Poor fit"))),
    cfi_interpretation = ifelse(fit["cfi"] > 0.95, "Excellent fit",
                                ifelse(fit["cfi"] > 0.90, "Acceptable fit", "Poor fit")),
    srmr_interpretation = ifelse(fit["srmr"] < 0.08, "Good fit", "Poor fit")
  )

  return(results)
}
