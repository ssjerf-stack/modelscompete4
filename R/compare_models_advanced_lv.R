#' Advanced Model Comparison with Latent Variable Support
#'
#' @param models List of model objects (lm, lavaan)
#' @param model_names Character vector of model names
#' @param model_types Character vector of model types ("lm", "lavaan")
#' @param criteria Criteria to calculate
#' @param latent_indicators List of latent variable indicators (for lavaan)
#' @param bootstrap Logical, whether to perform bootstrapping
#' @param n_bootstrap Number of bootstrap replications
#' @return Comparison results with latent variable support
#' @export
compare_models_advanced_lv <- function(models,
                                       model_names = NULL,
                                       model_types = NULL,
                                       criteria = c("AIC", "BIC", "CFI", "TLI", "RMSEA", "SRMR"),
                                       latent_indicators = NULL,
                                       bootstrap = FALSE,
                                       n_bootstrap = 1000) {

  if (is.null(models) || length(models) == 0) {
    stop("No models provided")
  }

  n_models <- length(models)

  # Set default model names if not provided
  if (is.null(model_names)) {
    model_names <- paste0("Model", 1:n_models)
  }

  # Determine model types if not provided
  if (is.null(model_types)) {
    model_types <- sapply(models, function(m) {
      if (inherits(m, "lavaan")) "lavaan" else "lm"
    })
  }

  # Initialize results list
  results <- list(
    models = models,
    model_names = model_names,
    model_types = model_types,
    latent_indicators = latent_indicators,
    comparisons = list()
  )

  # Extract fit indices for each model
  for (i in 1:n_models) {
    model <- models[[i]]
    type <- model_types[i]
    model_name <- model_names[i]

    if (type == "lavaan" && inherits(model, "lavaan")) {
      # Extract lavaan fit measures
      fits <- tryCatch({
        lavaan::fitMeasures(model)
      }, error = function(e) {
        warning("Could not extract fit measures for model ", model_name, ": ", e$message)
        numeric(0)
      })

      # Extract individual fit indices
      comparison <- list(
        type = "lavaan",
        AIC = tryCatch(stats::AIC(model), error = function(e) NA),
        BIC = tryCatch(stats::BIC(model), error = function(e) NA)
      )

      # Add other criteria if available
      if (length(fits) > 0) {
        if ("cfi" %in% names(fits)) comparison$CFI <- fits["cfi"]
        if ("tli" %in% names(fits)) comparison$TLI <- fits["tli"]
        if ("rmsea" %in% names(fits)) comparison$RMSEA <- fits["rmsea"]
        if ("srmr" %in% names(fits)) comparison$SRMR <- fits["srmr"]
        if ("logl" %in% names(fits)) comparison$logLik <- fits["logl"]
        if ("chisq" %in% names(fits)) comparison$chisq <- fits["chisq"]
        if ("df" %in% names(fits)) comparison$df <- fits["df"]
        if ("pvalue" %in% names(fits)) comparison$pvalue <- fits["pvalue"]
      }

      results$comparisons[[model_name]] <- comparison

    } else if (type == "lm" && inherits(model, "lm")) {
      # Extract lm fit indices
      results$comparisons[[model_name]] <- list(
        type = "lm",
        AIC = stats::AIC(model),
        BIC = stats::BIC(model),
        R2 = summary(model)$r.squared,
        AdjR2 = summary(model)$adj.r.squared,
        RMSE = sqrt(mean(stats::residuals(model)^2)),
        logLik = stats::logLik(model)
      )
    } else {
      warning("Model ", model_name, " is not a recognized type (lavaan or lm)")
      results$comparisons[[model_name]] <- list(
        type = "unknown",
        AIC = NA,
        BIC = NA
      )
    }
  }

  # Create a summary table
  summary_data <- data.frame(
    Model = model_names,
    Type = model_types,
    stringsAsFactors = FALSE
  )

  # Add criteria columns
  for (criterion in criteria) {
    values <- sapply(results$comparisons, function(x) {
      if (criterion %in% names(x)) x[[criterion]] else NA
    })
    summary_data[[criterion]] <- values
  }

  results$fit_indices <- summary_data

  # Perform latent variable specific comparisons if we have lavaan models
  lavaan_models <- models[model_types == "lavaan"]
  if (length(lavaan_models) > 0) {
    results$latent_comparison <- tryCatch({
      # Get names of lavaan models
      lavaan_names <- model_names[model_types == "lavaan"]

      # Check if we have at least 2 models for comparison
      if (length(lavaan_models) >= 2) {
        compare_latent_models(
          models = lavaan_models,
          model_names = lavaan_names,
          fit_measures = c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr", "aic", "bic"),
          nested = FALSE
        )
      } else {
        # For single model, just extract fit measures
        fits <- lavaan::fitMeasures(lavaan_models[[1]])
        list(
          fit_comparison = data.frame(
            chisq = fits["chisq"],
            df = fits["df"],
            pvalue = fits["pvalue"],
            cfi = fits["cfi"],
            tli = fits["tli"],
            rmsea = fits["rmsea"],
            srmr = fits["srmr"],
            aic = fits["aic"],
            bic = fits["bic"]
          ),
          n_models = 1,
          model_names = lavaan_names
        )
      }
    }, error = function(e) {
      warning("Latent model comparison failed: ", e$message)
      NULL
    })
  }

  # Bootstrap if requested and we have at least 2 lavaan models
  if (bootstrap && length(lavaan_models) >= 2) {
    results$bootstrap <- tryCatch({
      bootstrap_lavaan_comparison(
        model1 = lavaan_models[[1]],
        model2 = lavaan_models[[2]],
        R = n_bootstrap
      )
    }, error = function(e) {
      warning("Bootstrap comparison failed: ", e$message)
      NULL
    })
  } else if (bootstrap && length(lavaan_models) < 2) {
    warning("Bootstrap comparison requires at least 2 lavaan models")
  }

  class(results) <- c("modelscompete4_advanced", "list")
  return(results)
}
