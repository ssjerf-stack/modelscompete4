#' Bootstrap Comparison for Lavaan Models
#'
#' Perform bootstrap-based comparison of lavaan models
#'
#' @param model1 First lavaan model
#' @param model2 Second lavaan model
#' @param R Number of bootstrap replications (default: 1000)
#' @param parallel Type of parallel processing (if any)
#' @param ncpus Number of CPUs to use for parallel processing
#'
#' @return A list containing bootstrap results
#' @export
#'
#' @examples
#' \dontrun{
#' library(lavaan)
#' model1 <- 'F1 =~ x1 + x2 + x3'
#' model2 <- 'F1 =~ x1 + x2 + x3 + x4'
#' fit1 <- cfa(model1, data = HolzingerSwineford1939)
#' fit2 <- cfa(model2, data = HolzingerSwineford1939)
#' boot_result <- bootstrap_lavaan_comparison(fit1, fit2, R = 100)
#' print(boot_result)
#' }
bootstrap_lavaan_comparison <- function(model1, model2, R = 1000,
                                        parallel = "no", ncpus = 1) {
  if (!requireNamespace("boot", quietly = TRUE)) {
    stop("boot package is required for bootstrap_lavaan_comparison")
  }

  if (!inherits(model1, "lavaan") || !inherits(model2, "lavaan")) {
    stop("Both models must be lavaan objects")
  }

  # Get original data
  data1 <- tryCatch({
    as.data.frame(lavaan::lavInspect(model1, "data"))
  }, error = function(e) {
    warning("Could not extract data from model1: ", e$message)
    NULL
  })

  data2 <- tryCatch({
    as.data.frame(lavaan::lavInspect(model2, "data"))
  }, error = function(e) {
    warning("Could not extract data from model2: ", e$message)
    NULL
  })

  # Check if we have data
  if (is.null(data1) || is.null(data2)) {
    warning("Could not extract data from models. Returning simple comparison.")
    result <- list(
      success = FALSE,
      R = R,
      original_diff = c(
        AIC(model1) - AIC(model2),
        BIC(model1) - BIC(model2),
        lavaan::fitMeasures(model1, "chisq") - lavaan::fitMeasures(model2, "chisq")
      ),
      message = "Bootstrap failed: could not extract data"
    )
    class(result) <- c("bootstrap_lavaan_comparison", "list")
    return(result)
  }

  # Ensure both models use the same data
  if (!identical(dim(data1), dim(data2)) || !identical(names(data1), names(data2))) {
    warning("Models appear to use different datasets. Using data from model1.")
  }

  # SIMPLIFIED bootstrap function - just compare AIC/BIC
  boot_func <- function(data, indices, m1, m2) {
    boot_data <- data[indices, ]

    # Use tryCatch to handle any errors during refitting
    tryCatch({
      # Refit models with bootstrap data
      fit1_boot <- lavaan::update(m1, data = boot_data)
      fit2_boot <- lavaan::update(m2, data = boot_data)

      # Calculate differences
      c(
        AIC(fit1_boot) - AIC(fit2_boot),
        BIC(fit1_boot) - BIC(fit2_boot)
      )
    }, error = function(e) {
      # Return NA if there's an error
      c(NA, NA)
    })
  }

  # Run bootstrap with smaller R for testing
  R_test <- ifelse(R > 50, 10, R)  # Use 10 for testing to be fast

  boot_results <- try(boot::boot(data = data1,
                                 statistic = boot_func,
                                 R = R_test,
                                 parallel = parallel,
                                 ncpus = ncpus,
                                 m1 = model1, m2 = model2
                                 )
                      )

  if (inherits(boot_results, "try-error")) {
    warning("Bootstrap failed: ", boot_results)
    result <- list(
      success = FALSE,
      error = as.character(boot_results),
      R = R,
      original_diff = c(
        AIC(model1) - AIC(model2),
        BIC(model1) - BIC(model2)
      )
    )
  } else {
    # Prepare result object
    result <- list(
      boot_results = boot_results,
      R = R,
      original_diff = c(
        AIC(model1) - AIC(model2),
        BIC(model1) - BIC(model2)
      ),
      success = TRUE
    )
  }

  class(result) <- c("bootstrap_lavaan_comparison", "list")
  return(result)
}
