#' @title Compare Multiple Nested or Non-Nested Structural Equation Models
#'
#' @description This is the core function of the modelscompete4
#'   package. It automatically fits a list of SEM models, determines their
#'   nesting relationship, and performs the appropriate statistical
#'   comparison (chi-square difference test for nested models, Vuong test for
#'   non-nested models).
#'
#' @param model_list A named list. Each element is a character string
#'   specifying the model syntax in lavaan format.
#' @param data A data.frame containing the observed variables used in
#'   the models.
#' @param estimator The estimator to be used (e.g., "ML"). Passed to
#'   \code{\link[lavaan]{sem}}.
#' @param se Type of standard errors. Default is "standard". Use
#'   "bootstrap" for bootstrapped standard errors and confidence intervals.
#' @param bootstrap Number of bootstrap draws if
#'   \code{se="bootstrap"}. Default is 1000.
#' @param parallel Method for parallel processing for bootstrapping
#'   ("multicore", "snow", or "no"). Recommended for large samples.
#' @param ... Additional arguments passed to
#'   \code{\link[lavaan]{sem}}.
#'
#' @return An object of class \code{modelscompete4}. This is a list
#'   containing:
#'   \itemize{
#'     \item \code{fit_list}: The list of fitted lavaan objects.
#'     \item \code{fit_table}: A data.frame of key fit indices for all
#'       models.
#'     \item \code{comparison_matrix}: A matrix showing pairwise nesting
#'       relationships and test results.
#'     \item \code{test_results}: Detailed results of the statistical
#'       tests performed.
#'     \item \code{bootstrap_summary}: Summary of bootstrapped results if
#'       requested.
#'   }
#' @importFrom lavaan sem lavInspect fitMeasures lavTestLRT parameterEstimates
#' @importFrom nonnest2 vuongtest
#' @importFrom stats quantile sd
#' @export
compare_models <- function(model_list,
                           data,
                           estimator = "ML",
                           se = "standard",
                           bootstrap = 1000,
                           parallel = "no",
                           ...) {

  # 1. Input Validation
  if (!is.list(model_list) || is.null(names(model_list))) {
    stop("'model_list' must be a NAMED list of model syntax strings.")
  }
  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame.")
  }

  cat("modelscompete4: Fitting and comparing", length(model_list), "models...\n")
  cat("===========================================\n")

  # 2. Fit All Models
  fit_list <- list()
  for (model_name in names(model_list)) {
    cat("  Fitting:", model_name, "\n")
    tryCatch({
      if (se == "bootstrap") {
        fit <- lavaan::sem(model_list[[model_name]],
                           data = data,
                           estimator = estimator,
                           se = se,
                           do.fit = TRUE,
                           bootstrap = bootstrap,
                           parallel = parallel,
                           ...)
      } else {
        fit <- lavaan::sem(model_list[[model_name]],
                           data = data,
                           estimator = estimator,
                           se = se,
                           do.fit = TRUE,
                           ...)
      }
      fit_list[[model_name]] <- fit
    }, error = function(e) {
      warning("Model '", model_name, "' failed to fit: ", e$message)
      fit_list[[model_name]] <<- NULL
    })
  }

  # Remove models that failed to fit
  fit_list <- fit_list[!sapply(fit_list, is.null)]
  if (length(fit_list) < 2) {
    stop("At least 2 models must be successfully fitted to perform comparisons.")
  }
  cat("  [OK] All models fitted successfully.\n")

  # 3. Create Unified Fit Table
  cat("\n  Compiling fit indices...\n")
  fit_indices <- c("chisq", "df", "pvalue", "cfi", "tli",
                   "rmsea", "srmr", "aic", "bic")
  fit_table <- data.frame(Model = names(fit_list))
  for (index in fit_indices) {
    fit_table[[toupper(index)]] <- sapply(fit_list, function(x) {
      if (index %in% c("chisq", "df", "aic", "bic")) {
        round(fitMeasures(x)[index], 1)
      } else if (index == "pvalue") {
        pval <- fitMeasures(x)[index]
        if (is.na(pval) || pval < 0.001) "<.001"
        else round(pval, 3)
      } else {
        round(fitMeasures(x)[index], 3)
      }
    })
  }

  # 4. Determine Nesting & Perform Correct Tests
  cat("  Determining model relationships and performing tests...\n")
  model_names <- names(fit_list)
  n_models <- length(model_names)
  comparison_matrix <- matrix(NA, nrow = n_models, ncol = n_models,
                              dimnames = list(model_names, model_names))
  test_results_list <- list()

  # Function to check if models are nested
  are_models_nested <- function(fit1, fit2) {
    par1 <- parameterEstimates(fit1)
    par2 <- parameterEstimates(fit2)
    params1 <- paste(par1$lhs, par1$op, par1$rhs)
    params2 <- paste(par2$lhs, par2$op, par2$rhs)
    is_subset_1_in_2 <- all(params1 %in% params2)
    is_subset_2_in_1 <- all(params2 %in% params1)

    if (is_subset_1_in_2 && !is_subset_2_in_1) return(1)  # fit1 nested in fit2
    if (is_subset_2_in_1 && !is_subset_1_in_2) return(2)  # fit2 nested in fit1
    if (is_subset_1_in_2 && is_subset_2_in_1) return(0)    # equivalent models
    return(-1)  # non-nested
  }

  for (i in 1:(n_models - 1)) {
    for (j in (i + 1):n_models) {
      fit_i <- fit_list[[i]]
      fit_j <- fit_list[[j]]
      name_i <- model_names[i]
      name_j <- model_names[j]

      nesting <- are_models_nested(fit_i, fit_j)

      if (nesting == 0) {
        # Equivalent models
        comparison_matrix[i, j] <- comparison_matrix[j, i] <- "Equivalent models"
        test_results_list[[paste(name_i, "vs", name_j)]] <-
          list(test = "Equivalent models",
               conclusion = "Models are equivalent")

      } else if (nesting %in% c(1, 2)) {
        # Nested models - use chi-square difference test
        tryCatch({
          lrt <- lavTestLRT(fit_i, fit_j)
          chi_diff <- lrt$`Chisq diff`[2]
          df_diff <- lrt$`Df diff`[2]
          p_val <- lrt$`Pr(>Chisq)`[2]

          conclusion <- ifelse(p_val < 0.05,
                               "Significant difference (nested models)",
                               "No significant difference (nested models)")

          test_results_list[[paste(name_i, "vs", name_j)]] <-
            list(test = "Chi-square difference",
                 statistic = round(chi_diff, 3),
                 df = df_diff,
                 p.value = ifelse(p_val < 0.001, "<.001", round(p_val, 4)),
                 conclusion = conclusion)

          comparison_matrix[i, j] <- comparison_matrix[j, i] <-
            paste0("Chi2_diff(", df_diff, ") = ", round(chi_diff, 2),
                   ", p ", ifelse(p_val < 0.001, "<.001", paste("=", round(p_val, 4))))

        }, error = function(e) {
          comparison_matrix[i, j] <- comparison_matrix[j, i] <- "LRT test failed"
        })

      } else {
        # Non-nested models - use Vuong test
        tryCatch({
          vuong_res <- nonnest2::vuongtest(fit_i, fit_j)
          test_stat <- vuong_res$LRTstat

          # Choose the correct one-sided p-value based on sign of test statistic
          if (test_stat > 0) {
            p_val <- vuong_res$p_LRT$A   # model i is better
          } else if (test_stat < 0) {
            p_val <- vuong_res$p_LRT$B   # model j is better
          } else {
            p_val <- NA                   # equal fit (rare)
          }

          # Fallback for older nonnest2 versions
          if (is.null(p_val) && !is.null(vuong_res$p_value)) {
            p_val <- vuong_res$p_value
          }

          test_stat <- round(test_stat, 3)

          conclusion <- ifelse(!is.na(p_val) && p_val < 0.05,
                               "Significant difference (non-nested models)",
                               "No significant difference (non-nested models)")

          test_results_list[[paste(name_i, "vs", name_j)]] <-
            list(test = "Vuong Z",
                 statistic = test_stat,
                 p.value = ifelse(is.na(p_val), NA,
                                  ifelse(p_val < 0.001, "<.001", round(p_val, 4))),
                 conclusion = conclusion)

          comparison_matrix[i, j] <- comparison_matrix[j, i] <-
            paste0("Vuong Z = ", test_stat,
                   ", p ", ifelse(is.na(p_val), "NA",
                                  ifelse(p_val < 0.001, "<.001", paste("=", round(p_val, 4)))))

        }, error = function(e) {
          # If Vuong test fails, use AIC/BIC comparison
          aic_i <- fitMeasures(fit_i, "aic")
          aic_j <- fitMeasures(fit_j, "aic")
          better <- ifelse(aic_i < aic_j, name_i, name_j)

          test_results_list[[paste(name_i, "vs", name_j)]] <-
            list(test = "AIC comparison",
                 aic_i = aic_i,
                 aic_j = aic_j,
                 conclusion = paste(better, "has better AIC"))

          comparison_matrix[i, j] <- comparison_matrix[j, i] <-
            paste0("Non-nested; ", better, " has better AIC")
        })
      }
    }
  }

  # 5. Bootstrap Summaries (if requested)
  if (se == "bootstrap") {
    cat("  Extracting bootstrap summaries...\n")
    boot_summaries <- lapply(fit_list, function(fit) {
      boot_obj <- lavInspect(fit, "boot")
      if (is.null(boot_obj)) {
        return(list(note = "No bootstrap results available"))
      }
      boot_coefs <- tryCatch({
        if (is.list(boot_obj) && "coef" %in% names(boot_obj)) {
          boot_obj$coef
        } else if (is.matrix(boot_obj)) {
          boot_obj
        } else {
          as.matrix(boot_obj)
        }
      }, error = function(e) {
        return(NULL)
      })
      if (is.null(boot_coefs)) {
        return(list(note = "Could not extract bootstrap coefficients"))
      }
      if (!is.matrix(boot_coefs)) {
        boot_coefs <- as.matrix(boot_coefs)
      }
      summ <- list(
        n_boot = nrow(boot_coefs),
        means = apply(boot_coefs, 2, mean, na.rm = TRUE),
        sds = apply(boot_coefs, 2, sd, na.rm = TRUE),
        ci_lower = apply(boot_coefs, 2, quantile, 0.025, na.rm = TRUE),
        ci_upper = apply(boot_coefs, 2, quantile, 0.975, na.rm = TRUE)
      )
      return(summ)
    })
    names(boot_summaries) <- names(fit_list)
  } else {
    boot_summaries <- NULL
  }

  # 6. Prepare and Return Results Object
  result_object <- list(
    fit_list = fit_list,
    fit_table = fit_table,
    comparison_matrix = comparison_matrix,
    test_results = test_results_list,
    bootstrap_summary = boot_summaries,
    call = match.call(),
    timestamp = Sys.time()
  )
  class(result_object) <- "modelscompete4"

  cat("\n===========================================\n")
  cat("modelscompete4: Comparison complete.\n")
  return(invisible(result_object))
}

#' @export
print.modelscompete4 <- function(x, ...) {
  cat("modelscompete4 Comparison Results\n")
  cat("=================================\n\n")
  cat("Models compared:", length(x$fit_list), "\n\n")
  cat("Fit Indices:\n")
  print(x$fit_table)
  cat("\n")
  cat("Pairwise Comparisons:\n")
  print(x$comparison_matrix)
  if (!is.null(x$test_results) && length(x$test_results) > 0) {
    cat("\nDetailed Test Results:\n")
    for (test_name in names(x$test_results)) {
      cat("\n", test_name, ":\n", sep = "")
      print(x$test_results[[test_name]])
    }
  }
  invisible(x)
}
