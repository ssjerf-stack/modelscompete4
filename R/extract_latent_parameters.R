#' Extract Parameters from Lavaan Models
#'
#' This function extracts parameters (loadings, variances, etc.) from a lavaan model.
#' P-values are formatted appropriately (e.g., <0.001 for very small values).
#'
#' @param model A fitted lavaan model.
#' @param type Type of parameters to extract: "loadings", "variances", or "all". Default is "loadings".
#' @param standardized Logical; if TRUE, returns standardized estimates. Default is FALSE.
#' @param digits Number of decimal places for p-value formatting (default=3)
#' @param ... Additional arguments passed to \code{lavaan::parameterEstimates} or \code{lavaan::standardizedSolution}.
#'
#' @return A data frame containing the extracted parameters with formatted p-values.
#' @export
#'
#' @examples
#' \dontrun{
#' library(lavaan)
#' model <- 'F1 =~ x1 + x2 + x3'
#' fit <- cfa(model, data = HolzingerSwineford1939)
#' extract_latent_parameters(fit, type = "loadings")
#' }
extract_latent_parameters <- function(model, type = "loadings",
                                      standardized = FALSE, digits = 3, ...) {

  if (!inherits(model, "lavaan")) {
    stop("Model must be a lavaan object")
  }

  if (!type %in% c("loadings", "variances", "all")) {
    stop("type must be one of: 'loadings', 'variances', 'all'")
  }

  # Internal helper function for p-value formatting
  .format_pvalues <- function(pvals, digits = 3) {
    sapply(pvals, function(p) {
      if (is.na(p) || is.null(p)) return(NA)
      if (p < 10^(-digits)) {
        return(paste0("<0.", paste(rep("0", digits-1), collapse=""), "1"))
      }
      return(format(round(p, digits), nsmall = digits))
    })
  }

  # Get parameters based on standardization request
  if (standardized) {
    params <- tryCatch({
      lavaan::standardizedSolution(model, ...)
    }, error = function(e) {
      warning("Could not get standardized solution: ", e$message)
      # Fall back to parameterEstimates
      lavaan::parameterEstimates(model, ...)
    })

    # Check what columns we have and ensure we have what we need
    col_names <- colnames(params)

    # Determine which column to use as the estimate
    if ("std.all" %in% col_names) {
      # Create an 'est' column from std.all for backward compatibility
      params$est <- params$std.all
    } else if ("est.std" %in% col_names) {
      # Some lavaan versions use est.std
      params$est <- params$est.std
      if (!"std.all" %in% col_names) {
        params$std.all <- params$est.std
      }
    } else if ("est" %in% col_names) {
      # Already has est, check if we need std.all
      if (!"std.all" %in% col_names) {
        params$std.all <- params$est
      }
    } else {
      # No estimate column found
      warning("Standardized solution does not contain expected columns. Using 'est' from parameterEstimates.")
      params <- lavaan::parameterEstimates(model, ...)
      params$std.all <- params$est
    }

  } else {
    # Unstandardized parameters
    params <- lavaan::parameterEstimates(model, ...)
  }

  # Format p-values if they exist
  if ("pvalue" %in% colnames(params)) {
    params$pvalue <- .format_pvalues(params$pvalue, digits = digits)
  }

  # Extract based on type
  if (type == "loadings") {
    result <- params[params$op == "=~", ]
  } else if (type == "variances") {
    # Variances are covariances with same lhs and rhs
    result <- params[params$op == "~~" & params$lhs == params$rhs, ]
  } else if (type == "all") {
    result <- params
  }

  # Ensure we have at least an 'est' column
  if (!"est" %in% colnames(result) && "std.all" %in% colnames(result)) {
    result$est <- result$std.all
  }

  # Reorder columns for better readability
  preferred_order <- c("lhs", "op", "rhs", "est", "se", "z", "pvalue",
                       "ci.lower", "ci.upper", "std.all")
  existing_cols <- colnames(result)
  ordered_cols <- c()

  for (col in preferred_order) {
    if (col %in% existing_cols) {
      ordered_cols <- c(ordered_cols, col)
      existing_cols <- setdiff(existing_cols, col)
    }
  }

  # Add any remaining columns
  if (length(existing_cols) > 0) {
    ordered_cols <- c(ordered_cols, existing_cols)
  }

  result <- result[, ordered_cols]

  # Reset row names
  rownames(result) <- NULL

  return(result)
}
