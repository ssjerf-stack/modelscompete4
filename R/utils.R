# Utility functions for modelscompete4 package

#' Format p-values for display
#'
#' This internal function formats p-values for display, converting very
#' small values (e.g., < 0.001) to appropriate string representations.
#'
#' @param pvals Vector of p-values
#' @param digits Number of digits (default=3)
#' @return Formatted p-values as character strings
#' @keywords internal
#' @examples
#' \dontrun{
#' format_pvalues(c(0.00001, 0.0345, 0.5, NA))
#' }
format_pvalues <- function(pvals, digits = 3) {
  sapply(pvals, function(p) {
    if (is.na(p) || is.null(p)) return(NA)
    if (p < 10^(-digits)) {
      return(paste0("<0.", paste(rep("0", digits-1), collapse=""), "1"))
    }
    return(format(round(p, digits), nsmall = digits))
  })
}

#' Format confidence intervals
#'
#' This function formats confidence intervals into a readable string.
#'
#' @param lower Lower bound of CI
#' @param upper Upper bound of CI
#' @param digits Number of decimal places (default=3)
#' @return Formatted CI string
#' @keywords internal
format_ci <- function(lower, upper, digits = 3) {
  if (is.na(lower) || is.na(upper)) return(NA)
  paste0("[", format(round(lower, digits), nsmall = digits),
         ", ", format(round(upper, digits), nsmall = digits), "]")
}
