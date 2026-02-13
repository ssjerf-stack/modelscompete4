#' modelscompete4: Advanced Model Comparison for Latent Variable Models
#'
#' The modelscompete4 package provides advanced tools for comparing latent
#' variable models, including bootstrap comparisons, fit index extraction,
#' and visualization functions.
#'
#' @section Main functions:
#' \describe{
#'   \item{\code{\link{compare_latent_models}}}{Compare multiple latent variable models}
#'   \item{\code{\link{extract_latent_parameters}}}{Extract parameters from lavaan models}
#'   \item{\code{\link{plot_latent_comparison}}}{Visualize model comparison results}
#'   \item{\code{\link{bootstrap_lavaan_comparison}}}{Bootstrap-based model comparison}
#' }
#'
#' @keywords internal
'_PACKAGE'

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
#' @importFrom lavaan fitMeasures parameterEstimates standardizedSolution lavTestLRT cfa sem
#' @importFrom boot boot boot.ci
#' @importFrom stats AIC BIC logLik residuals update
#' @importFrom utils head tail str
## usethis namespace: end
NULL
