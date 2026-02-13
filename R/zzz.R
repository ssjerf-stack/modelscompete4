#' Package startup message
#'
#' @param libname library location
#' @param pkgname package name
#' @importFrom utils packageVersion
.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste0(
    "Welcome to modelscompete4 v", packageVersion("modelscompete4"), "\n",
    "A package for comparing nested and non-nested SEM models\n",
    "Type ?compare_models for help"
  ))
}

#' Package load
#'
#' @param libname library location
#' @param pkgname package name
.onLoad <- function(libname, pkgname) {
  # No special initialization needed
  invisible()
}
