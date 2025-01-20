#' .onLoad
#'
#' @description
#'
#' This function is called when the package is loaded. It sets the default options for the package.
#'
#' @param libname libname
#' @param pkgname package
#'
#' @return invisible
#' @keywords internal
#'
.onLoad <- function(libname, pkgname) {
  op <- options()
  op.ordinalsimr <- list(
    ordinalsimr.default_iterations = 1000,
    ordinalsimr.default_size_min = 30,
    ordinalsimr.default_size_max = 200,
    ordinalsimr.default_ratio = "50:50"
  )

  toset <- !(names(op.ordinalsimr) %in% names(op))
  if (any(toset)) options(op.ordinalsimr[toset]) # nocov

  invisible()
}
