#' Clean up an Embedded Scala Bridge of a Package
#'
#' This function should be called in the \code{.onUnload} function of a
#' depending package if, in the corresponding function
#' \code{\link{scalaPackage}}, the argument \code{mode=""} was used.  It has no
#' effect otherwise.
#'
#' @return Returns \code{NULL}, invisibly.
#' @seealso \code{\link{scalaPackage}}
#' @export
#' 
#' @examples \dontrun{
#' 
#' .onUnload <- function(libname, pkgname) {
#'   scalaPackageUnload()
#' }
#' }
#'
scalaPackageUnload <- function() {
  pkgEnv <- parent.env(parent.frame())    # Environment of depending package (assuming this function is only called in .onUnload function).
  if ( pkgEnv[["rscalaBridgeOwner"]] ) close(pkgEnv[[pkgEnv[["rscalaBridgeName"]]]])
  invisible()
}
