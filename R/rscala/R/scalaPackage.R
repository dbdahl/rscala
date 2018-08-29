#' Embed a Scala Bridge in a Package
#'
#' This function should be called in the \code{\link{.onLoad}} function of a
#' depending package.
#'
#' The function makes an rscala bridge available to the functions of the
#' package. Package developers should call this function in the package's
#' \code{\link{.onLoad}} function. The \code{\link{scalaPackageUnload}} function
#' should be called in the package's \code{\link{.onUnload}} function.
#'
#' @inheritParams scala
#' @param mode A string.  If the package is to have its own bridge, this should
#'   be \code{""}.  If the package is to use the bridge of another package
#'   (e.g., package "foo"), then this should be the fully qualified variable of
#'   that package (e.g., \code{"foo:::s"}).
#' @param ... When using \code{mode=""}, other arguments passed to the function
#'   \code{\link{scala}}.
#'
#' @return Returns \code{NULL}, invisibly.
#'
#' @seealso \code{\link{scalaPackageUnload}}, \code{\link{scala}}
#' @export
#'
#' @examples \dontrun{
#' 
#' .onLoad <- function(libname, pkgname) {
#'   scalaPackage(c(pkgname, "commonsMath"), function(s) s + "
#'     import org.apache.commons.math3._
#'   ")
#' }
#' }
#' 
scalaPackage <- function(packages=character(),
                         assign.callback=function(s) {},
                         assign.name="s",
                         JARs=character(),
                         mode="",...) {
  pkgEnv <- parent.env(parent.frame())    # Environment of depending package (assuming this function is only called in .onLoad function).
  if ( is.null(assign.name) || ( assign.name == "" ) ) stop("'assign.name' must be supplied.")
  assign.callback2 <- function(s) {
    assign.callback(s)
    scalaPackageSuspend(s)
  }
  if ( mode == "" ) {
    assign("rscalaBridgeOwner",TRUE,envir=pkgEnv)
    assign("rscalaBridgeName",assign.name,envir=pkgEnv)
    scala(packages,assign.callback2,assign.name,JARs,assign.env=pkgEnv,...)
  }
  else {
    assign("rscalaBridgeOwner",FALSE,envir=pkgEnv)
    delayedAssign(assign.name,{
      s <- eval(parse(text=mode))
      majorVersion <- attr(s,"details")[["config"]]$scalaMajorVersion
      JARs <- c(JARs,unlist(lapply(packages, function(p) jarsOfPackage(p, majorVersion))))
      scalaAddJARs(JARs, s)
      assign.callback2(s)
      details <- attr(s,"details")
      transcompileHeader <- c(get("transcompileHeader",envir=details), unlist(lapply(packages,transcompileHeaderOfPackage)))
      assign("transcompileHeader",transcompileHeader,envir=details)
      transcompileSubstitute <- unlist(c(get("transcompileSubstitute",envir=details),lapply(packages,transcompileSubstituteOfPackage)))
      assign("transcompileSubstitute",transcompileSubstitute,envir=details)
      s
    },assign.env=pkgEnv)
  }
  invisible()
}
