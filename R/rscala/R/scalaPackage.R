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
#' @param ... Other arguments passed to the function \code{\link{scala}}.  The
#'   \code{assign.env} option is \emph{not} allowed.  Many arguments are ignored
#'   if the package uses the bridge of another package.
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
                         mode="",
                         JARs="",...) {
  pkgEnv <- parent.env(parent.frame())    # Environment of depending package (assuming this function is only called in .onLoad function).
  if ( is.null(assign.name) || ( assign.name == "" ) ) stop("'assign.name' must be supplied.")
  if ( mode == "" ) {
    assign("rscalaBridgeOwner",TRUE,envir=pkgEnv)
    assign("rscalaBridgeName",assign.name,envir=pkgEnv)
    scala(packages=packages,assign.callback=assign.callback,assign.name=assign.name,assign.env=pkgEnv,...)
  }
  else {
    assign("rscalaBridgeOwner",FALSE,envir=pkgEnv)
    delayedAssign(assign.name,{
      s <- eval(parse(text=mode))
      JARs <- c(JARs,unlist(lapply(packages, function(p) jarsOfPackage(p, s[["details"]][["scalaMajor"]]))))
      assign.callback(s)
      s
    },assign.env=pkgEnv)
  }
  invisible()
}
