#' Embed a Scala Bridge in a Package
#'
#' This function should be called in the \code{\link{.onLoad}} function of a
#' depending package.
#'
#' The function makes a Scala bridge available to the functions of the package.
#' Package developers should call this function in the package's
#' \code{\link{.onLoad}} function. The \code{\link{scalaPackageUnload}} function
#' should be called in the package's \code{\link{.onUnload}} function.
#'
#' @param packages See the function \code{\link{scala}}, but this will include
#'   the name of the package if it has its owns JAR file(s).
#' @param assign.callback See the function \code{\link{scala}}, but this is
#'   where setup code goes, like import statements.  For example, it might equal
#'   \code{function(s) { s + "import scala.util.Random" }}.  \strong{Note}
#'   the use of the execution operator \code{+} instead of the evaluation
#'   operator \code{*}.
#' @param assign.name See the function \code{\link{scala}}.  This will be the
#'   name of the bridge available to the package's functions.
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
                         ...) {
  pkgEnv <- parent.env(parent.frame())    # Environment of depending package (assuming this function is only called in .onLoad function).
  if ( is.null(assign.name) || ( assign.name == "" ) ) stop("'assign.name' must be supplied.")
  if ( mode == "" ) {
    assign("rscalaBridgeOwner",TRUE,envir=pkgEnv)
    assign("rscalaBridgeName",assign.name,envir=pkgEnv)
    scala(packages,assign.callback,assign.name,pkgEnv,...)
  }
  else {
    assign("rscalaBridgeOwner",FALSE,envir=pkgEnv)
    delayedAssign(assign.name,{
      s <- eval(parse(text=mode))
      assign.callback(s)
      s
    },assign.env=pkgEnv)
  }
  invisible()
}
