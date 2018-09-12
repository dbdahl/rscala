# DBD 2018-09-12: These are hacks to allow current CRAN versions of bamboo,
# sdols, and shallot to pass CRAN checks without being updated.  Working
# versions of sdols and shallot without this hack are already in git.  When
# these packages are updated, this hack can be dropped.

#' @export
#' 
`[[.rscalaReference` <- function(x, condition) {
  if ( condition == "type" ) scalaType(x)
  else stop("Not supported.")
}

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
#' @param packages A character vector of package names whose JARs should be included in the classpath.
#' @param assign.callback A function.
#' @param mode A string.  If the package is to have its own bridge, this should
#'   be \code{""}.  If the package is to use the bridge of another package
#'   (e.g., package "foo"), then this should be the fully qualified variable of
#'   that package (e.g., \code{"foo:::s"}).
#'
#' @return Returns \code{NULL}, invisibly.
#'
#' @export
#'
scalaPackage <- function(packages, assign.callback, mode="") {
  env <- parent.env(parent.frame())
  if ( mode == "" ) {
    s <- scala()
  } else {
    assign(".rscalaBorrowed",TRUE,envir=env)
    s <- eval(parse(text=mode))
  }
  scalaJARs(packages)
  scalaLazy(assign.callback)
  assign("s",s,envir=env)
}

#' Clean up an Embedded Scala Bridge of a Package
#'
#' This function should be called in the \code{.onUnload} function of a
#' depending package if, in the corresponding function
#' \code{\link{scalaPackage}}, the argument \code{mode=""} was used.  It has no
#' effect otherwise.
#'
#' @return Returns \code{NULL}, invisibly.
#'
#' @export
#'
scalaPackageUnload <- function() {
  if ( exists(".rscalaBorrowed") ) {
    s <- get("s",envir=parent.env(parent.frame()))
    close(s)
  }
}

