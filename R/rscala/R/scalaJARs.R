#' Add JAR Files to Classpath
#' 
#' This function is no longer needed but remains until all packages based on
#' \pkg{rscala} are updated.  It does *not* affect the classpath.
#'
#' @param JARs Character vector whose elements are some combination of
#'   individual JAR files or package names which contain embedded JARs.  These
#'   JAR files are added to the runtime classpath.
#' @param bridge A Scala bridge from the \code{scala} function.
#'
#' If the \code{JARs} argument is missing, a character vector of loaded JARs
#' is returned.
#'
#' @return Returns \code{NULL}, invisibly.
#' 
#' @export
#' @seealso \code{\link{scalaFindBridge}}
#'
scalaJARs <- function(JARs, bridge=scalaFindBridge()) {
  invisible()
}

