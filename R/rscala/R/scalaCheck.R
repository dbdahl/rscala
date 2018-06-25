#' Check Scala/Java Installation
#'
#' Run this function to confirm that Scala and \R are a compatible version of
#' Java is installed.
#'
#' @param verbose Should a helpful message be displayed?
#'
#' @return Returns a logical indicating whether a compatible version of Java is
#'   installed.
#' @export
#'
#' @examples \donttest{
#'
#' scalaCheck()
#' }
scalaCheck <- function(verbose=TRUE) {
  out <- tryCatch(system2(scalaExec(verbose),c('-nc','-e',shQuote('println(sys.props("java.version")+java.io.File.pathSeparator+scala.util.Properties.versionNumberString)')),stdout=TRUE,stderr=TRUE), warning=function(w) "")
  cells <- strsplit(out,"(:|;)")[[1]]
  javaOK <- grepl("^1\\.8\\.0_",cells[1]) || grepl("^9\\.",out) || grepl("^1\\d+\\.",cells[1])
  scalaOK <- grepl("^2\\.12\\.",cells[2])
  if ( verbose ) {
    if ( javaOK ) {
      cat("Java is properly configured.\n")
    } else {
      cat("Java is not working.  Is Java 8 or higher installed?  Please install it and check again.\n")
    }
    if ( scalaOK ) {
      cat("Scala is properly configured.\n")
    } else {
      cat("Scala is not working.  Please install it and check again.\n")     
    }
  }
  javaOK && scalaOK
}
