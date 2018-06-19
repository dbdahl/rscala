#' Check Scala/Java Installation
#'
#' Run this function to confirm that Scala and R are a compatible version of Java is installed.
#'
#' @param verbose Should a helpful message be displayed?
#'
#' @return A logical indicating whether a compatible version of Java is insalled.
#' @export
#'
#' @examples{
#' scalaCheck()
#' }
scalaCheck <- function(verbose=TRUE) {
  out <- tryCatch(system2(scalaExec(FALSE),c('-e',"'print(sys.props(\"java.version\")+\" \"+scala.util.Properties.versionNumberString)'"),stdout=TRUE,stderr=TRUE,timeout=60), warning=function(w) "")
  cells <- strsplit(out," ")[[1]]
  javaOK <- grepl("^1\\.8\\.0_",cells[1]) || grepl("^9\\.",out) || grepl("^1\\d+\\.",cells[1])
  scalaOK <- grepl("^2\\.12\\.",cells[2])
  if ( verbose ) {
    if ( javaOK ) {
      cat("Java success: Java is properly configured.\n")
    } else {
      cat("Java failure: Is Java 8 or higher installed?  Please install it and check again.\n")
    }
    if ( scalaOK ) {
      cat("Scala success: Scala is properly configured.\n")
    } else {
      cat("Scala failure: Is Scala 2.12 installed?  Please install it and check again.\n")     
    }
  }
  javaOK
}
