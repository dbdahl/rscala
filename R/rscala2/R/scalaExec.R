#' Get Path of Scala Executable
#'
#' @param verbose Logical indicating whether details of the search should be printed to the console.
#'
#' @return The path to the Scala executable, as a character vector of length one.
#' @export
#'
#' @examples{
#' scalaExec()
#' }
scalaExec <- function(verbose=TRUE) {
  candidate <- addBat(file.path(Sys.getenv("SCALA_HOME"),"bin","scala"))
  if ( file.exists(candidate) ) {
    if ( verbose ) cat("SCALA_HOME environment method succeeded.\n")
    return(candidate)
  } else if ( verbose ) cat("SCALA_HOME environment method failed.\n")
  candidate <- Sys.which("scala")
  if ( file.exists(candidate) ) {
    if ( verbose ) cat("Shell PATH method succeeded.\n")
    return(unname(candidate))
  } else if ( verbose ) cat("Shell PATH method failed.\n")
  candidate <- addBat(file.path(installPath(),"bin","scala"))
  if ( file.exists(candidate) ) {
    if ( verbose ) cat("~/.rscala directory method succeeded.\n")
    return(candidate)
  } else if ( verbose ) cat("~/.rscala directory method failed.\n")
  if ( ! interactive() ) ""
  while ( TRUE ) {
    response <- readline(prompt="Scala is not found.  Would you like to install it now? [Y/n] ")
    response <- toupper(trimws(response))
    if ( response == "" ) response <- "Y"
    if ( response == "Y" ) {
      scalaInstall()
      if ( file.exists(candidate) ) return(candidate)
      else return("")
    }
    if ( response == "N" ) return("")
  }
}

addBat <- function(cmd) if ( .Platform$OS.type == "windows" ) paste0(cmd,".bat") else cmd
