#' Add a JAR File to the Classpath
#'
#' @param bridge An rscala bridge from the \code{scala} function.
#' @param JAR A character vector of length one giving the path to a JAR file.
#'
#' @return Returns \code{NULL}, invisibly.
#' 
#' @export
#'
#' @examples{
#' \dontrun{
#' scalaAddJAR(s, "PATH/TO/jarToLoad.jar")
#' }
#' }
scalaAddJAR <- function(bridge, JAR) {
  details <- attr(bridge,"details")
  scalaLastEngine(details)
  if ( details[["interrupted"]] ) return(invisible())
  socketOut <- details[["socketOut"]]
  wb(socketOut,PCODE_ADD_TO_CLASSPATH)
  wc(socketOut,as.character(JAR))
  invisible(pop(details))
}
