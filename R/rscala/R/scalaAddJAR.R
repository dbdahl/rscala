#' Add JAR Files to the Classpath
#'
#' @param bridge An rscala bridge from the \code{scala} function.
#' @param JARs Paths to JAR files, as a character vector.
#'
#' @return Returns \code{NULL}, invisibly.
#' 
#' @export
#'
#' @examples \dontrun{
#' 
#' scalaAddJARs(e, "PATH/TO/jarFileToLoad.jar")
#' }
scalaAddJARs <- function(bridge, JARs) {
  if ( ! inherits(bridge,"rscalaBridge") ) stop("'bridge' should be a Scala bridge.")
  if ( ! is.character(JARs) ) stop("'JARs' should be a character vector.")
  JARs <- path.expand(JARs)
  details <- attr(bridge,"details")
  socketOut <- details[["socketOut"]]
  for ( JAR in JARs ) {
    if ( ! file.exists(JAR) ) stop(paste0("File ",JAR," does not exist."))
    scalaLastEngine(details)
    if ( details[["interrupted"]] ) return(invisible())
    wb(socketOut,PCODE_ADD_TO_CLASSPATH)
    wc(socketOut,JAR)
    tryCatch(pop(details), error=function(e) stop(paste0("Failed to add ",JAR," to classpath.")))
    invisible() 
  }
}
