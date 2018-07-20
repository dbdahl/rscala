#' Add JAR Files to Classpath
#'
#' @param JARs Paths to JAR files, as a character vector.
#' @param bridge An rscala bridge from the \code{scala} function.
#'
#' @return Returns \code{NULL}, invisibly.
#' 
#' @export
#' @seealso \code{\link{scalaFindBridge}}
#'
#' @examples \dontrun{
#' 
#' scalaAddJARs("PATH/TO/jarFileToLoad.jar", e)
#' }
scalaAddJARs <- function(JARs, bridge=scalaFindBridge()) {
  details <- if ( inherits(bridge,"rscalaBridge") ) attr(bridge,"details") else bridge
  checkConnection(details)
  if ( ! is.character(JARs) ) stop("'JARs' should be a character vector.")
  JARs <- path.expand(JARs)
  socketOut <- details[["socketOut"]]
  for ( JAR in JARs ) {
    if ( ! file.exists(JAR) ) stop(paste0("File ",JAR," does not exist."))
    scalaLastEngine(details)
    if ( details[["interrupted"]] ) return(invisible())
    wb(socketOut,PCODE_ADD_TO_CLASSPATH)
    wc(socketOut,JAR)
    tryCatch(pop(details), error=function(e) stop(paste0("Failed to add ",JAR," to classpath.")))
    assign("JARs",c(get("JARs",envir=details),JAR),envir=details)
  }
  invisible() 
}
