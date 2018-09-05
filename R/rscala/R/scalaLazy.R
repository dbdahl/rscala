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
#' scalaJARs("PATH/TO/jarFileToLoad.jar", e)
#' }
scalaLazy <- function(functions, bridge=scalaFindBridge()) {
  details <- if ( inherits(bridge,"rscalaBridge") ) attr(bridge,"details") else bridge
  if ( details[["suspended"]] ) {
    assign("pendingCallbacks",c(get("pendingCallbacks",envir=details),functions),envir=details)
  } else {
    bridge2 <- if ( inherits(bridge,"rscalaBridge") ) bridge else mkBridge(bridge)
    if ( is.list(functions) ) lapply(functions, function(f) f(bridge2))
    else functions(bridge2)
  }
  invisible() 
}
