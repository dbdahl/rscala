#' Get or Set Memory Available to Scala
#'
#' Depending on the argument type, this function has several uses related to
#' memory in Scala.
#'
#' @param x If the argument is a string (e.g., "8G" or "512M"), the function
#'   sets the default maximum heap size for new instances of rscala bridges
#'   created by the function \code{\link{scala}}.  If the argument is missing,
#'   the current default maximum heap size for new instances is returned.  Set
#'   the argument to \code{NULL} to disable this global option and, therefore,
#'   use Scala's own default.  If the argument is an rscala bridge, the function
#'   returns a numeric vector giving the current and the maximum heap sizes.
#'
#' @export
#' @seealso \code{\link{scala}}
#'
#' @examples \dontrun{
#' 
#' scalaMemory("1G")
#' }
scalaMemory <- function(x) {
  if ( missing(x) ) getOption("rscala.heap.maximum")
  else if ( inherits(x,"rscalaBridge") ) {
    x * 'Array((Runtime.getRuntime.totalMemory / ( 1024 * 1024 )).toInt, (Runtime.getRuntime.maxMemory / ( 1024 * 1024 )).toInt)'
  } else {
    options(rscala.heap.maximum=as.character(x)[1])
    invisible()
  }
}

#' @export
scalaPackageSuspend <- function(s) {
  details <- attr(s,"details")
  if ( details[["suspended"]] ) return(invisible())
  scalaLastEngine(details)
  if ( details[["interrupted"]] ) return(invisible())
  socketOut <- details[["socketOut"]]
  socketIn  <- details[["socketIn"]]
  wb(socketOut,PCODE_SUSPEND)
  close(socketIn)
  close(socketOut)
  assign("suspended",TRUE,envir=details)
  invisible()
}

#' @export
scalaPackageResume <- function(details) {
  socketIn  <- socketConnection(host="localhost", port=details[['socketInPort']], server=FALSE, blocking=TRUE, open="rb", timeout=2678400L)
  socketOut <- socketConnection(host="localhost", port=details[['socketOutPort']], server=FALSE, blocking=TRUE, open="ab", timeout=2678400L)
  assign("socketIn",socketIn,envir=details)
  assign("socketOut",socketOut,envir=details)
  assign("suspended",FALSE,envir=details)
  invisible()
}
