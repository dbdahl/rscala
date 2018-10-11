#' Lazily Execute Functions on a Scala Bridge
#'
#' @param functions A single function or list of functions.  Each function takes a Scala bridge as its only argument.
#'   These functions are called immediately after the next time the bridge is connected.
#'   These functions are where setup code should go, like \emph{global}
#'   imports, objects, classes, methods, etc.  For example, it might equal
#'   \code{function(s) { s + 'import scala.util.Random' }}.  \strong{Note} the
#'   use of the declaration operator \code{+} instead of the operators \code{*}
#'   or \code{^}.
#' @param bridge An rscala bridge from the \code{scala} function.
#'
#' @return Returns \code{NULL}, invisibly.
#' 
#' @export
#' @seealso \code{\link{scalaFindBridge}}
#'
#' @examples \donttest{
#' 
#' s <- scala()
#' scalaLazy(function(s) { s + 'import scala.util.Random' })
#' s$.new_Random()$nextDouble()
#' close(s)
#' }
scalaLazy <- function(functions, bridge=scalaFindBridge()) {
  details <- if ( inherits(bridge,"rscalaBridge") ) attr(bridge,"details") else bridge
  if ( details[["disconnected"]] ) {
    assign("pendingCallbacks",c(get("pendingCallbacks",envir=details),functions),envir=details)
  } else {
    bridge2 <- if ( inherits(bridge,"rscalaBridge") ) bridge else mkBridge(bridge)
    if ( is.list(functions) ) lapply(functions, function(f) f(bridge2))
    else functions(bridge2)
  }
  invisible() 
}
