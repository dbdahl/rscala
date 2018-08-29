#' Temporarily Suspend Scala by Closing Connections
#'
#' This function temporarily suspends an rscala bridge by closing its associated
#' connections.  The primary place where this function is used is at the end of
#' examples of packages that depend on rscala (because, under some versions of
#' R, "\code{R CMD check --as-cran}" does not permit connections to persist
#' after examples end.
#'
#' @param bridge An rscala bridge.

#' @export
#' @seealso \code{\link{scalaPackage}}
#'
#' @examples \dontrun{
#' 
#' showConnections()
#' scala(assign.name="e")
#' showConnections()         # No additional connections yet.
#' e * "3+4"
#' showConnections()         # Now there are two additional connections.
#' scalaSuspend()
#' showConnections()         # The new connections are gone.
#' e * "3+4"
#' showConnections()         # New connections are established as needed.
#' close(e)
#' }
scalaSuspend <- function(bridge=scalaFindBridge()) {
  details <- attr(bridge,"details")
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
