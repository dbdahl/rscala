#' Temporarily Disconnect Scala by Closing Connections
#'
#' This function temporarily disconnects an rscala bridge by closing its
#' associated socket connections.  The primary place where this function is used
#' is at the end of examples of packages that depend on rscala (because, under
#' some versions of R, "\code{R CMD check --as-cran}" does not permit
#' connections to persist after an example ends).
#'
#' @param bridge An rscala bridge.
#' @export
#' @examples \dontrun{
#'
#' showConnections()
#' s <- scala()
#' showConnections()         # No additional connections yet.
#' s * "3+4"
#' showConnections()         # Now there are two additional connections.
#' scalaSuspend()
#' showConnections()         # The new connections are gone.
#' s * "3+4"
#' showConnections()         # New connections are established as needed.
#' close(s)
#' }
scalaDisconnect <- function(bridge=scalaFindBridge()) {
  details <- attr(bridge,"details")
  if ( details[["disconnected"]] ) return(invisible())
  scalaLastEngine(details)
  if ( details[["interrupted"]] ) return(invisible())
  socketOut <- details[["socketOut"]]
  socketIn  <- details[["socketIn"]]
  wb(socketOut,PCODE_SUSPEND)
  close(socketIn)
  close(socketOut)
  assign("disconnected",TRUE,envir=details)
  invisible()
}
