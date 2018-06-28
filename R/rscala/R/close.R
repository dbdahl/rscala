#' Close a Scala Bridge
#'
#' @param con An rscala bridge.
#' @param ... Currently ignored.
#'
#' @return Returns \code{NULL}, invisibly.
#' @export
#'
close.rscalaBridge <- function(con, ...) {
  details <- if ( inherits(con,"rscalaBridge") ) attr(con,"details") else con
  if ( details[["closed"]] ) return(invisible())
  assign("closed",TRUE,envir=details)
  if ( details[["connected"]] ) {
    close(details[["socketIn"]])
    close(details[["socketOut"]])
  }
  unlink(details[["sessionFilename"]])
  invisible()
}

