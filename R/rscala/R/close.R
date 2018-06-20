#' Close rscala Bridge
#'
#' @param con rscala bridge.
#' @param ... Currently ignored.
#'
#' @return Returns \code{NULL}, invisibly.
#' @export
#'
close.rscalaBridge <- function(con, ...) {
  details <- if ( inherits(con,"rscalaBridge") ) attr(con,"details") else con
  if ( details[["closed"]] ) return(invisible())
  assign("closed",TRUE,envir=details)
  close(details[["buffer"]])
  if ( details[["connected"]] ) {
    close(details[["socketIn"]])
    close(details[["socketOut"]])
  }
  sessionFilename <- details[['sessionFilename']]
  if ( file.exists(sessionFilename) ) {
    unlink(sessionFilename)
    if ( identical(.Platform$OS.type,"windows") && ! interactive() ) {
      Sys.sleep(6)
    }
  }
  invisible()
}
