#' Close rscala Bridge
#'
#' @param con rscala bridge.
#' @param ... Currently ignored.
#'
#' @return Returns \code{NULL}, invisibly.
#' @export
#'
close.rscalaBridge <- function(con, ...) {
  details <- attr(con,"details")
  if ( details[["closed"]] ) return(invisible())
  scalaLastEngine(details)
  if ( details[["interrupted"]] ) return(invisible())
  socketOut <- details[["socketOut"]]
  wb(socketOut,PCODE_EXIT)
  close(details[["buffer"]])
  close(details[["socketIn"]])
  close(details[["socketOut"]])
  unlink(details[['sessionFilename']])
  assign("closed",TRUE,envir=details)
  assign("killStamp",proc.time()['elapsed'],envir=details)
  invisible()
}
