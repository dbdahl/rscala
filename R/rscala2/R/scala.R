#' Title
#'
#' @return
#' @export
#'
#' @examples
#' 
#' @export
#'
scala <- function(useSockets=TRUE, useBuffer=TRUE) {
  if ( useSockets ) {
    socketIn  <- socketConnection(host="localhost", port=9998, server=FALSE, blocking=TRUE, open="rb")
    socketOut <- socketConnection(host="localhost", port=9999, server=FALSE, blocking=TRUE, open="ab")
  } else {
    socketIn  <- file("/home/dahl/docs/devel/rscala2/R/rscala2/pipe-s2r", blocking=TRUE, open="rb")
    socketOut <- file("/home/dahl/docs/devel/rscala2/R/rscala2/pipe-r2s", blocking=TRUE, open="ab")
  }
  details <- list2env(parent=emptyenv(), list(
      socketIn=socketIn,
      socketOut=socketOut,
      useBuffer=useBuffer,
      buffer=rawConnection(raw(),open="wb")))
  bridge <- function(...) {
    bridge2 <- list(...)
    argNames <- names(bridge2)
    if ( ( length(bridge2) > 0 )  && ( is.null(argNames) || ! all(grepl("^\\w+$",argNames,perl=TRUE)) ) ) {
      stop("Argument names must be given and consist only alphanumeric & underscore characters.")
    }
    attr(bridge2,"details") <- details
    class(bridge2) <- "rscalaBridge"
    bridge2
  }
  attr(bridge,"details") <- details
  class(bridge) <- "rscalaBridge"
  assign("s",bridge,envir=.GlobalEnv)
}
