#' Title
#'
#' @return
#' @export
#'
#' @examples
#' 
#' @export
#'
scala <- function(useSockets=TRUE) {
  if ( useSockets ) {
    socketIn  <- socketConnection(host="localhost", port=9998, server=FALSE, blocking=TRUE, open="rb")
    socketOut <- socketConnection(host="localhost", port=9999, server=FALSE, blocking=TRUE, open="wb")
  } else {
    socketIn  <- fifo("/home/dahl/docs/devel/rscala2/pipe-s2r", blocking=TRUE, open="rb")
    socketOut <- fifo("/home/dahl/docs/devel/rscala2/pipe-r2s", blocking=TRUE, open="wb")
  }
  details <- list(socketIn=socketIn,socketOut=socketOut)
  bridge <- function(...) {
    bridge2 <- list(...)
    if ( ! all(grepl("^\\w+$",names(bridge2))) ) {
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
