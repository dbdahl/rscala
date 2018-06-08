#' Title
#'
#' @return
#' @export
#'
#' @examples
#' 
#' @export
#'
scala <- function() {
  socketIn  <- socketConnection(host="localhost", port=9998, server = FALSE, blocking = TRUE, open = "rb", timeout = getOption("timeout"))
  socketOut <- socketConnection(host="localhost", port=9999, server = FALSE, blocking = TRUE, open = "wb", timeout = getOption("timeout"))
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
