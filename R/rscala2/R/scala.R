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
  list(socketIn=socketIn, socketOut=socketOut)
}
