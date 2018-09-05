#' Retrieve the Last Scala Computation
#'
#' This function retrieves the last result from the supplied rscala bridge.
#'
#' @param bridge An rscala bridge
#'
#' @export
#' @seealso \code{\link{scalaFindBridge}}
#' 
#' @examples \donttest{
#' s <- scala()
#' s * "2+3"
#' scalaLast(s)
#' close(s)
#' }
#' 
scalaLast <- function(bridge=scalaFindBridge()) {
  details <- attr(bridge,"details")
  last <- scalaLastEngine(details)
  if ( details[["interrupted"]] ) invisible() else last
}

scalaLastEngine <- function(details) {
  checkConnection(details)
  if ( details[["interrupted"]] ) {
    cat("<< waiting for previously interrupted computation to finish >>\n")
    assign("interrupted",FALSE,envir=details)
    pop(details)
  }
  details[["last"]]
}
