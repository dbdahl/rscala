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
#' scala(assign.name='e')      # Implicitly defines the bridge 'e'.
#' e * "2+3"
#' scalaLast(e)
#' close(e)
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
