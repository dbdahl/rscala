#' @export
#'
scalaLast <- function(bridge) {
  details <- attr(bridge,"details")
  last <- scalaLastEngine(details)
  if ( details[["interrupted"]] ) invisible() else last
}

scalaLastEngine <- function(details) {
  if ( details[["closed"]] ) stop("Bridge is closed.")
  if ( details[["interrupted"]] ) {
    cat("<< waiting for previously interrupted computation to finish >>\n")
    assign("interrupted",FALSE,envir=details)
    pop(details)
  }
  details[["last"]]
}
