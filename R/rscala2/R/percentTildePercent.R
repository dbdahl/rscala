#'
#' @export
#' 
'%~%' <- function(bridge, snippet) UseMethod("%~%")

#'
#' @export
#' 
'%~%.rscalaBridge' <- function(bridge, snippet) {
  details <- attr(bridge,"details")
  if ( is.function(bridge) ) scalaInvokeWithNames(details, snippet, list())
  else scalaInvokeWithNames(details, snippet, bridge)
}
