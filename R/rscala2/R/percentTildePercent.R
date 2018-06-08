#'
#' @export
#' 
'%~%' <- function(bridge, snippet) UseMethod("%~%")

#'
#' @export
#' 
'%~%.rscalaBridge' <- function(bridge, snippet) {
  details <- attr(bridge,"details")
  args <- if ( is.function(bridge) ) list() else bridge
  scalaInvoke(details, snippet, args, withNames=TRUE)
}
