#' Evaluation Operator
#'
#' @param bridge A foreign language bridge, e.g., an rscala bridge.
#' @param snippet String providing a code snippet to compile and evaluate.
#'
#' @return A vector or matrix of R's basic types (if possible) or an rscala
#'   reference (otherwise).
#' @seealso \code{\link{scala}}
#' @export
#' 
'%~%' <- function(bridge, snippet) UseMethod("%~%")

#' Evaluation Operator
#'
#' @param bridge An rscala bridge.
#' @param snippet String providing a code snippet to compile and evaluate.
#'
#' @return A vector or matrix of R's basic types (if possible) or an rscala
#'   reference (otherwise).
#' @seealso \code{\link{scala}}
#' @export
#'
'%~%.rscalaBridge' <- function(bridge, snippet) {
  details <- attr(bridge,"details")
  args <- if ( is.function(bridge) ) list() else bridge
  scalaInvoke(details, snippet, args, withNames=TRUE)
}

#' Evaluation Operator Ensuring a Reference
#'
#' @param bridge A foreign language bridge, e.g., an rscala bridge.
#' @param snippet String providing a code snippet to compile and evaluate.
#'
#' @return An rscala reference.
#' @seealso \code{\link{scala}}
#' @export
#'
'%.~%' <- function(bridge, snippet) UseMethod("%.~%")

#' Evaluation Operator Ensuring a Reference
#'
#' @param bridge An rscala bridge.
#' @param snippet String providing a code snippet to compile and evaluate.
#'
#' @return An rscala reference.
#' @seealso \code{\link{scala}}
#' @export
#'
'%.~%.rscalaBridge' <- function(bridge, snippet) {
  details <- attr(bridge,"details")
  args <- if ( is.function(bridge) ) list() else bridge
  scalaInvoke(details, paste0(".",snippet), args, withNames=TRUE)
}

#' Execution Operator
#'
#' @param bridge A foreign language bridge, e.g., an rscala bridge.
#' @param snippet String providing a code snippet to compile and execute.
#'
#' @return NULL, invisibly.
#' @seealso \code{\link{scala}}
#' @export
#' 
'%%.rscalaBridge' <- function(bridge, snippet) {
  details <- attr(bridge,"details")
  scalaEvaluate(details, snippet)
}
