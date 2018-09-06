#' Test for Scala Reference
#'
#' @param x An arbitrary R object.
#'
#' @return Logical indicating whether \code{x} is an rscala reference.
#' @export
#'
#' @examples
#' is.scalaReference(c(1,2))
#' 
is.scalaReference <- function(x) inherits(x,"rscalaReference")
