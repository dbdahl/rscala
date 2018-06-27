#' Globally Set Scala's Maximum Heap Size
#'
#' This function sets the maximum heap size for new instances of rscala bridges
#' created by the function \code{\link{scala}}.  The value set here overrides
#' the argument of the same name of the function \code{\link{scala}}.  Set the
#' argument to \code{NULL} to disable this global option.  The function is helpful
#' for users of packages that depend on rscala.
#'
#' @inheritParams scala
#'
#' @return Returns \code{NULL}, invisibly.
#' @export
#' @seealso \code{\link{scala}}
#'
#' @examples \dontrun{
#' scalaHeapMaximum("1G")
#' }
scalaHeapMaximum <- function(heap.maximum=NULL) {
  options(rscala.heap.maximum=heap.maximum)
  invisible()
}