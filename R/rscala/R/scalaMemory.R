#' Get or Set Memory Available to Scala
#'
#' Depending on the argument type, this function has several uses related to
#' memory in Scala.
#'
#' @param x If the argument is a string (e.g., "8G" or "512M"), the function
#'   sets the default maximum heap size for new instances of rscala bridges
#'   created by the function \code{\link{scala}}.  If the argument is missing,
#'   the current default maximum heap size for new instances is returned.  Set
#'   the argument to \code{NULL} to disable this global option, and therefore
#'   use \pkg{rscala}'s default.  If the argument is an rscala bridge, the
#'   function returns a numeric vector giving the current heap size and the
#'   maximum heap size, in megabytes.
#'
#' @export
#' @seealso \code{\link{scala}}
#'
#' @examples \dontrun{
#' 
#' scalaMemory("1G")
#' }
scalaMemory <- function(x) {
  if ( missing(x) ) getOption("rscala.heap.maximum")
  else if ( inherits(x,"rscalaBridge") ) {
    x * 'Array((Runtime.getRuntime.totalMemory / ( 1024 * 1024 )).toInt, (Runtime.getRuntime.maxMemory / ( 1024 * 1024 )).toInt)'
  } else {
    if ( ! is.null(x) ) x <- as.character(x)[1]
    options(rscala.heap.maximum=x)
    invisible()
  }
}
