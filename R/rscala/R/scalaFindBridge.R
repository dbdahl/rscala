#' Find a Scala Bridge
#'
#' This function attempts to find an instance of an rscala bridge based on an
#' rscala reference or by searching the environment path.
#'
#' @param reference Either: i. An rscala reference, or ii. \code{NULL} (in which
#'   case the environment path is searched).
#'
#' @return An rscala bridge.
#' @export
#' 
scalaFindBridge <- function(reference=NULL) {
  if ( ! is.null(reference) ) {
    mkBridge(attr(reference,"rscalaReferenceEnvironment")[["details"]])
  } else {
    counter <- 2
    while ( TRUE ) {
      frame <- parent.frame(counter)
      w <- which(sapply(ls(envir=frame),function(x) class(get(x,envir=frame)))=="rscalaBridge")
      if ( length(w) == 1 ) return(get(names(w),envir=frame))
      if ( length(w) >  1 ) stop("Multiple Scala instances were found in the same environment.")
      if ( identical(frame,.GlobalEnv) ) stop("Cannot find an rscala bridge.")
      counter <- counter + 1
    }
  }
}
