#' Find a Scala Bridge
#'
#' This functions attempts to find an instance of an rscala bridge in the environment path.
#'
#' @return An rscala bridge.
#' @export
#'
scalaFindBridge <- function() {
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

