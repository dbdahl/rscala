#' @export
#' 
"$.rscalaBridge" <- function(bridge, snippet) {
  if ( ! is.function(bridge) ) stop("When defining an rscala function, parameter lists are not supported.")
  details <- attr(bridge,"details")
  structure(function(...) {
    scalaInvoke(details, snippet, list(...))
  },class="rscalaFunction")
}

#' @export
#' 
"$.rscalaReference" <- function(reference, snippet) {
  details <- reference[["details"]]
  structure(function(...) {
    scalaInvoke(details, snippet, list(...,reference), withReference=TRUE)
  },class="rscalaFunction")
}

#' @export
#' 
"$<-.rscalaBridge" <- function(bridge, snippet, value) {
  details <- attr(bridge,"details")
  if ( snippet == "showCode" ) {
    bridge(x=as.logical(value[1])) * "conduit.showCode = x"
  } else if ( snippet == "debug" ) {
    bridge(x=as.logical(value[1])) * "conduit.debug = x"
  } else stop(paste0("Unsupported option: ", snippet))
  bridge
}
