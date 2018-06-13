#' @export
#' 
"$.rscalaBridge" <- function(bridge, snippet) {
  details <- attr(bridge,"details")
  function(...) {
    scalaInvoke(details, snippet, list(...))
  }
}

#' @export
#' 
"$.rscalaReference" <- function(reference, snippet) {
  details <- reference[["details"]]
  function(...) {
    scalaInvoke(details, snippet, list(...,reference), withReference=TRUE)
  }
}
