#' @export
#' 
"$.rscalaBridge" <- function(bridge, snippet) {
  details <- attr(bridge,"details")
  function(...) {
    scalaInvoke(details, snippet, list(...))
  }
}
