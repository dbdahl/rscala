#' @export
#' 
"$.rscalaBridge" <- function(bridge, snippet) {
  details <- attr(bridge,"details")
  function(...) {
    scalaInvokeWithoutNames(details, snippet, list(...))
  }
}
