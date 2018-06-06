#' Title
#'
#' @param x 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
#' 
#' @export
#' 
scalaDef <- function(bridge, x, ...) {
  argsValues <- list(...)
  argsTypes <- if ( length(argsValues) > 0 ) sapply(list(...), push, bridge=bridge, getType=TRUE) else integer()
  list(x, argsTypes)
}
