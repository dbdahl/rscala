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
scalaDef <- function(x,...) {
  argsValues <- list(...)
  argsTypes <- if ( length(argsValues) > 0 ) sapply(list(...), push, getType=TRUE) else integer()
  list(x, argsTypes)
}
