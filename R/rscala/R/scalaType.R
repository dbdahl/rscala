#' Work with Scala Types
#'
#' @param type An rscala reference or a character vector of length one giving a Scala type.
#'
#' @return A character vector of length one indicating a Scala type.
#' @export
#'
#' @examples
#' scalaType("Double")
#' 
scalaType <- function(type) {
  type <- if ( inherits(type,"rscalaReference") ) attr(type,"rscalaReferenceEnvironment")[["type"]]
  else if ( ( typeof(type) == "character" ) && ( length(type) == 1 ) ) type
  else stop("Illegal argument.")
  structure(type, class="rscalaType")
}
