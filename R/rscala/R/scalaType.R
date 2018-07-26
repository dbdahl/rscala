#' Get or Provide a Scala Type
#'
#' @param type An rscala reference or a character vector of length one giving a Scala type.
#'
#' @return A character vector of length one indicating a Scala type.
#' @export
#'
#' @examples
#' scalaType("Double")
#' scalaType("D0")
#' scalaType("Array[String]")
#' scalaType("S1")
#' 
scalaType <- function(type) {
  type <- if ( is.scalaReference(type) ) attr(type,"rscalaReferenceEnvironment")[["type"]]
  else if ( ( typeof(type) == "character" ) && ( length(type) == 1 ) ) {
    if ( nchar(type) == 2 ) {
      if ( FALSE ) ""
      else if ( type == "I0" ) "Int"
      else if ( type == "D0" ) "Double"
      else if ( type == "L0" ) "Boolean"
      else if ( type == "R0" ) "Byte"
      else if ( type == "S0" ) "String"
      else if ( type == "I1" ) "Array[Int]"
      else if ( type == "D1" ) "Array[Double]"
      else if ( type == "L1" ) "Array[Boolean]"
      else if ( type == "R1" ) "Array[Byte]"
      else if ( type == "S1" ) "Array[String]"
      else if ( type == "I2" ) "Array[Array[Int]]"
      else if ( type == "D2" ) "Array[Array[Double]]"
      else if ( type == "L2" ) "Array[Array[Boolean]]"
      else if ( type == "R2" ) "Array[Array[Byte]]"
      else if ( type == "S2" ) "Array[Array[String]]"
      else type
    } else type
  }
  else stop("Illegal argument.")
  structure(type, class="rscalaType")
}
