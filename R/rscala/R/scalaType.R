#' Get or Specify a Scala Type
#'
#' This function gets the Scala type of an rscala reference.  It also, together
#' with the associated convenience objects, specifies a Scala type for
#' transcompilation purposes.
#' 
#' The convenience objects are of the form \code{stXY} (where \code{X} is in
#' \code{{I, D, L, R, S}} and \code{Y} is in \code{{0, 1, 2}}) as as indicated
#' below:
#' 
#' \itemize{
#'   \item \code{I} corresponds to Scala's \code{Int} and R's \code{integer}.
#'   \item \code{D} corresponds to Scala's \code{Double} and R's \code{double}.
#'   \item \code{L} corresponds to Scala's \code{Boolean} and R's \code{logical}.
#'   \item \code{R} corresponds to Scala's \code{Byte} and R's \code{raw}.
#'   \item \code{S} corresponds to Scala's \code{String} and R's \code{character}.
#' }
#'   
#' \itemize{
#'   \item \code{0} corresponds to a Scala primitive and an R length one vector.
#'   \item \code{1} corresponds to a Scala array and an R vector.
#'   \item \code{2} corresponds to a Scala array of arrays and an R matrix.
#' }
#' 
#' For example, \code{stS2} is equivalent to Scala's
#' \code{scalaType("Array[Array[String]]")} and R's type for
#' \code{matrix(character())}.  Also, \code{stL1} is equivalent to Scala's
#' \code{scalaType("Boolean")} and R's type for \code{logical(1)}.
#' 
#' @param type An rscala reference or a character vector of length one giving a
#'   Scala type.
#'
#' @return An object of class \code{rscalaType} whose value is a character
#'   vector of length one indicating a Scala type.
#' @format See 'Value' below.
#' 
#' @export
#'
#' @examples
#' scalaType("Double")
#' stD0
#' scalaType("Array[Byte]")
#' stR1
#' scalaType("Array[Array[Int]]")
#' stI2
#' 
scalaType <- function(type) {
  type <- if ( is.scalaReference(type) ) attr(type,"rscalaReferenceEnvironment")[["type"]]
  else if ( ( typeof(type) == "character" ) && ( length(type) == 1 ) ) type
  else stop("Illegal argument.")
  structure(type, class="rscalaType")
}

#' @rdname scalaType
#' @export
stI0 <- scalaType("Int")

#' @rdname scalaType
#' @export
stD0 <- scalaType("Double")

#' @rdname scalaType
#' @export
stL0 <- scalaType("Boolean")

#' @rdname scalaType
#' @export
stR0 <- scalaType("Byte")

#' @rdname scalaType
#' @export
stS0 <- scalaType("String")

#' @rdname scalaType
#' @export
stI1 <- scalaType("Array[Int]")

#' @rdname scalaType
#' @export
stD1 <- scalaType("Array[Double]")

#' @rdname scalaType
#' @export
stL1 <- scalaType("Array[Boolean]")

#' @rdname scalaType
#' @export
stR1 <- scalaType("Array[Byte]")

#' @rdname scalaType
#' @export
stS1 <- scalaType("Array[String]")

#' @rdname scalaType
#' @export
stI2 <- scalaType("Array[Array[Int]]")

#' @rdname scalaType
#' @export
stD2 <- scalaType("Array[Array[Double]]")

#' @rdname scalaType
#' @export
stL2 <- scalaType("Array[Array[Boolean]]")

#' @rdname scalaType
#' @export
stR2 <- scalaType("Array[Array[Byte]]")

#' @rdname scalaType
#' @export
stS2 <- scalaType("Array[Array[String]]")
