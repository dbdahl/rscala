#' Register Functions to Serialize between R and Scala
#'
#' The package provides support for serializing R objects to Scala and
#' unserializing Scala references to R.  These registration functions allows
#' additional, more-specialized serializers and unserializers to be added.
#' Package developers may want to call these registration functions in the
#' \code{\link{.onLoad}} function.
#'
#' @param serializer A function whose first arguments are documented in
#'   \code{\link{scalaSerialize}}.  Other arguments can be used as additional
#'   arguments.
#' @param unserializer A function whose first arguments are documented in
#'   \code{\link{scalaUnserialize}}.  Other arguments can be used as additional
#'   arguments.
#' @param bridge An rscala bridge.
#' @seealso \code{\link{scalaSerialize}}, \code{\link{scalaUnserialize}}
#' @export
#'
#' @examples \donttest{
#' s <- scala()
#'
#' name <- "Grace"
#' nameAsRObject <- scalaSerialize(name)   # Basic serialization
#' scalaType(nameAsRObject)
#' identical(name,scalaUnserialize(nameAsRObject))
#'
#' scalaSerialize.character <- function(x, bridge=scalaFindBridge(), verbose=FALSE) {
#'   if ( verbose ) cat("scalaSerializer.character: Trying...\n")
#'   if ( is.character(x) ) {
#'     if ( verbose ) cat("scalaSerializer.character: Success.\n")
#'     bridge(x=x) ^ 'x'
#'   } else NULL
#' }
#' scalaSerializeRegister(scalaSerialize.character)
#' nameAsString <- scalaSerialize(name)    # More specific serialization
#' scalaType(nameAsString)
#'
#' scalaUnserialize.character <- function(reference, type=scalaType(reference),
#'                                        bridge=scalaFindBridge(reference), verbose=FALSE) {
#'   if ( verbose ) cat("scalaUnserialize.character: Trying...\n")
#'   if ( type == "String" ) {
#'     if ( verbose ) cat("scalaUnserialize.character: Success.\n")
#'     reference$toString()
#'   } else NULL
#' }
#' scalaUnserializeRegister(scalaUnserialize.character)
#' identical(name,scalaUnserialize(nameAsString))
#'
#' close(s)
#' }
#' 
scalaSerializeRegister <- function(serializer, bridge=scalaFindBridge()) {
  details <- attr(bridge,"details")
  assign("serializers",c(serializer,get("serializers",envir=details)),envir=details)
}

#' @rdname scalaSerializeRegister
#' @export
scalaUnserializeRegister <- function(unserializer, bridge=scalaFindBridge()) {
  details <- attr(bridge,"details")
  assign("unserializers",c(unserializer,get("unserializers",envir=details)),envir=details)
}
