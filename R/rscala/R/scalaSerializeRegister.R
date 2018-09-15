#' Register Functions to Push and Pull Between R and Scala
#'
#' The 'rscala' package provides support for serializing objects between R and Scala.
#' These registration functions allows
#' additional, more-specialized push and pull methods to be added.
#' Package developers may want to call these registration functions in the package's
#' \code{\link{.onLoad}} function.
#'
#' @param pusher A function whose first two arguments are as shown in the example below.
#'   Other arguments can be used as additional arguments.
#' @param puller A function whose first two arguments are as shown in the example below.
#'   Other arguments can be used as additional arguments.
#' @param method A string giving the name of the specific 'push' or 'pull' method.
#' @param bridge An rscala bridge.
#' @seealso \code{\link{scalaPush}}, \code{\link{scalaPull}}
#' @export
#'
#' @examples \donttest{
#' s <- scala()
#'
#' name <- "Grace"
#' nameAsRObject <- scalaPush(name,"generic")   # Basic serialization
#' scalaType(nameAsRObject)
#' identical(name,scalaPull(nameAsRObject,"generic"))
#'
#' scalaPush.character <- function(x, bridge) {
#'   if ( is.character(x) && ( length(x) == 1L ) ) bridge(x=x) ^ 'x'
#'   else stop("'x' should be a character vector.")
#' }
#' scalaPushRegister(scalaPush.character, "character")
#' nameAsString <- scalaPush(name, "character", s)    # More specific serialization
#' scalaType(nameAsString)
#'
#' scalaPull.character <- function(reference, bridge) {
#'   if ( scalaType(reference) == "String" ) reference$toString()
#'   else stop("'reference' should be a 'String'.")
#' }
#' scalaPullRegister(scalaPull.character, "character")
#' identical(name,scalaPull(nameAsString,"character"))
#'
#' close(s)
#' }
#' 
scalaPushRegister <- function(pusher, method, bridge=scalaFindBridge()) {
  assign(method,pusher,envir=get("pushers",envir=attr(bridge,"details")))
}

#' @rdname scalaPushRegister
#' @export
scalaPullRegister <- function(puller, method, bridge=scalaFindBridge()) {
  assign(method,puller,envir=get("pullers",envir=attr(bridge,"details")))
}
