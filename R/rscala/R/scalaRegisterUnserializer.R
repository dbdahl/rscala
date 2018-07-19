#' Register a Function to Serialize from R to Scala
#'
#' @export
#'
scalaRegisterUnserializer <- function(unserializer, bridge=scalaFindBridge()) {
  details <- attr(bridge,"details")
  assign("unserializers",c(unserializer,get("unserializers",envir=details)),envir=details)
}
