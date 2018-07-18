#' Register a Function to Serialize from R to Scala
#'
#' @export
#'
scalaRegisterSerializer <- function(serializer, bridge=scalaFindBridge()) {
  details <- attr(bridge,"details")
  assign("serializers",c(serializer,get("serializers",envir=details)),envir=details)
}
