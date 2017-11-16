is.scalaReference <- function(x) {
  inherits(x,"ScalaCachedReference") ||
  inherits(x,"ScalaInterpreterReference") ||
  inherits(x,"ScalaInterpreterItem") ||
  inherits(x,"ScalaNullReference")
}

