#' @export
#' 
print.rscalaBridge <- function(x, ...) {
  cat(toString(x, ...),"\n",sep="")
}

#' @export
#
toString.rscalaBridge <- function(x, ...) {
  if ( is.function(x) ) "rscala bridge"
  else "rscala bridge with a parameter list"
}

#' @export
#' 
print.rscalaReference <- function(x, ...) {
  cat(toString(x, ...),"\n",sep="")
}

#' @export
#
toString.rscalaReference <- function(x, ...) {
  paste0("rscala reference of type ",attr(x,"rscalaReferenceEnvironment")[["type"]])
}

#' @export
#' 
print.rscalaSub <- function(x, ...) {
  cat(toString(x, ...),"\n",sep="")
}

#' @export
#
toString.rscalaSub <- function(x, ...) {
  "rscala stub"
}

#' @export
#' 
print.rscalaType <- function(x, ...) {
  print(unclass(x), ...)
}
