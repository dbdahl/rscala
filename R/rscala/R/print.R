#' @export
#' 
print.rscalaBridge <- function(x, ...) {
  cat(toString(x, ...),"\n",sep="")
}

#' @export
#
toString.rscalaBridge <- function(x, ...) {
  "rscalaBridge"
}

#' @export
#' 
print.rscalaReference <- function(x, ...) {
  cat(toString(x, ...),"\n",sep="")
}

#' @export
#
toString.rscalaReference <- function(x, ...) {
  paste0("Scala reference of type ",x[["type"]])
}
