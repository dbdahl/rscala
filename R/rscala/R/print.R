#' @export
#' 
print.rscalaBridge <- function(x, ...) {
  cat(toString(x, ...),"\n",sep="")
}

#' @export
#
toString.rscalaBridge <- function(x, ...) {
  if ( is.function(x) ) "Scala bridge"
  else "Scala bridge with a parameter list"
}

#' @export
#' 
print.rscalaReference <- function(x, ...) {
  env <- attr(x,"rscalaReferenceEnvironment")
  if ( exists("original",envir=env) ) {
    func <- get("original",envir=env)
    if ( ! is.null(func) ) print(func)
  }
  cat(toString(x, ...),"\n",sep="")
}

#' @export
#
toString.rscalaReference <- function(x, ...) {
  env <- attr(x,"rscalaReferenceEnvironment")
  paste0("rscala reference of type ",env[["type"]])
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
toString.rscalaType <- function(x, ...) {
  paste0(unclass(x))
}

#' @export
#' 
print.rscalaType <- function(x, ...) {
  cat(toString(x, ...),"\n",sep="")
}
