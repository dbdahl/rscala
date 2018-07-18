#' Unserialize Object from Scala to R
#' 
#' @param reference An rscala reference.
#' @param use.original If available from a previous serialization to Scala, should the original R object be returned?
#'
#' @export
#' @examples \donttest{
#' scala(assign.name='e')      # Implicitly defines the bridge 'e'.
#' ref <- scalaSerialize(mtcars, e)
#' ref$mpg()
#' mtcars2 <- scalaUnserialize(ref)
#' mtcars2 <- scalaUnserialize(ref)
#' identical(mtcars, mtcars2)
#' close(e)
#' }
#' 
scalaUnserialize <- function(reference, use.original=TRUE) {
  if ( ! inherits(reference,"rscalaReference") ) stop("An rscala reference is required.")
  env <- attr(reference,"rscalaReferenceEnvironment")
  original <- get("original",envir=env)
  if ( ( ! is.null(original) ) && ( use.original ) ) original
  else {
    if ( ! exists("unserializer",envir=env) ) stop("No unserializer is registered for this object.")
    unserialize <- get("unserializer",envir=env)
    unserialize(reference)
  }
}

#' @describeIn scalaUnserialize Unserialize List or Data Frame from Scala to R
#' @export
#' 
scalaUnserialize.list <- function(reference) {
  names <- reference$names()
  asIs <- reference$asIs()
  x <- lapply(seq_along(names),function(i) {
    x <- eval(parse(text=paste0("reference$",names[i],"()")))
    if ( asIs[i] ) I(x) else x
  })
  names(x) <- names
  if ( ! reference$isDataFrame() ) x
  else {
    rowNamesOptions <- reference$rowNames()
    rowNames <- if ( rowNamesOptions$isDefined() ) rowNamesOptions$get() else NULL
    as.data.frame(x,row.names=rowNames,stringsAsFactors=FALSE)
  }
}
