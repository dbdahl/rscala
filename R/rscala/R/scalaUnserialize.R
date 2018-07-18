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
#' identical(mtcars, mtcars2)
#' close(e)
#' }
#' 
scalaUnserialize <- function(reference, use.original=TRUE, ...) {
  if ( ! inherits(reference,"rscalaReference") ) stop("An rscala reference is required.")
  envOfReference <- attr(reference,"rscalaReferenceEnvironment")
  if ( ( use.original ) && ( exists("object",envir=envOfReference) ) ) get("object",envir=env)
  else {
    if ( ! exists("unserializer",envir=envOfReference) ) stop("No unserializer is registered for this reference.")
    unserialize <- get("unserializer",envir=envOfReference)
    original <- unserialize(reference, ...)
    envOfObject <- new.env(parent=emptyenv())
    attr(original,"rscalaObjectEnvironment") <- envOfObject
    assign("reference",reference,envir=envOfObject)
    assign("original",original,envir=envOfReference)
    original
  }
}

#' @describeIn scalaUnserialize Unserialize List or Data Frame from Scala to R
#' @export
#' 
scalaUnserialize.list <- function(reference, ...) {
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
