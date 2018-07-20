#' Unserialize a Scala Reference from Scala to R
#' 
#' This function tries to unserialize an rscala reference from Scala to R.  A few built unserializers are provided and more may be added using the function \code{\link{scalaUnserializeRegister}}.
#' 
#' @param reference An rscala reference.
#' @param type The type of the rscala reference.  This is computed by the default argument, but may be set for efficency or customization.
#' @param bridge The rscala bridge associated the rscala reference.  This is computed by the default argument, but may be set for efficency.
#' @param verbose Should details of the search for an appropriate unserializer function be shown?
#' @param ... Extra arguments passed to type-specific unserializers.
#'
#' @seealso \code{\link{scalaSerialize}}, \code{\link{scalaUnserializeRegister}}, \code{\link{scalaFindBridge}}
#' @export
#' @examples \donttest{
#' scala(assign.name='e')      # Implicitly defines the bridge 'e'.
#' 
#' mtcarsRef <- scalaSerialize(mtcars, e)
#' mtcarsRef$names()
#' mtcarsRef$mpg()
#' mtcars2 <- scalaUnserialize(mtcarsRef)
#' identical(mtcars, mtcars2)
#' 
#' ref <- scalaSerialize(iris, e, verbose=TRUE)
#' scalaType(ref)
#' scalaUnserialize(ref)  # Oops, variable names are lost.
#' 
#' irisCleaned <- iris
#' names(irisCleaned) <- gsub("\\W","_",names(iris))
#' irisCleaned$Species <- as.character(iris$Species)
#' ref2 <- scalaSerialize(irisCleaned, e, verbose=TRUE)
#' scalaType(ref2)
#' ref2$Sepal_Length()
#' irisCleaned2 <- scalaUnserialize(ref2)
#' identical(irisCleaned, irisCleaned2)
#' 
#' close(e)
#' }
#' 
scalaUnserialize <- function(reference, type=scalaType(reference), bridge=scalaFindBridge(reference), verbose=FALSE, ...) {
  if ( ! inherits(reference,"rscalaReference") ) stop("An rscala reference is required.")
  object <- NULL
  unserializers <- get("unserializers",envir=attr(bridge,"details"))
  for ( unserializer in unserializers ) {
    object <- unserializer(reference, type, bridge, verbose, ...)
    if ( ! is.null(object) ) break
  }
  if ( is.null(object) ) stop("No matching unserializer was found.")
  object
}

#' @describeIn scalaUnserialize Unserialize an R List or Data Frame from Scala to R
#' @export
#' 
scalaUnserialize.list <- function(reference, type=scalaType(reference), bridge=scalaFindBridge(reference), verbose=FALSE) {
  if ( verbose ) cat("scalaUnserialize.list: Trying...\n")
  if ( ! grepl("List\\d+",type) ) return(NULL)
  names <- reference$names()
  asIs <- reference$asIs()
  x <- lapply(seq_along(names),function(i) {
    x <- eval(parse(text=paste0("reference$",names[i],"()")))
    if ( asIs[i] ) I(x) else x
  })
  names(x) <- names
  if ( ! reference$isDataFrame() ) {
    if ( verbose ) cat("scalaUnserialize.list: Success with general list.\n")
    x
  }
  else {
    rowNamesOptions <- reference$rowNames()
    rowNames <- if ( rowNamesOptions$isDefined() ) rowNamesOptions$get() else NULL
    if ( verbose ) cat("scalaUnserialize.list: Success with data frame.\n")
    as.data.frame(x,row.names=rowNames,stringsAsFactors=FALSE)
  }
}

#' @describeIn scalaUnserialize Unserialize an \code{RObject} or List of \code{RObject}'s from Scala to R
#' @export
#' 
scalaUnserialize.generic <- function(reference, type=scalaType(reference), bridge=scalaFindBridge(reference), verbose=FALSE) {
  if ( verbose ) cat("scalaUnserialize.generic: Trying...\n")
  if ( type == "org.ddahl.rscala.RObject" ) {
    if ( verbose ) cat("scalaUnserialize.generic: Success on single element.\n")
    unserialize(reference$x())
  }
  else if ( type == "List[org.ddahl.rscala.RObject]" ) {
    pair <- bridge(arr=reference) ^ '(arr.flatMap(_.x).toArray, arr.scanLeft(1)((sum,y) => sum + y.x.length).toArray)'
    bytes <- pair$"_1"()
    sizes <- pair$"_2"()
    if ( verbose ) cat("scalaUnserialize.generic: Success on list of elements.\n")
    lapply(seq_along(sizes[-1]), function(i) {
      unserialize(bytes[sizes[i]:(sizes[i+1]-1)])
    }) 
  } else NULL
}
