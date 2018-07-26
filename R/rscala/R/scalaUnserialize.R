#' @describeIn scalaSerialize Unserialize a Scala Reference to an R Object
#'
#' @param reference An rscala reference.
#' @param type The type of the rscala reference.  This is computed by the default argument, but may be set for efficency or customization.
#'
#' @export
#' 
scalaUnserialize <- function(reference, type=scalaType(reference), bridge=scalaFindBridge(reference), verbose=FALSE, ...) {
  if ( ! is.scalaReference(reference) ) stop("An rscala reference is required.")
  object <- NULL
  unserializers <- get("unserializers",envir=attr(bridge,"details"))
  for ( unserializer in unserializers ) {
    object <- unserializer(reference, type, bridge, verbose, ...)
    if ( ! is.null(object) ) break
  }
  if ( is.null(object) ) stop("No matching unserializer was found.")
  object
}

#' @describeIn scalaSerialize Unserialize a Scala Reference to an R List or Data Frame
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

#' @describeIn scalaSerialize Unserialize an \code{RObject} or List of \code{RObject}'s from Scala to R
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
