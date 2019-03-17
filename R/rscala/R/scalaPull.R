#' @rdname scalaPush
#'
#' @param reference An rscala reference.
#'
#' @export
#' 
scalaPull <- function(reference, method, ...) {
  if ( ! is.scalaReference(reference) ) stop("An rscala reference is required.")
  bridge <- scalaFindBridge(reference)
  pullers <- get("pullers",envir=attr(bridge,"details"))
  pullers[[method]](reference, bridge, ...)
}

scalaPull.generic <- function(reference, bridge) {
  type <- scalaType(reference) 
  if ( type == "org.ddahl.rscala.RObject" ) unserialize(reference$x())
  else if ( type == "List[org.ddahl.rscala.RObject]" ) {
    pair <- bridge(arr=reference) ^ '(arr.flatMap(_.x).toArray, arr.scanLeft(1)((sum,y) => sum + y.x.length).toArray)'
    bytes <- pair$"_1"()
    sizes <- pair$"_2"()
    lapply(seq_along(sizes[-1]), function(i) {
      unserialize(bytes[sizes[i]:(sizes[i+1]-1)])
    }) 
  } else stop("Reference is not an RObject or a List[RObject].")
}

scalaPull.list <- function(reference, bridge) {
  if ( ! grepl("List\\d+",scalaType(reference)) ) stop("This is not a reference to a list.")
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

scalaPull.arrayOfMatrices <- function(reference, bridge, mode="double") {
  modeInfo <- if ( mode == "double" ) list(double(0),"Double")
  else if ( mode == "integer" ) list(integer(0),"Int")
  else if ( mode == "logical" ) list(logical(0),"Boolean")
  else if ( mode == "character" ) list(character(0),"String")
  else stop("Unsupport 'mode'.")
  modeZero <- modeInfo[[1]]
  modeType <- modeInfo[[2]]
  if ( gsub("<<modeType>>",modeType,"Array[Array[Array[<<modeType>>]]]") != scalaType(reference) ) stop("This is not a reference to an array of matrices (i.e., Array[Array[Array[_]]]).")
  dims <- bridge(reference) * '
    reference.map(X => if ( X == null ) Array(0,0) else Array(X.length,X(0).length))
  '
  data <- bridge(reference) * gsub("<<modeType>>",modeType,'
    reference.flatMap(X => if ( X == null ) Array.ofDim[<<modeType>>](0,0) else X).flatten
  ')
  offset <- 1
  result <- vector(nrow(dims),mode="list")
  for ( i in 1:nrow(dims) ) {
    d <- dims[i,]
    pd <- prod(d)
    dt <- if ( pd == 0 ) modeZero else data[offset:(offset+pd-1)]
    result[[i]] <- matrix(dt,nrow=d[1],byrow=TRUE)
    offset <- offset + pd
  }
  result
}
