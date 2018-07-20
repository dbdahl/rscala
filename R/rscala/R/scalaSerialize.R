#' Serialization Between R and Scala
#'
#' These functions attempt to i. serialize an R object to Scala or ii. unserialize an rscala reference to R.  A few built serializers and unserializers are provided and more may be added using the functions \code{\link{scalaSerializeRegister}} and \code{\link{scalaUnserializeRegister}}.
#'
#' @param x An R object.
#' @param bridge An rscala bridge.
#' @param verbose Should details of the search for an appropriate serializer or unserializer
#'   be shown?
#' @param ... Other arguments passed to specialized functions.
#'
#' @seealso \code{\link{scalaSerializeRegister}}
#' @export
#' @describeIn scalaSerialize Serialize Object from R to Scala
#' @examples \donttest{
#' scala(assign.name='e')      # Implicitly defines the bridge 'e'.
#' 
#' s(rn=scalaSerialize(rnorm)) * 'R.evalD1("%-(%-)",rn,5)'
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
scalaSerialize <- function(x, bridge=scalaFindBridge(), verbose=FALSE, ...) {
  reference <- NULL
  serializers <- get("serializers",envir=attr(bridge,"details"))
  for ( serializer in serializers ) {
    reference <- serializer(x, bridge, verbose, ...)
    if ( ! is.null(reference) ) break
  }
  if ( is.null(reference) ) stop("No matching serializer was found.")
  reference
}

#' @describeIn scalaSerialize Serialize Arbitrary Object from R to Scala
#'
#' This function serializes an arbitrary R object to an instance of \code{RObject} in Scala.  Since the \code{RObject} merely contains an array of bytes, the \code{RObject} is really only useful as storage for later unserialization.
#' 
#' @param as.is If \code{x} is a list, \code{TRUE} causes the list to serialized
#'   as a single object and \code{FALSE} causes each element of the list to the
#'   serialized individually.
#'
#' @export
#' 
scalaSerialize.generic <- function(x, bridge=scalaFindBridge(), verbose=FALSE, as.is=FALSE) {
  if ( verbose ) cat("scalaSerialize.generic: Trying...\n")
  if ( is.list(x) && ( ! as.is ) ) {
    if ( verbose ) cat("scalaSerialize.generic: List detected.\n")
    if ( verbose ) cat("scalaSerialize.generic: Success.\n")
    bridge(len=length(x)) ^ '
      List.tabulate(len) { i =>
        R.evalObject("x[[%-]]",i+1)
      }
    '
  } else {
    if ( verbose ) cat("scalaSerialize.generic: Nonlist detected.\n")
    if ( verbose ) cat("scalaSerialize.generic: Success.\n")
    bridge$.R.evalObject('x')
  }
}

#' @describeIn scalaSerialize Serialize List from R to Scala
#'
#' @export
#' 
scalaSerialize.list <- function(x, bridge=scalaFindBridge(), verbose=FALSE) {
  if ( verbose ) cat("scalaSerialize.list: Trying...\n")
  if ( ! is.list(x) ) {
    if ( verbose ) cat("scalaSerialize.list: Object is not a list.\n")
    return(NULL)
  }
  uniqueNames <- unique(names(x))
  if ( ( length(uniqueNames) != length(x) ) || ( any(uniqueNames=="") ) ) {
    if ( verbose ) cat(paste0("scalaSerialize.list: All items must be named.\n"))
    return(NULL)   
  }
  if ( any(grepl("\\W",uniqueNames)) ) {
    if ( verbose ) cat(paste0("scalaSerialize.list: The following variable names are problematic: ",paste0(uniqueNames[grepl("\\W",uniqueNames)],collapse=", "),"\n"))
    return(NULL)
  }
  asIs <- lapply(x,function(y) if ( inherits(y,"AsIs") ) "true" else "false")
  types <- lapply(x,function(y) {
    type <- typeof(y) 
    if ( type == "double" ) "Double"
    else if ( type == "integer" ) "Int"
    else if ( type == "logical" ) "Boolean"
    else if ( type == "character" ) "String"
    else {
      if ( verbose ) cat("scalaSerialize.list: Unsupported type.\n")
      return(NULL)
    }
  })
  shapes <- lapply(x,function(y) {
    if ( is.matrix(y) ) c("Array[Array[","]]")
    else {
      forceVector <- inherits(y,"AsIs")
      if ( ( ! forceVector ) && ( length(y) == 1L ) ) c("","")
      else c("Array[","]")
    }
  })
  fullTypes <- lapply(seq_along(types),function(i) paste0(shapes[[i]][1],types[[i]],shapes[[i]][2]))
  names <- names(types)
  rowNameStr <- if ( is.data.frame(x) && ! all(row.names(x) == as.character(seq_len(nrow(x)))) ) {
    paste0("Some(Array(",paste0('"',row.names(x),'"',collapse=","),"))")
  } else "None"
  definition <- paste0("(\n",paste0("  val ",names,": ",fullTypes,collapse=",\n"),"\n) {\n",
                       "  val names = Array(",paste0('"',names,'"',collapse=","),")\n",
                       "  val asIs = Array(",paste0(asIs,collapse=","),")\n",
                       "  val isDataFrame = ",if (is.data.frame(x)) "true" else "false","\n",
                       "  val rowNames: Option[Array[String]] = ",rowNameStr,"\n",
                       "}")
  name <- paste0("List",bridge(x=definition) * 'math.abs(x.hashCode).toString')
  definition <- paste0("class ",name,definition)
  bridge + definition
  f <- eval(parse(text=paste0("bridge$.new_",name)))
  args <- lapply(seq_len(length(x)), function(j) x[[j]])
  reference <- do.call(f,args)
  if ( verbose ) cat("scalaSerialize.list: Success with list or data frame.\n")
  reference
}
