#' Push and Pull Objects Between R and Scala
#'
#' The push function serializes an R object to Scala and the push function does
#' the opposite. A couple of built push and pull methods are provided, namely
#' \code{"generic"} and \code{"list"}. The \code{"generic"} method serializes an
#' arbitrary R object to an instance of \code{RObject} in Scala. Since the
#' \code{RObject} merely contains an array of bytes, the \code{RObject} is
#' really only useful as storage for later unserialization. The \code{"generic"}
#' method has an optional \code{as.is} argument which is either \code{TRUE} to
#' cause the list to serialized as a single object or \code{FALSE} to cause each
#' element of the list to the serialized individually. More methods may be added
#' using the functions \code{\link{scalaPushRegister}} and
#' \code{\link{scalaPullRegister}}.
#'
#' @param x An R object.
#' @param method A string giving the specific 'push' or 'pull' method to use.
#' @param bridge An rscala bridge.
#' @param ... Other arguments passed to specialized push and pull functions.
#'
#' @seealso \code{\link{scalaPushRegister}}, \code{\link{scalaPullRegister}}
#' @export
#' @examples \donttest{
#' s <- scala()
#'
#' s(rn=scalaPush(rnorm,"generic"),n=5) * 'R.evalD1("%-(%-)",rn,n)'
#'
#' mtcarsRef <- scalaPush(mtcars, "list")
#' mtcarsRef$names()
#' mtcarsRef$mpg()
#' mtcars2 <- scalaPull(mtcarsRef, "list")
#' identical(mtcars, mtcars2)
#'
#' # Oops, the variable names are bad...
#' tryCatch(ref <- scalaPush(iris, "list"), error=function(e) e)
#'
#' # ... so let's clean up the variable names.
#' irisCleaned <- iris
#' names(irisCleaned) <- gsub("\\W","_",names(iris))
#' irisCleaned$Species <- as.character(iris$Species)
#' ref2 <- scalaPush(irisCleaned, "list")
#' scalaType(ref2)
#' ref2$Sepal_Length()
#' irisCleaned2 <- scalaPull(ref2,"list")
#' identical(irisCleaned, irisCleaned2)
#'
#' close(s)
#' }
#'  
scalaPush <- function(x, method, bridge=scalaFindBridge(), ...) {
  if ( missing(method) ) stop("'method' for push must be supplied.")
  pushers <- get("pushers",envir=attr(bridge,"details"))
  pushers[[method]](x, bridge, ...)
}

scalaPush.generic <- function(x, bridge, as.is=FALSE) {
  if ( is.list(x) && ( ! as.is ) ) {
    bridge(len=length(x)) ^ '
      List.tabulate(len) { i =>
        R.evalObject("x[[%-]]",i+1)
      }
    '
  } else {
    bridge$.R.evalObject('x')
  }
}

scalaPush.list <- function(x, bridge) {
  if ( ! is.list(x) ) stop("Object is not a list.")
  uniqueNames <- unique(names(x))
  if ( ( length(uniqueNames) != length(x) ) || ( any(uniqueNames=="") ) ) stop("All items must be named.")
  if ( any(grepl("\\W",uniqueNames)) ) {
    stop(paste0("The following variable names are problematic: ",paste0(uniqueNames[grepl("\\W",uniqueNames)],collapse=", ")))
  }
  asIs <- lapply(x,function(y) if ( inherits(y,"AsIs") ) "true" else "false")
  types <- lapply(x,function(y) {
    type <- typeof(y) 
    if ( type == "double" ) "Double"
    else if ( type == "integer" ) "Int"
    else if ( type == "logical" ) "Boolean"
    else if ( type == "character" ) "String"
    else stop(paste0("Unsupported type: ",type))
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
  reference
}
