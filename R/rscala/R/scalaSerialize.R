#' Serialize Object from R to Scala
#'
#' @param x An R object.
#' @param bridge An rscala bridge.
#' @param name Name of the Scala type which may or may not be used by specialize functions.
#' @param ... Other arguments passed to the function for the type of \code{x}.
#'
#' @seealso \code{\link{scalaFindBridge}}
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
scalaSerialize <- function(x, bridge=scalaFindBridge(), verbose=FALSE, ...) {
  reference <- NULL
  serializers <- get("serializers",env=attr(bridge,"details"))
  for ( serializer in serializers ) {
    reference <- serializer(x, bridge, verbose, ...)
    if ( ! is.null(reference) ) break
  }
  if ( is.null(reference) ) stop("No matching serializer was found.")
  reference
}

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
#' @export
#' 
scalaSerialize.list <- function(x, bridge=scalaFindBridge(), verbose=FALSE, name=NULL) {
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
  name <- if ( ! is.null(name) ) name else paste0("List",bridge(x=definition) * 'math.abs(x.hashCode).toString')
  definition <- paste0("class ",name,definition)
  bridge + definition
  f <- eval(parse(text=paste0("bridge$.new_",name)))
  args <- lapply(seq_len(length(x)), function(j) x[[j]])
  reference <- do.call(f,args)
  if ( verbose ) cat("scalaSerialize.list: Success.\n")
  reference
}
