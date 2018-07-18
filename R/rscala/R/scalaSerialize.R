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
scalaSerialize <- function(x, bridge=scalaFindBridge(), ...) {
  UseMethod("scalaSerialize")
}

#' @describeIn scalaSerialize Serialize Data Frame from R to Scala
#' @export
#' 
scalaSerialize.data.frame <- function(x, bridge=scalaFindBridge(), name=NULL, ...) {
  name <- if ( is.null(name) ) gsub("\\W","_",deparse(substitute(x))) else name
  scalaSerialize.list(x, bridge, name)
}

#' @describeIn scalaSerialize Serialize List from R to Scala
#' @export
#' 
scalaSerialize.list <- function(x, bridge=scalaFindBridge(), name=NULL, ...) {
  if ( any(grepl("\\W",names(x))) ) {
    stop(paste0("The following variable names are problematic: ",paste0(names(x)[grepl("\\W",names(x))],collapse=", "),"\n"))
  }
  asIs <- lapply(x,function(y) if ( inherits(y,"AsIs") ) "true" else "false")
  types <- lapply(x,function(y) {
    type <- typeof(y) 
    if ( type == "double" ) "Double"
    else if ( type == "integer" ) "Int"
    else if ( type == "logical" ) "Boolean"
    else if ( type == "character" ) "String"
    else stop("Unsupported type.")
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
  env <- attr(reference,"rscalaReferenceEnvironment")
  assign("original",x,envir=env)
  assign("unserializer",scalaUnserialize.list,envir=env)
  reference
}
