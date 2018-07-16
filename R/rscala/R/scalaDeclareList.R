#' @export
#' 
scalaSerialize <- function(x, bridge, ..., verbose=FALSE) UseMethod("scalaSerialize")

#' @export
#' 
scalaSerialize.data.frame <- function(x, bridge, name=NULL, verbose=FALSE) {
  name <- if ( is.null(name) ) gsub("\\W","_",deparse(substitute(x))) else name
  scalaSerialize.list(x, bridge, name, verbose)
}

#' @export
#' 
scalaSerialize.list <- function(x, bridge, name=NULL, verbose=FALSE) {
  name <- if ( is.null(name) ) gsub("\\W","_",deparse(substitute(x))) else name
  l <- x
  names(l) <- gsub("\\.","_",names(l))  
  asIs <- lapply(l,function(y) if ( inherits(y,"AsIs") ) "true" else "false")
  types <- lapply(l,function(y) {
    type <- typeof(y) 
    if ( type == "double" ) "Double"
    else if ( type == "integer" ) "Int"
    else if ( type == "logical" ) "Boolean"
    else if ( type == "character" ) "String"
    else stop("Unsupported type.")
  })
  shapes <- lapply(l,function(y) {
    if ( is.matrix(y) ) c("Array[Array[","]]")
    else {
      forceVector <- inherits(y,"AsIs")
      if ( ( ! forceVector ) && ( length(y) == 1L ) ) c("","")
      else c("Array[","]")
    }
  })
  fullTypes <- lapply(seq_along(types),function(i) paste0(shapes[[i]][1],types[[i]],shapes[[i]][2]))
  names <- names(types)
  rowNameStr <- if ( is.data.frame(l) && ! all(row.names(l) == as.character(seq_len(nrow(l)))) ) {
    paste0("Some(Array(",paste0('"',row.names(l),'"',collapse=","),"))")
  } else "None"
  definition <- paste0("class ",name,"(\n",paste0("  val ",names,": ",fullTypes,collapse=",\n"),"\n) {\n",
                       "  val names = Array(",paste0('"',names,'"',collapse=","),")\n",
                       "  val namesOriginal = Array(",paste0('"',names(x),'"',collapse=","),")\n",
                       "  val asIs = Array(",paste0(asIs,collapse=","),")\n",
                       "  val isDataFrame = ",if (is.data.frame(l)) "true" else "false","\n",
                       "  val rowNames: Option[Array[String]] = ",rowNameStr,"\n",
                       "}")
  declarationsCache <- get("declarationsCache",envir=attr(bridge,"details"))
  key <- paste0("scalaSerializeList.",name)
  if ( verbose ) {
    cat("Generated code:\n")
    cat(definition,"\n")
  }
  if ( ! exists(key,envir=declarationsCache) ) {
    bridge + definition
    assign(key,TRUE,envir=declarationsCache)
  }
  f <- eval(parse(text=paste0("bridge$.new_",name)))
  args <- lapply(seq_len(length(l)), function(j) l[[j]])
  reference <- do.call(f,args)
  env <- attr(reference,"rscalaReferenceEnvironment")
  assign("original",x,envir=env)
  assign("unserializer",scalaUnserialize.list,envir=env)
  reference
}

#' @export
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

#' @export
#' 
scalaUnserialize.list <- function(reference) {
  names <- reference$names()
  namesOriginal <- reference$namesOriginal()
  asIs <- reference$asIs()
  l <- lapply(seq_along(names),function(i) {
    x <- eval(parse(text=paste0("reference$",names[i],"()")))
    if ( asIs[i] ) I(x) else x
  })
  names(l) <- namesOriginal
  if ( ! reference$isDataFrame() ) l
  else {
    rowNamesOptions <- reference$rowNames()
    rowNames <- if ( rowNamesOptions$isDefined() ) rowNamesOptions$get() else NULL
    as.data.frame(l,row.names=rowNames,stringsAsFactors=FALSE)
  }
}
