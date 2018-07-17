#' @export
#' 
scalaSerialize <- function(x, bridge, ...) UseMethod("scalaSerialize")

#' @export
#' 
scalaSerialize.data.frame <- function(x, bridge, name=NULL) {
  name <- if ( is.null(name) ) gsub("\\W","_",deparse(substitute(x))) else name
  scalaSerialize.list(x, bridge, name)
}

#' @export
#' 
scalaSerialize.list <- function(x, bridge, name=NULL) {
  name <- if ( is.null(name) ) gsub("\\W","_",deparse(substitute(x))) else name
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
  definition <- paste0("class ",name,"(\n",paste0("  val ",names,": ",fullTypes,collapse=",\n"),"\n) {\n",
                       "  val names = Array(",paste0('"',names,'"',collapse=","),")\n",
                       "  val asIs = Array(",paste0(asIs,collapse=","),")\n",
                       "  val isDataFrame = ",if (is.data.frame(x)) "true" else "false","\n",
                       "  val rowNames: Option[Array[String]] = ",rowNameStr,"\n",
                       "}")
  bridge + definition
  f <- eval(parse(text=paste0("bridge$.new_",name)))
  args <- lapply(seq_len(length(x)), function(j) x[[j]])
  reference <- do.call(f,args)
  env <- attr(reference,"rscalaReferenceEnvironment")
  assign("original",x,envir=env)
  assign("unserializer",scalaUnserialize.list,envir=env)
  reference
}
