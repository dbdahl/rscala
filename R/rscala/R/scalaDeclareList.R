#' @export
#' 
scalaDeclareList <- function(bridge, list, name, andConvert=TRUE, verbose=TRUE) {
  l <- list
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
  rowNameStr <- if ( is.data.frame(list) && ! all(row.names(list) == as.character(seq_len(nrow(list)))) ) {
    paste0("Some(Array(",paste0('"',row.names(list),'"',collapse=","),"))")
  } else "None"
  definition <- paste0("class ",name,"(\n",paste0("  val ",names,": ",fullTypes,collapse=",\n"),"\n) {\n",
                       "  val names = Array(",paste0('"',names,'"',collapse=","),")\n",
                       "  val namesOriginal = Array(",paste0('"',names(list),'"',collapse=","),")\n",
                       "  val asIs = Array(",paste0(asIs,collapse=","),")\n",
                       "  val isDataFrame = ",if (is.data.frame(list)) "true" else "false","\n",
                       "  val rowNames: Option[Array[String]] = ",rowNameStr,"\n",
                       "}")
  if ( verbose ) {
    cat("Generated code:\n")
    cat(definition,"\n")
  }
  bridge + definition
  if ( andConvert ) scalaSerializeList(bridge, list, name) else invisible()
}

#' @export
#' 
scalaSerializeList <- function(bridge, list, name) {
  f <- eval(parse(text=paste0("bridge$.new_",name)))
  args <- lapply(seq_len(length(list)), function(j) list[[j]])
  do.call(f,args)
}

#' @export
#' 
scalaUnserializeList <- function(reference) {
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
