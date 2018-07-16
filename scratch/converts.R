library(rscala)
scala()

sanitize <- function(names) {
  gsub("\\.","_",names)  
}

types <- function(x) {
  lapply(x,function(y) {
    type <- typeof(y) 
    if ( type == "double" ) "Double"
    else if ( type == "integer" ) "Int"
    else if ( type == "logical" ) "Boolean"
    else if ( type == "character" ) "String"
    else stop("Unsupported type.")
  })
}

shapes <- function(x) {
  lapply(x,function(y) {
    if ( is.matrix(y) ) c("Array[Array[","]]")
    else {
      forceVector <- inherits(y,"AsIs")
      if ( ( ! forceVector ) && ( length(y) == 1L ) ) c("","")
      else c("Array[","]")
    }
  })
}

# scalaDeclareDataFrame <- function(bridge, data.frame, name, andConvert=TRUE, verbose=TRUE) {
#   df <- data.frame
#   names(df) <- sanitize(names(df))
#   types <- types(df)
#   names <- names(types)
#   definition <- paste0("class ",name,"(\n",paste0("  val ",names,": Array[",types,"]",collapse=",\n"),"\n)")
#   if ( verbose ) {
#     cat("Generated code:\n")
#     cat(definition,"\n")
#   }
#   bridge + definition
#   if ( andConvert ) {
#     scalaConvertDataFrame(bridge, data.frame, name)
#   } else invisible()
# }
# 
# scalaConvertDataFrame <- function(bridge, data.frame, name) {
#   f <- eval(parse(text=paste0("bridge$.new_",name)))
#   args <- lapply(seq_len(ncol(data.frame)), function(j) data.frame[,j])
#   do.call(f,args)
# }
# 
# a <- scalaDeclareDataFrame(s, iris[,-5], "Iris")
# a <- scalaConvertDataFrame(s, iris[,-5], "Iris")
# 
# a <- scalaDeclareDataFrame(s, mtcars, "MTCars")
# a <- scalaConvertDataFrame(s, mtcars, "MTCars")



scalaDeclareList <- function(bridge, list, name, andConvert=TRUE, verbose=TRUE) {
  l <- list
  names(l) <- sanitize(names(l))
  types <- types(l)
  shapes <- shapes(l)
  fullTypes <- lapply(seq_along(types),function(i) paste0(shapes[[i]][1],types[[i]],shapes[[i]][2]))
  names <- names(types)
  definition <- paste0("class ",name,"(\n",paste0("  val ",names,": ",fullTypes,collapse=",\n"),"\n) extends org.ddahl.rscala.RList {\n",
                       "  val names = Array(",paste0('"',names,'"',collapse=","),")\n",
                       "  val namesOriginal = Array(",paste0('"',names(list),'"',collapse=","),")\n",
                       "  val isDataFrame = ",if (is.data.frame(list)) "true" else "false","\n",
                       "}")
  if ( verbose ) {
    cat("Generated code:\n")
    cat(definition,"\n")
  }
  bridge + definition
  if ( andConvert ) {
    scalaConvertList(bridge, list, name)
  } else invisible()
}

scalaConvertList <- function(bridge, list, name) {
  f <- eval(parse(text=paste0("bridge$.new_",name)))
  args <- lapply(seq_len(length(list)), function(j) list[[j]])
  do.call(f,args)
}

scalaUnconvertList <- function(reference) {
  names <- reference$names()
  namesOriginal <- reference$namesOriginal()
  l <- lapply(seq_along(names),function(i) {
    eval(parse(text=paste0("reference$",names[i],"()")))
  })
  names(l) <- namesOriginal
  if ( reference$isDataFrame() ) as.data.frame(l) else l
}

myList <- list(a=1, b=c(TRUE,FALSE), c=I(3.0))
scalaDeclareList(s, myList, "MyList", andConvert=FALSE)
a <- scalaConvertList(s, myList, "MyList")
a$a()
a$length()
identical(scalaUnconvertList(a),myList)

scalaDeclareList(s, iris[,-5], "IrisAsList", andConvert=FALSE)
a <- scalaConvertList(s, iris[,-5], "IrisAsList")
a$length()
a$isDataFrame()
a$nrow()
a$Sepal_Length()
identical(scalaUnconvertList(a),iris[,-5])




big <- rbind(iris[,-5])
big <- rbind(big,big,big,big)
big <- rbind(big,big,big,big)
big <- rbind(big,big,big,big)
big <- rbind(big,big,big,big)
big <- rbind(big,big,big,big)
a <- scalaConvertList(s, big, "IrisAsList")



