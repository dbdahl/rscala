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

scalaDeclareDataFrame <- function(bridge, data.frame, name, andConvert=TRUE, verbose=TRUE) {
  df <- data.frame
  names(df) <- sanitize(names(df))
  types <- types(df)
  names <- names(types)
  definition <- paste0("class ",name,"(\n",paste0("  val ",names,": Array[",types,"]",collapse=",\n"),"\n)")
  if ( verbose ) {
    cat("Generated code:\n")
    cat(definition,"\n")
  }
  bridge + definition
  if ( andConvert ) {
    scalaConvertDataFrame(bridge, data.frame, name)
  } else invisible()
}

scalaConvertDataFrame <- function(bridge, data.frame, name) {
  f <- eval(parse(text=paste0("bridge$.new_",name)))
  args <- lapply(seq_len(ncol(data.frame)), function(j) data.frame[,j])
  do.call(f,args)
}

a <- scalaDeclareDataFrame(s, iris[,-5], "Iris")
a <- scalaConvertDataFrame(s, iris[,-5], "Iris")

a <- scalaDeclareDataFrame(s, mtcars, "MTCars")
a <- scalaConvertDataFrame(s, mtcars, "MTCars")




scalaDeclareList <- function(bridge, list, name, andConvert=TRUE, verbose=TRUE) {
  l <- list
  names(l) <- sanitize(names(l))
  types <- types(l)
  shapes <- shapes(l)
  fullTypes <- lapply(seq_along(types),function(i) paste0(shapes[[i]][1],types[[i]],shapes[[i]][2]))
  names <- names(types)
  definition <- paste0("class ",name,"(\n",paste0("  val ",names,": ",fullTypes,collapse=",\n"),"\n)")
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

myList <- list(a=1, b=c(TRUE,FALSE), c=I(3.0))
scalaDeclareList(s, myList, "MyList", andConvert=FALSE)
a <- scalaConvertList(s, myList, "MyList")
a$a()

scalaDeclareList(s, iris[,-5], "IrisAsList", andConvert=FALSE)
a <- scalaConvertList(s, iris[,-5], "IrisAsList")







big <- rbind(iris[,-5])
big <- rbind(big,big,big,big)
big <- rbind(big,big,big,big)
big <- rbind(big,big,big,big)
big <- rbind(big,big,big,big)
big <- rbind(big,big,big,big)

a <- scalaConvertDataFrame(s, big, "Iris")
  
a$Sepal_Length()
a$Sepal_Width()
a$Petal_Width()



s$showCode <- TRUE
big <- rbind(iris[,-5])
big <- rbind(big,big,big,big)
big <- rbind(big,big,big,big)
big <- rbind(big,big,big,big)
big <- rbind(big,big,big,big)
nrow(big)

a <- df2scala(s,big,"Iris",define=FALSE)
a(4L)$toString()

a[[5]]$toString()
a[[1]]$Sepal_Width()
a[[150]]$Sepal_Length()

sapply(seq_along(a), function(i) a[[i]]$Sepal_Length())


names <- names(iris)
types <- lapply(iris,typeof)

a <- s + '
  case class Iris(
    SepalLength: Option[Double],
    SepalWidth: Option[Double],
    PetalLength: Option[Double],
    PetalWidth: Option[Double],
    Species: String
  )
'

a <- s ^ '
  MTCars(1.2,2.3,3.4,22,"Iris")
'
