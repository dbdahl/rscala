lEtTeRs <- c(letters,LETTERS)
alphabet <- c(lEtTeRs,0:9)

uniqueName <- function(value, envir, prefix, inherits=TRUE, length=8) {
  while (TRUE) {
    name <- paste0(prefix,sample(lEtTeRs,1),paste0(sample(alphabet,length-1,replace=TRUE),collapse=""))
    if ( ! exists(name,envir=envir,inherits=inherits) ) {
      assign(name,value,envir=envir,inherits=FALSE)
      return(name)
    }
  }
}

findScalaInstance <- function() {
  counter <- 2
  while ( TRUE ) {
    frame <- parent.frame(counter)
    w <- which(sapply(ls(envir=frame),function(x) class(get(x,envir=frame)))=="ScalaInterpreter")
    if ( length(w) == 1 ) return(get(names(w),envir=frame))
    if ( length(w) >  1 ) stop("Multiple Scala instances were found in the same environment.")
    if ( identical(frame,.GlobalEnv) ) stop("Cannot find a Scala instance.")
    counter <- counter + 1
  }
}

