library(rscala)

serialize <- as.logical(Sys.getenv("RSCALA_SERIALIZE"))
output <- as.logical(Sys.getenv("RSCALA_OUTPUT"))
version <- Sys.getenv("RSCALA_SCALA_VERSION")
s <- scala(serialize=serialize,stdout=output,stderr=output)
if ( version != s %~% "scala.util.Properties.versionNumberString" ) stop("Version mismatch.")

setget <- function(value,method="$",...) {
  if ( method == "$" ) {
    s$tt <- value
    value2 <- s$tt
  } else {
    scalaSet(s,"tt",value,...,workspace=environment())
    value2 <- scalaGet(s,"tt",FALSE,environment())
  }
  if ( length(value) != length(value2) ) stop(paste("Value not equal:\n",paste(value,collapse=","),"\n",paste(value2,collapse=",")))
  if ( any(value != value2) ) stop(paste("Value not equal:\n",paste(value,collapse=","),"\n",paste(value2,collapse=",")))
  if ( class(value) != class(value2) ) stop(paste("Class not equal:\n",class(value),"\n",class(value2)))
  if ( mode(value) != mode(value2) ) stop(paste("Mode not equal:\n",mode(value),"\n",mode(value2)))
}

for ( method in c("$","") ) {

  setget(integer(0),method=method)

  setget(double(0),method=method)
  setget(logical(0),method=method)
  setget(character(0),method=method)

  setget(4L,length.one.as.vector=TRUE,method=method)
  setget(5,length.one.as.vector=TRUE,method=method)
  setget(TRUE,length.one.as.vector=TRUE,method=method)
  setget(FALSE,length.one.as.vector=TRUE,method=method)
  setget("David",length.one.as.vector=TRUE,method=method)

  setget(4L,length.one.as.vector=FALSE,method=method)
  setget(5,length.one.as.vector=FALSE,method=method)
  setget(TRUE,length.one.as.vector=FALSE,method=method)
  setget(FALSE,length.one.as.vector=FALSE,method=method)
  setget("David",length.one.as.vector=FALSE,method=method)

  setget(c(4L,3L),length.one.as.vector=FALSE,method=method)
  setget(c(5,6),length.one.as.vector=FALSE,method=method)
  setget(c(TRUE,FALSE),length.one.as.vector=FALSE,method=method)
  setget(c("David","Dahl"),length.one.as.vector=FALSE,method=method)

  setget(matrix(c(1L,2L,3L,4L,5L,6L),nrow=1),length.one.as.vector=FALSE,method=method)
  setget(matrix(c(1,2,3,4,5,6,7,8),nrow=1),length.one.as.vector=FALSE,method=method)
  setget(matrix(c(TRUE,FALSE,TRUE,TRUE,FALSE,FALSE),nrow=1),length.one.as.vector=FALSE,method=method)
  setget(matrix(c("1","2","3","4","5","6","7","8"),nrow=1),length.one.as.vector=FALSE,method=method)

  setget(matrix(c(1L,2L,3L,4L,5L,6L),nrow=2),length.one.as.vector=FALSE,method=method)
  setget(matrix(c(1,2,3,4,5,6,7,8),nrow=2),length.one.as.vector=FALSE,method=method)
  setget(matrix(c(TRUE,FALSE,TRUE,TRUE,FALSE,FALSE),nrow=2),length.one.as.vector=FALSE,method=method)
  setget(matrix(c("1","2","3","4","5","6","7","8"),nrow=2),length.one.as.vector=FALSE,method=method)

  a <- matrix(1:6,nrow=2)
  setget(a[,-c(1,2,3)],length.one.as.vector=FALSE,method=method)
  setget(a[-1,],length.one.as.vector=FALSE,method=method)
  setget(a[-2,-c(1,2,3)],length.one.as.vector=FALSE,method=method)

  mode(a) <- "character"
  setget(a[,-c(1,2,3)],length.one.as.vector=FALSE,method=method)
  setget(a[-1,],length.one.as.vector=FALSE,method=method)
  setget(a[-2,-c(1,2,3)],length.one.as.vector=FALSE,method=method)

}

