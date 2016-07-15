test <- function(a,value,func=list(),method=c("name","$")[1]) {
  if ( method == "name" ) {
    scalaSet(a,"tmp",value)
    tmp <- scalaGet(a,"tmp")
  } else if ( method == "$" ) {
    a$tmp <- value
    tmp <- a$tmp
  } else stop("Unrecognized method.")
  if ( length(func) > 0 ) {
    if ( ! is.list(func) ) func <- list(func)
    for ( f in func ) {
      if ( ! f(tmp) ) return(FALSE)
    }
  }
  if ( class(tmp) != class(value) ) return(FALSE)
  if ( length(tmp) != length(value) ) return(FALSE)
  if ( any(tmp != value) ) return(FALSE)
  TRUE
}

assertAll <- function(interpreter) {
  a <- interpreter
  assert <- function(x) {
    if ( ! x ) stop("Assertion not true.")
  }
  for ( method in c("name","$") ) {
    cat("Testing method:",method,"\n")
    assert(test(a,"Lisa",method=method))
    assert(test(a,c("Lisa","Susan","Grace","Evalyn"),method=method))
    assert(test(a,matrix(c("a","b","c","d","e","f"),nrow=3),method=method))
    assert(test(a,1,method=method))
    assert(test(a,c(1,2,3,4),method=method))
    assert(test(a,matrix(c(1,2,3,4,5,6),nrow=3),method=method))
    assert(test(a,TRUE,method=method))
    assert(test(a,as.integer(c(TRUE,FALSE)),method=method))
    assert(test(a,matrix(c(TRUE,TRUE,FALSE,TRUE,FALSE,TRUE),nrow=3),method=method))
    assert(test(a,1L,method=method))
    assert(test(a,as.integer(c(1,2,3,4)),method=method))
    assert(test(a,matrix(as.integer(c(1,2,3,4,5,6)),nrow=3),method=method))
  }
}

library(rscala)

# General tests
serialize <- as.logical(Sys.getenv("RSCALA_SERIALIZE"))
output <- as.logical(Sys.getenv("RSCALA_OUTPUT"))
version <- Sys.getenv("RSCALA_SCALA_VERSION")
s <- scala(serialize=serialize,stdout=output,stderr=output)
if ( version != s %~% "scala.util.Properties.versionNumberString" ) stop("Version mismatch.")

assertAll(s)


# Tests of static typing in Scala
s$b <- matrix(1L:12L,ncol=3)
s %~% "println(b(1)(2))"
s$b <- matrix(c(FALSE,TRUE),ncol=2)
s %~% "println(b(0)(1))"
s$b <- seq(0,1,length=5)
s %~% "println(b(3))"
s$b <- FALSE
s %~% "println(!b)"

close(s)
rm(s)

