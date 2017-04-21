source("common.R",print.eval=TRUE)


test <- function(a,value,func=list()) {
  a$tmp <- value
  tmp <- a$tmp
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
  assert(test(a,"Lisa"))
  assert(test(a,c("Lisa","Susan","Grace","Evalyn")))
  assert(test(a,matrix(c("a","b","c","d","e","f"),nrow=3)))
  assert(test(a,1))
  assert(test(a,c(1,2,3,4)))
  assert(test(a,matrix(c(1,2,3,4,5,6),nrow=3)))
  assert(test(a,TRUE))
  assert(test(a,as.integer(c(TRUE,FALSE))))
  assert(test(a,matrix(c(TRUE,TRUE,FALSE,TRUE,FALSE,TRUE),nrow=3)))
  assert(test(a,1L))
  assert(test(a,as.integer(c(1,2,3,4))))
  assert(test(a,matrix(as.integer(c(1,2,3,4,5,6)),nrow=3)))
}


assertAll(s)


# Tests of static typing in Scala
s$b <- matrix(1L:12L,ncol=3)
s %~% "println(b(1)(2))"
s$b <- matrix(c(FALSE,TRUE),ncol=2)
s %~% "println(b(0)(1))"
s$b <- seq(0,1,length=5)
s %~% "println(b(3))"
s$b <- I(FALSE)
s %~% "println(!b)"

# What about when there is no result?
tryCatch(s %~% 'import scala.util._', error=function(e) e)
s %~% "5+4"
s %@% 'import scala.util._'

tryCatch(s %.~% 'import scala.util._', error=function(e) e)
s %~% "3+4"
s %@% 'import scala.util._'

# Close
close(s)
rm(s)

