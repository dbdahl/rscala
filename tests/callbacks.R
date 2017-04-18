source("common.R",print.eval=TRUE)

assert <- function(xx) {
  if ( ! identical(( s %~% 'R.get("xx")._1' ), xx) ) stop("Not identical (test 1)")
  if ( ! identical(( s %~% 'R.xx._1' ), xx) ) stop("Not identical (test 2)")
  s$xx <- xx
  s %@% 'R.a = xx'
  if ( ! identical(a, xx) ) stop("Not identical (test 3)")
  if ( ! identical(s$.R$get(I("xx"))$"_1"(), xx) ) stop("Not identical (test 4)")
  m <- s$def() %~% 'R.get("xx")._1'
  if ( ! identical(m(), xx) ) stop("Not identical (test 5)")
}

y <- c(0,1,2,3,4,5,6,8)
for ( x in list(as.integer(y),as.double(y),as.logical(y),as.character(y),as.raw(y)) ) {
  assert(x[1])
  assert(x[2])
  assert(x)
  assert(matrix(x,nrow=1))
  assert(matrix(x,nrow=2))
  assert(matrix(x,nrow=4))
}

counter <- 0
for ( i in 1:10 ) {
  s %~% 'R.eval("counter <- counter + 1")'
}
if ( counter != 10 ) stop("Counter is off.")
for ( i in 1:10 ) {
  s$.R$eval(I("counter <<- counter - 1"))
}
if ( counter != 0 ) stop("Counter is off.")


# Should be a compile-time error because 'ewf' is not defined.
tryCatch(s %~% '
  3+4+ewf
  R.eval("""
    cat("I love Lisa!\n")
    a <- "3+9"
  """)
',error=function(e) e)
s %~% '3+2'


# Should be an R evaluation error because 'asfd' is not defined and out of place.
tryCatch(s %~% '
  3+4
  R.eval("""
    cat("I love Lisa!\n")
    a <- "3+9" asfd
  """)
',error=function(e) e)
s %~% '3+6'


myMean <- function(x) {
  cat("Here is am.\n")
  mean(x)
}

callRFunction <- s$def(functionName=I(""), x=numeric()) %~% '
  R.xx = x
  R.eval("y <- "+functionName+"(xx)")
  R.y._1
'

tryCatch(callRFunction(1:100),error = function(e) {})
callRFunction('myMean',1:100)

# Should be an R evaluation error because 'asfd' is not a package.
tryCatch(s %@% 'R.eval("library(asdf)")',error=function(e) e)
s %~% 'R.evalD0("3+4")'

# Note that callbacks can be infinitely deep.
s %~% "3+4"
s %~% 'R.evalD0("""s %~% "2+3"""")'
s %~% "3+4"


