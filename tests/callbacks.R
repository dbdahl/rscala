library(rscala)

serialize <- as.logical(Sys.getenv("RSCALA_SERIALIZE"))
output <- as.logical(Sys.getenv("RSCALA_OUTPUT"))
version <- Sys.getenv("RSCALA_SCALA_VERSION")
s <- scala(serialize=serialize,stdout=output,stderr=output)
actualVersion <- s %~% "scala.util.Properties.versionNumberString"
if ( version != s %~% "scala.util.Properties.versionNumberString" ) {
  cat("Requested version: ",version,"\n")
  cat("Actual version:    ",actualVersion,"\n")
  stop("Version mismatch.")
}

assert <- function(x) {
  if ( ! identical(( s %~% 'R.get("x")._1' ), x) ) stop("Not identical (test 1)")
  if ( ! identical(( s %~% 'R.x._1' ), x) ) stop("Not identical (test 2)")
  s$x <- x
  s %@% 'R.a = x'
  if ( ! identical(a, x) ) stop("Not identical (test 3)")
  if ( ! identical(s$.R$get(scalaPrimitive("x"))$"_1"(), x) ) stop("Not identical (test 4)")
  m <- s$def2() %~% 'R.get("x")._1'
  if ( ! identical(m(), x) ) stop("Not identical (test 5)")
}

y <- c(0,1,2,3,4,5,6,8)
for ( x in list(as.integer(y),as.double(y),as.logical(y),as.character(y)) ) {
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
  s$.R$eval(scalaPrimitive("counter <<- counter - 1"))
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

callRFunction <- s$def2(functionName=scalaScalar(""), x=numeric()) %~% '
  R.xx = x
  R.eval("y <- "+functionName+"(xx)",false)
'

tryCatch(callRFunction(1:100),error = function(e) {})
callRFunction('myMean',1:100)

# Should be an R evaluation error because 'asfd' is not a package.
tryCatch(scalaEval(s,'R.eval("library(asdf)")',environment(),FALSE),error=function(e) e)
s %~% 'R.evalD0("3+4")'

# Note that callbacks can only be one-level deep.
s %~% "3+4"
s %~% 'R.eval("""s %~% "2+3"""")'
s %~% "3+4"


