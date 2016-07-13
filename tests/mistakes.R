library(rscala)

serialize <- as.logical(Sys.getenv("RSCALA_SERIALIZE"))
cat(serialize,"\n")
s <- scala(serialize=serialize)

s %~% "scala.util.Properties.versionNumberString"

a <- list(a=3,b=4)
tryCatch(s$j <- a,error=function(e) {'Caught'})
s$.j <- a
identical(a,scalaUnwrap(s,s$.j))
s$j
s$jj <- unlist(a)

tryCatch(s$j <- complex(3,4,5,32),error=function(e) {'Caught'})

s %~% "null"

s %~% "
  val a = 3
  val b = 5
  a ++ b
"

s %~% "
  val a = 3
  val b = 5
  (a,b)
"

s %.~% "
  val a = 3
  val b = 5
  (a,b)
"

tryCatch(s$xaa,error = function(e) {'Caught'})
s$xaa <- NULL
tryCatch(s$xaa,error = function(e) {'Caught'})

s$xaa <- 3
s$xaa
s$.xaa

withNA <- c(1,2,3,NA,4)
s$withNA <- withNA

withNaN <- c(1,2,3,NaN,4)
s$withNaN <- withNaN

identical(  withNA,  withNaN)
identical(s$withNA,s$withNaN)

withMax <- c(1,.Machine$double.xmax,.Machine$double.xmin)
s$withMax <- withMax
identical(withMax,s$withMax)

withMax <- c(1L,.Machine$integer.max)
s$withMax <- withMax
identical(withMax,s$withMax)

