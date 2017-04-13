source("common.R",print.eval=TRUE)

if ( substr(Sys.getenv("RSCALA_SCALA_VERSION"),1,4) != "2.10" ) {
  a <- s %.~% "null"
  print(a)
}

a <- s %~% "null"
a

a <- list(a=3,b=4)
tryCatch(s$j <- a,error=function(e) e)
s$jj <- unlist(a)
s$jj

tryCatch(s$j <- complex(3,4,5,32),error=function(e) e)

tryCatch(s %~% "
  val a = 3
  val b = 5
  a ++ b
",error=function(e) e)

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

tryCatch(s$xaa,error = function(e) e)
s$xaa <- NULL
s$xaa

s$xaa <- I(3)
s$xaa
s$.val("xaa")

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

