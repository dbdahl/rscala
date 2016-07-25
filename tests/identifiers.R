library(rscala)

serialize <- as.logical(Sys.getenv("RSCALA_SERIALIZE"))
output <- as.logical(Sys.getenv("RSCALA_OUTPUT"))
version <- Sys.getenv("RSCALA_SCALA_VERSION")
s <- scala(serialize=serialize,stdout=output,stderr=output)
if ( version != s %~% "scala.util.Properties.versionNumberString" ) stop("Version mismatch.")
set.seed(3432585)

s$.asdf <- 3
s %~% "3+4"

tryCatch(s$adf,error = function(e) e)

tryCatch(scalaGet(s,"def"),error = function(e) e)
s$def <- 4
s %~% "3+4"

f <- s$ def("x: Int, y: Double",'
  x+y
')
f(3,4)

x <- as.character(0:8)
set.seed(234235)
s$x <- rnorm(100)
tryCatch(s$val(x),error = function(e) e)
identical(s$x,s$.val("x"))

s %~% "val $bob = 4"
scalaGet(s,"$bob")
s$'$bob'

