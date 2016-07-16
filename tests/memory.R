library(rscala)

serialize <- as.logical(Sys.getenv("RSCALA_SERIALIZE"))
output <- as.logical(Sys.getenv("RSCALA_OUTPUT"))
version <- Sys.getenv("RSCALA_SCALA_VERSION")

s <- scala(heap.max="64M",serialize=serialize,stdout=output,stderr=output)
if ( version != s %~% "scala.util.Properties.versionNumberString" ) stop("Version mismatch.")
tryCatch(a <- tryCatch(s$do("Array[Double]")$new(20000000L),error=function(e) e),error=function(e) e)
close(s)
cat("----\n")

s <- scala(heap.max="256M",serialize=serialize,stdout=output,stderr=output)
if ( version != s %~% "scala.util.Properties.versionNumberString" ) stop("Version mismatch.")
tryCatch(a <- s$do("Array[Double]")$new(20000000L),error=function(e) e)
close(s)
cat("----\n")

options(rscala.heap.maximum="64M")
s <- scala(serialize=serialize,stdout=output,stderr=output)
if ( version != s %~% "scala.util.Properties.versionNumberString" ) stop("Version mismatch.")
tryCatch(a <- s$do("Array[Double]")$new(20000000L),error=function(e) e)
close(s)
cat("----\n")

options(rscala.heap.maximum="96M")
s <- scala(serialize=serialize,stdout=output,stderr=output)
if ( version != s %~% "scala.util.Properties.versionNumberString" ) stop("Version mismatch.")
for ( i in 1:2000 ) {
  a <- s$do("Array[Double]")$new(100000L)  # No memory problems
  # s %~% "new Array[Double](100000)"      # Memory problems
}



