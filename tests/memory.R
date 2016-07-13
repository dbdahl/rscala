library(rscala)

serialize <- as.logical(Sys.getenv("RSCALA_SERIALIZE"))
cat(serialize,"\n")

s <- scala(heap.max="64M")
s %~% "util.Properties.versionNumberString"
a <- tryCatch(s$do("Array[Double]")$new(10000000L),error=function(e) e)
close(s)
cat("----\n")

s <- scala(heap.max="256M")
s %~% "util.Properties.versionNumberString"
a <- s$do("Array[Double]")$new(10000000L)
close(s)
cat("----\n")

options(rscala.heap.maximum="64M")
s <- scala()
s %~% "util.Properties.versionNumberString"
a <- s$do("Array[Double]")$new(10000000L)
close(s)
cat("----\n")



options(rscala.heap.maximum="96M")
s <- scala()
s %~% "util.Properties.versionNumberString"
for ( i in 1:2000 ) {
  a <- s$do("Array[Double]")$new(100000L)  # No memory problems
  # s %~% "new Array[Double](100000)"      # Memory problems
}



