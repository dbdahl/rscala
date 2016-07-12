library(rscala)

serialize <- as.logical(Sys.getenv("RSCALA_SERIALIZE"))
cat(serialize,"\n")
s <- scala(serialize=serialize)

s <- scala(heap.max="64M")
s %~% "util.Properties.versionNumberString"
a <- tryCatch(s$do("Array[Double]")$new(10000000L),error=function(e) e)
close(s)

s <- scala(heap.max="256M")
s %~% "util.Properties.versionNumberString"
a <- s$do("Array[Double]")$new(10000000L)
close(s)

options(rscala.heap.max="256M")
s <- scala()
s %~% "util.Properties.versionNumberString"
a <- s$do("Array[Double]")$new(10000000L)
close(s)

