library(rscala)

serialize <- as.logical(Sys.getenv("RSCALA_SERIALIZE"))
cat(serialize,"\n")
s <- scalaInterpreter(serialize=serialize)

s <- scalaInterpreter(heap.max="64M")
s %~% "util.Properties.versionNumberString"
a <- tryCatch(s$do("Array[Double]")$new(10000000L),error=function(e) e)
close(s)

s <- scalaInterpreter(heap.max="256M")
s %~% "util.Properties.versionNumberString"
a <- s$do("Array[Double]")$new(10000000L)
close(s)

