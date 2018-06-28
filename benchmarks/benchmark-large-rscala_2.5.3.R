library(rscala)
library(microbenchmark)

cat(system2("hostname",stdout=TRUE),"\n",sep="")
scalaInfo()
sessionInfo()

scala(heap.maximum="14090M")
nReps <- 100

#####

big <- rnorm(10000000)
a <- function(x) s %.!% 'x'
b <- function(x) s %!% 'x'
invisible(a(big))
invisible(b(big))

microbenchmark(
  a(big),
  b(big),
  times=nReps
)
 
s %~% "true"    # Is everything still okay?

