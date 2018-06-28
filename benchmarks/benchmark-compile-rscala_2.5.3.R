library(rscala)
library(microbenchmark)

cat(system2("hostname",stdout=TRUE),"\n",sep="")
scalaInfo()
sessionInfo()

scala()
nReps <- 100

#####
a <- function() s %!% "2056700609"
a()   # First-time compilation

b <- function() s %!% as.character(sample.int(.Machine$integer.max,1))
b()   # Every compilation is new

microbenchmark(
  b(),
  a(),
  times=nReps
)

