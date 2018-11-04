library(rscala)
library(microbenchmark)

cat(system2("hostname",stdout=TRUE),"\n",sep="")
scalaConfig()
sessionInfo()

scalaMemory()
s <- scala()
nReps <- 100

#####

big <- rnorm(10000000)
microbenchmark(
  s(x=big) ^ 'x',
  s(x=big) * 'x',
  times=nReps
)
 
s * "true"    # Is everything still okay?

