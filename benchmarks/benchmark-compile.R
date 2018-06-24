library(rscala)
library(microbenchmark)

cat(system2("hostname",stdout=TRUE),"\n",sep="")
scalaExec()
sessionInfo()

scala()
nReps <- 100

#####
s %~% 2056700609   # First-time compilation

microbenchmark(
  s %~% sample.int(.Machine$integer.max,1),
  s %~% 2056700609,
  times=nReps
)

