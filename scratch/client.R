source("mmap-command.R")

# Client

ping <- function(i) {
  mCommand[1] <- CMD_PUSH
  mInt[1] <- i
  releaseTo(CMD_IT_SERVER)
  waitFor(CMD_IT_CLIENT)
  mCommand[1] <- CMD_POP
  releaseTo(CMD_IT_SERVER)
  waitFor(CMD_IT_CLIENT)
  mDouble[1]
}

ping(10)

library(microbenchmark)
library(rscala)
scala()

rng <- s$.scala.util.Random$new()
rng$nextGaussian()

microbenchmark(
  ping(10),
  rng$nextGaussian(),
  times=1000
)

