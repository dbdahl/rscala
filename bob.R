library(rscala)
s <- scala()

library(rJava)
.jinit(
  list.files(file.path(scalaInfo(s)$home, "lib"), full.names=TRUE)
)

rng1 <- .jnew("scala.util.Random")
rng2 <- s$.scala.util.Random$new()

library(microbenchmark)
microbenchmark(
  .jnew("scala.util.Random"),
  s$.scala.util.Random$new(),
  rng1$nextGaussian(),
  rng2$nextGaussian(),
  s$.scala.util.Random$nextGaussian(),
  times=10000)


rng3 <- s$..scala.util.dRandom$new()
s %~% "3+5"

