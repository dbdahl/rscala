library(rscala)
s <- scala()

library(rJava)
.jinit(
  list.files(file.path(scalaInfo(s)$home, "lib"), full.names=TRUE)
)

rng1 <- .jnew("scala.util.Random")
rng2 <- s$.scala.util.Random$new()
rng3 <- s$..scala.util.Random$new()
rng3 <- s$..scala.util.Random$new(5L)

library(microbenchmark)
microbenchmark(
  .jnew("scala.util.Random"),
  s$.scala.util.Random$new(),
  s$..scala.util.Random$new(),
  rng1$nextGaussian(),
  rng2$nextGaussian(),
  rng3$nextGaussian(),
  s$.scala.util.Random$nextGaussian(),
  s$..scala.util.Random$nextGaussian(),
  times=1000)


rng3 <- s$..scala.util.dRandom$new()
s %~% "3+5"

