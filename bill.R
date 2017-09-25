library(rscala)

s <- scala()

rng <- s$.scala.util.Random$new()
#fasterNextGaussian <- rng$nextGaussian(.EVALUATE=FALSE)
fasterNextGaussian <- function() rng$nextGaussian()

library('rJava', verbose=FALSE, quietly=TRUE)
rJava::.jinit(
  list.files(file.path(scalaInfo(s)$home, "lib"), full.names=TRUE)
)

rngRJava <- rJava::.jnew("scala.util.Random")
fasterNextGaussianRJava <- function() rJava::.jcall(rngRJava, "D", "nextGaussian")

library('microbenchmark')
microbenchmark(
fasterNextGaussianRJava(),
fasterNextGaussian(),
rngRJava$nextGaussian(),
rng$nextGaussian(),
times=30000)

