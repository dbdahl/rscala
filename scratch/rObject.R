library(rscala)
library(microbenchmark)

scala()

sampler <- function() {
  list(mean=rnorm(1),prob=rbeta(1,1,1))
}

microbenchmark(
o <- s(samplerFunction=s-sampler) * '
  R.evalObject("%-()",samplerFunction)
',
s$R.evalObject("%-()",s-sampler),
times=1000)

sf <- s-sampler
-sf

y <- s$R.evalObject
microbenchmark(
lapply(1:10000, function(i) s$R.evalObject("%-()",s-sampler)),
lapply(1:10000, function(i) s$R.evalObject("%-()",sf)),
lapply(1:10000, function(i) y("%-()",sf)),
sapply(e, function(i) -i),
times=3)

p <- s$R.evalObject("sapply(1:10000, function(i) %-())",sf)
-p


microbenchmark(
  1:100,
  as.integer(1:100),
  times=1000)



y <- s - rnorm
(-y)(100)

doit <- function(x, nSamples) {
  s(y=s-x) * 'R.evalD1("I(%-(nSamples))",y)'
}

microbenchmark(
  doit(rnorm,1),
  rnorm(1),
  doit(rnorm,1000),
  rnorm(1000),
  times=100)




