library(rscala)
library(microbenchmark)

scala()

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




