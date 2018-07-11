library(rscala)
scala("commonsMath",serialize.output = TRUE)
s$showCode <- TRUE
s$debugTranscompilation <- FALSE


f <- function() {
  dnorm(rnorm(100000))
}
g <- s^f

library(microbenchmark)
microbenchmark(
  f(),
  g(),
  (s^f)(),
  times=100)

f <- s ^ function(x=scalaType("D0")) {
  lgamma(x)
}
f(43)
lgamma(43)
