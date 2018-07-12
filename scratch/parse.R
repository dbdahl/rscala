library(rscala)
scala("commonsMath",serialize.output = TRUE)
s$showCode <- TRUE
s$debugTranscompilation <- FALSE


f <- function() {
  sample(c("A","B","C"),500000,TRUE,c(0.25,0.65,0.10))
}
g <- s^f
x <- g()
table(x)/500000
table(f())/500000

f <- function() {
  rgamma(1000000,1,100)
}
summary((s^f)())
summary(f())

f <- function() {
  rbeta(1000000,1,2)
}
summary((s^f)())
summary(f())

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
