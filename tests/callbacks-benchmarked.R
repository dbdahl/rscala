source("common.R",print.eval=TRUE)
set.seed(924234)


sleep.time <- 0
f <- function(x) { Sys.sleep(sleep.time); mean(x) }
g <- function(x) { Sys.sleep(sleep.time); sd(x) }



# Native R code implementation
doit0 <- function(x) {
  y <- 2*x
  c(f(y),g(y))
}

doit0(rnorm(10))



# Single callback in interpreted code.
doit1 <- function(x) {
  s$x <- x
  s %@% 'R.set("y",x.map(2*_))'
  c(s %~% 'R.evalD0("f(y)")',
    s %~% 'R.evalD0("g(y)")')
}

doit1(rnorm(10))



# Multiple callbacks in interpreted code.
doit2 <- function(x) {
  s$x <- x
  s %~% '
    R.set("y",x.map(2*_))
    Array(R.evalD0("f(y)"),
          R.evalD0("g(y)"))
  '
}

doit2(rnorm(10))




# Multiple callbacks in compiled code.
doit3 <- function(x=numeric()) s %!% '
  R.set("y",x.map(2*_))
  Array(R.evalD0("f(y)"),
        R.evalD0("g(y)"))
'

doit3(rnorm(10))



# Multiple callbacks in compiled code with optimization.
doit4 <- scalaOptimize(doit3)

doit4(rnorm(10))



# Benchmarks

library(microbenchmark)

sleep.time <- 0
microbenchmark(
  doit0(rnorm(10)),
  doit1(rnorm(10)),
  doit2(rnorm(10)),
  doit3(rnorm(10)),
  doit4(rnorm(10)),
  times=10
)
microbenchmark(
  doit0(rnorm(10)),
  #doit1(rnorm(10)),
  #doit2(rnorm(10)),
  doit3(rnorm(10)),
  doit4(rnorm(10)),
  times=1000
)


sleep.time <- 0.1
microbenchmark(
  doit0(rnorm(10)),
  doit1(rnorm(10)),
  doit2(rnorm(10)),
  doit3(rnorm(10)),
  doit4(rnorm(10)),
  times=5
)

