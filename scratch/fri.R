library(rscala)

scala(debug=TRUE)

s$debug <- FALSE
s$showCode <- FALSE

f <- function(x) s(x=x) %~% "3+x"

s %@% '
  def add3(x: Double) = 3+x
'
g <- s$add3

f(4)
g(4)

library(microbenchmark)
microbenchmark(
  f(4),
  g(4),
  s$add3(4),
  times=10000)

g(3)

mkList <- s$List
s$Some(mkList(2,3,4))


