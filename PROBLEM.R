library(rscala)
s <- scala()

f <- function(x=I(1.0)) s %!% "2*x"
f()
f(5)

h <- scalaOptimize(f)
h()        # Should that support default arguments?  I think this could be easy done by setting the write environemtn when the funtion is defined.  It doesn't right now.
h(5)

library(microbenchmark)
microbenchmark(
  f(5),
  h(5),
  times=1000)

