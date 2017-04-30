library(rscala)
s <- scala()

x <- 5
m <- function() s %!% 'R.get("x")._1'
m()

wrapper <- function() {
  n <- function() s %!% 'R.get("x")._1'
  n()
}

wrapper()

wrapper2 <- function(x) {
  n <- function() s %!% 'R.get("x")._1'
  n()
}

wrapper2(55) ## But should be 55

wrapper3 <- function() {
  x <- 10
  n <- function() s %!% 'R.get("x")._1'
  n()
}

wrapper3() ## But should be 10

#### Idea:  rather than .EVALUATE=FALSE,
###
##

f <- function(x=I(1.0)) s %!% "2*x"
f()

g <- function(x=I(1.0),.EVALUATE=FALSE) s %!% "2*x"
h <- g()
h()        # Should that support default arguments?  It doesn't right now.
h(5)

h <- scalaAsFunction(f)
h()
h(5)

