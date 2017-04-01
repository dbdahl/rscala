library(rscala)
library(microbenchmark)

s <- scala()
e <- s %~% 'new scala.util.Random()'
s$w <- 4

microbenchmark(
  s$w,
  s %~% 'w',
  s$a <- 4,
  s$b <- rnorm(100),
  e$nextDouble(),
  e$toString(),
  times=50
)



s <- scala(debug=TRUE)

(s %new% 'scala.util.Random')(5L)


(s %new% 'Array[Int]')(6L)








bob <- function() {
  s$a <- 1L
  s$b <- 8L
  s %~% 'Array(a,b)'
}

bill <- function(a,b) {
  s %~% 'Array(@{a},@{b})'
}

sam <- function(a,b) {
  s %~% 'Array(R.getI0("a"),R.getI0("b"))'
}

ultimate <- function(a,b) {
  method <- s %~% '() => { Array(R.getI0("a"),R.getI0("b")) }'
  s$invoke(method)
}

microbenchmark(
  (s %~% 'Array')$apply(1L,8L),
  bob(),
  bill(1L,8L),
  sam(1L,8L),
  times=50
)



is.integer((s %~% 'Array')$apply(1L,8L))
is.integer((s %~% 'Array')$'apply[Double]'(1L,8L))





library(rscala)

s <- scala()

e <- s$def2(x=scalaPrimitive(0),y=scalaPrimitive(4L),z=logical(),name=scalaPrimitive(""))

f <- e %~% '
  3+4
'

e <- s$def2(x=scalaPrimitive(0),y=scalaPrimitive(4L),name=scalaPrimitive("dog")) %~% '
  name + " " + ( x + y )
'

e


