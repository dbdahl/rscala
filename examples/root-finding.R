library(rscala)
scala()

bisection <- function(func, lower, upper, epsilon=1e-14) s(g=func, lower=lower, upper=upper, epsilon=epsilon) * '
  val (fLower, fUpper) = (g(lower), g(upper))
  if ( fLower * fUpper > 0 ) sys.error("lower and upper do not straddle the root.")
  @scala.annotation.tailrec
  def engine(l: Double, u: Double, fLower: Double, fUpper: Double): Double = {
    if ( math.abs( l - u ) <= epsilon ) ( l + u ) / 2
    else {
      val c = ( l + u ) / 2
      val fCenter = g(c)
      if ( fLower * fCenter < 0 ) engine(l, c, fLower, fCenter)
      else engine(c, u, fCenter, fUpper)
    }
  }
  engine(lower, upper, fLower, fUpper)
'

mkFunction1 <- function(n, target) {
  f1 <- function(a) sum(a / (1:n + a - 1))
  s(func=s-f1, n=n, target=target) ^ '
    (x: Double) => R.evalD0("%-(%-)", func, x) - target
  '
}

mkFunction2 <- function(n, target) {
  s(a=scalaType("Double"), n=n, target=target) %~% {
    x <- numeric(n)
    for ( i in 1:n ) {
      x[i] <- a / ( i + a - 1 )
    }
    sum(x) - target
  }
}

mkFunction1(100, 10)
mkFunction2(100, 10)

library(microbenchmark)

microbenchmark(
  bisection(mkFunction1(100, 10), 0.1, 20),
  bisection(mkFunction2(100, 10), 0.1, 20),
  times=100)


f <- function(x,y=2) {
  2*x
}
as.list(f)


str(f)
f2 <- attributes(f)$srcref
str(f2)
attributes(f2)$srcfile

