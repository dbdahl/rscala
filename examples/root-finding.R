library(rscala)
s <- scala()

bisection <- function(func, lower, upper, epsilon=.Machine$double.eps^0.25) s(g=func, lower=lower, upper=upper, epsilon=epsilon) * '
  val (fLower, fUpper) = (g(lower), g(upper))
  if ( fLower * fUpper > 0 ) sys.error("lower and upper do not straddle the root.")
  @scala.annotation.tailrec
  def engine(l: Double, u: Double, fLower: Double, fUpper: Double): Double = {
    val c = ( l + u ) / 2
    if ( math.abs( l - u ) <= epsilon ) c
    else {
      val fCenter = g(c)
      if ( fLower * fCenter < 0 ) engine(l, c, fLower, fCenter)
      else engine(c, u, fCenter, fUpper)
    }
  }
  engine(lower, upper, fLower, fUpper)
'



func1 <- function(a=scalaType("Double"), n=100, target=10) {
  sum(a / (1:n + a - 1)) - target
}

wrapped1 <- {
  s(func=scalaPush(func1)) ^ '
    (a: Double) => R.evalD0("%-(%-)", func, a)
  '
}

wrapped2 <- s ^ func1

wrapped3 <- s(func=s ^ func1) ^ function(a=scalaType("Double")) {
  func(a)
}

wrapped4 <- s(n=100L, target=10L) ^ '(a: Double) => {
  var sum = 0.0
  var i = 0
  while ( i < n ) {
    sum += a / ( i + a )
    i += 1
  }
  sum - target
}
'


library(microbenchmark)

microbenchmark(
  bisection(wrapped1, 0.1, 20),
  bisection(wrapped2, 0.1, 20),
  bisection(wrapped3, 0.1, 20),
  bisection(wrapped4, 0.1, 20),
  uniroot(func1,c(0.1,20)),
  times=100)


