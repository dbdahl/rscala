library(rscala)

s <- scala(debug=FALSE)

bob <- s$def(NULL) %~% 'R.evalD0(s"sum($x1)")'
bob(1:55)

bill <- s$def(f=NULL,x=NULL,y=numeric()) %~% '
  R.y = y
  R.evalD1(f,x,"y")
'

f <- function(x,y) {
  2*x^3 + y
}

bill(f,1:33,0)









library(microbenchmark)


s <- scala()
x <- rnorm(100)
s$a <- x

f <- s$def() %.~% '
  R.get("x")
  null
'

g <- s$def() %~% '1'
g2 <- s$def() %~% 'a'
g3 <- s$def() %.~% 'a'

gc()

e <- new.env()
reg.finalizer(e,function(r) cat('#####\n'))

microbenchmark(
  s$b <- x,
  s %~% 'a',
  s$a,
  g2(),
  g3(),
  g(),
  times=100)

length(get("garbage",envir=s[['env']]))



microbenchmark(
  s$b <- x,
  times=1000)

microbenchmark(
  s$a,
  times=100)

microbenchmark(
  g2(),
  times=1000)





library(rscala)
s <- scala()
f <- s$def(x=numeric(),y=numeric()) %~% '
  val z = new Array[Double](x.length+y.length)
  for ( i <- x.indices )
    for ( j <- y.indices )
      z(i+j) = x(i) + y(j)
  z
'

f(1:4,1:3)
f(1:40,1:30)

g <- function(x,y) {
  z <- numeric(length(x)+length(y))
  for ( i in seq_along(x) )
    for ( j in seq_along(y) )
      z[i+j] <- x[i] + y[j]
  z
}
g(1:4,1:3)
g(1:40,1:30)


library(microbenchmark)
microbenchmark(
  f(1:40,1:30),
  g(1:40,1:30),
  times=100)

microbenchmark(
  g(1:400,1:300),
  f(1:400,1:300),
  times=100)

