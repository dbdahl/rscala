library(rscala)

scala(debug=TRUE, stdout="RSCALA-OUT")
s

s %~% '2+3'

debug(rscala:::callback)
s %~% 'R.evalD1("rnorm(%-, mean=%-, sd=%-)", 10, 5, 2.0)'




s %~% 'R.evalS1("I(paste0(%-, %-))", 10, Array("David","Dahl"))'
s %~% 'R.evalD1("as.double(paste0(%-, %-))", 10, Array("0.1",".1"))'



s %~% "341"
s$R.evalD0("rnorm(%-)",1)
s$R.evalD0("rnor(%-)",1)
s$R.evalD0("rnor(%-)",1)
s$R.eval("list(1,2,%-)",3)

s$R.evalD1("list(1,2,%-)",3)

i

s$R.evalD0("rnor(%-)",list(3,4))

s(x=1) %~% 'R.evalD1("list(1,2,3)")'



s %~% 'R.evalI0("3L",1)'

s %~% "342"




s %~% 'R.evalS0("""
  s <- get("s",envir=.GlobalEnv)
  s$Array(1)
""")'




s$debug <- TRUE

library(microbenchmark)
microbenchmark(
  s$R.evalD1("I(rnorm(%-))",1),
  s %~% 'R.evalD1("I(rnorm(%-))", 1)',
  times=1000)







rng <- s$.new_java.util.Random()

f <- function(x) x*rng$nextInt(30L)

f <- function(x) {
  cat("Hi from R\n")
  s(x=x,rng=rng) %~% '
    println("Hi from Scala")
    x * rng.nextInt(30)
  '
}

f(10)


debug(rscala:::callback)

h <- function() s %~% '
  println("Hi form Scala0.")
  R.evalI0("f(%-)",5+2)
'
h()


s(y=7) %~% 'R.evalD0("f(%-)",y+2)'




g <- function(y) s(y=y) %~% '
  R.evalD0("f(%-)",y)
'

g(3)





library(microbenchmark)
microbenchmark(
f(3),
g(3),
times=1000)


