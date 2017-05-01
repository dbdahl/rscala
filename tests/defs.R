source("common.R",print.eval=TRUE)


####


rng1 <- s %.~% "new scala.util.Random"
rng1$nextDouble()

s$r1 <- rng1
rng2 <- s$r1
rng2$nextDouble()

rng3 <- s$.scala.util.Random$new()
rng3$nextDouble()

s$r3 <- rng3
rng4 <- s$r3
rng4$nextDouble()

f <- rng4$nextDouble(.EVALUATE=FALSE)
f()


####


see <- "David"
mkFunc1 <- function() {
  see <- "B."
  x <- "Milly"
  print(environment())
  r <- function(x1=I("Mack"),x2=I("Bob")) s %!% '
    x1+" "+x2+" "+R.getS0("see")
  '
  r
}
y1 <- mkFunc1()
identical(y1("Lisa","Dahl"),"Lisa Dahl B.")


mkFunc2 <- function() {
  see <- "MMMM."
  x <- "Milly"
  r <- function(x=I("Mack"),y=I("Bob")) s %.!% '
      x+" "+y+" "+R.getS0("see")
  '
  r
}
y2 <- mkFunc2()
identical(y2("Lisa","Dahl")$toString(),"Lisa Dahl MMMM.")
gc()



func1 <- function() {
  see <- "Knudsen"
  y1("Lisa","Dahl")
}
identical(func1(),"Lisa Dahl B.")


####


# Realistic timing
system.time({
  e <- function(x=I(0),y=I(4L),name=I("dog")) s %!% '
    name + " " + ( x + y )
  '
  scalaOptimize(e)
})

# Taking advantage of caching
system.time({
  f <- function(x=I(0),y=I(4L),name=I("dog")) s %!% '
    name + " " + ( x + y )
  '
  scalaOptimize(f)
})

####

nextDouble <- function(rng=s$null("scala.util.Random")) s %!% "rng.nextDouble()"

mkRNG1 <- function() s %.!% 'new scala.util.Random()'
mkRNG2 <- function() s %.~% 'new scala.util.Random()'

rng1 <- mkRNG1()
rng2 <- mkRNG2()

rng1$nextInt(I(10L))
rng2$nextInt(I(10L))

str <- rng1$toString(.AS.REFERENCE=TRUE)
str$length()

nd0 <- rng1$nextDouble(.EVALUATE=FALSE)
nd1 <- function() s %!% 'R.cached("@{toString(rng1)}").asInstanceOf[@{rng1[[\'type\']]}].nextDouble()'
nd1b <- scalaOptimize(nd1)
nd2 <- function() s %!% '@{rng2}.nextDouble()'
nd2b <- scalaOptimize(nd2)

library("microbenchmark")

microbenchmark(
  runif(1),
  rng1$nextDouble(),
  rng2$nextDouble(),
  nd0(),
  nd1(),
  nd1b(),
  nd2(),
  nd2b(),
  times=500
)

####

f <- function(x=4) {
  if ( x < 0 ) stop("'x' must be positive")
  if ( x == 0 ) x <- 10
  s %!% '2*x.sum'
}

f(4)
tryCatch(f(-3), error=function(e) e)
f(0)

g <- scalaOptimize(f)
g(4)
g(-3)     ## Note that no error because R code as been optimized away!
g(0)      ## And we don't get the special case when x==0.

h <- function(x=4) {
  if ( x < 0 ) stop("'x' must be positive")
  if ( x == 0 ) x <- 10
  s %~% '2*R.getD1("x").sum'
}

h(4)
tryCatch(h(-3), error=function(e) e)
h(0)

library(microbenchmark)
microbenchmark(
  f(4),
  g(4),
  h(4),        # h is *slow* and it has the memory leak inherent in Scala's REPL.
  times=100)

microbenchmark(
  f(4),        
  g(4),        # g is faster, but you lose the checking and special behavior.
  times=1000)




####

s$.scala.util.Random$nextDouble()
m <- s$.scala.util.Random$new(I(342L),.EVALUATE=FALSE)
m(23436)$nextDouble()
m(63502)$nextDouble()
m(93222)$nextDouble()
m(93222)$nextDouble()

n <- m(5)$nextDouble(.EVALUATE=FALSE)   # Wrapping with I() is not needed.
n()
n()
n()

s$'.Array[Int]'$new(I(5L))

####

s$a <- 1:10
a <- s$.a
a$apply(I(3L))
tryCatch(a$apply(3L),error=function(e) e)  # Needs to be wrapped in I().

s$.scala.util.Random$nextDouble()
s$.scala.util.Random$nextDouble()

s %@% 'import scala.util.Random'
s$.Random$nextDouble()

a <- function() s %!% 'Random.nextDouble'
a()

a <- s$.scala.util.Random
a$nextDouble()

b <- ( function() s %!% 'scala.util.Random' )()
b$nextDouble()

library(microbenchmark)
microbenchmark(
  a$nextDouble(),
  b$nextDouble(),
  times=1000
  )
  

####

f <- function(x=NULL, wantNull=I(TRUE)) s %.!% '
  val r = R.makePersistent(x)
  if ( wantNull ) null else r
'

a <- f(1:10, FALSE)
a$name()

g <- function(func=NULL,y=scalaNull("RPersistentReference")) s %!% '
  R.invoke(func,y)
'

g(print, a)


####

f <- function(x=scalaNull("(Int,Int)")) s %!% 'x._1 + x._2'
g <- s %~% "(300,400)"
f(g)
f(s %.~% "(30,40)")

f2 <- function() s %~% 'println("Yes")'
f2()
capture.output(f2())

a <- s %.~% "(300,234)"
f1 <- function(x=scalaNull("(Int,Int)"),y=numeric()) s %!% 'x._1 + x._2 + y.sum'
f1(a,c(2,3,4,6))

f1 <- function(x=scalaNull("(Int,Int)"),y=scalaNull("Array[Double]")) s %!% 'x._1 + x._2 + y.sum'
b <- s %.~% "Array[Double](2,3,4,5)"
f1(a,b)

####

(function() s %!% 'println("Yes")')()
(function() s %!% '0')()
(function() s %!% 'null')()

####

tryCatch((function() s %!% 'a+b')(),error = function(e) e)
tryCatch((function() s %!% 'a+')(),error = function(e) e)
tryCatch((function() s %!% 'import org.asdfad')(),error = function(e) {e})
tryCatch((function() s %!% 'throw new RuntimeException()')(),error = function(e) {e})
s %~% "5+6"   # Everything's still okay!

