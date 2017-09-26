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
  r <- function(x1="Mack",x2="Bob") s %!% '
    x1+" "+x2+" "+R.getS0("see")
  '
  r
}
y1 <- mkFunc1()
identical(y1("Lisa","Dahl"),"Lisa Dahl B.")


mkFunc2 <- function() {
  see <- "MMMM."
  x <- "Milly"
  r <- function(x="Mack",y="Bob") s %.!% '
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
  e <- function(x=0,y=4L,name="dog") s %!% '
    name + " " + ( x + y )
  '
})

# Taking advantage of caching
system.time({
  f <- function(x=0,y=4L,name="dog") s %!% '
    name + " " + ( x + y )
  '
})

####

nextDouble <- function(rng=s$null("scala.util.Random")) s %!% "rng.nextDouble()"

mkRNG1 <- function() s %.!% 'new scala.util.Random()'
mkRNG2 <- function() s %.~% 'new scala.util.Random()'

rng1 <- mkRNG1()
rng2 <- mkRNG2()

rng1$nextInt(10L)
rng2$nextInt(10L)

str <- rng1$toString(.AS.REFERENCE=TRUE)
str$length()

nd0 <- rng1$nextDouble(.EVALUATE=FALSE)
nd1 <- function() s %!% 'R.cached("@{toString(rng1)}").asInstanceOf[@{rng1[[\'type\']]}].nextDouble()'
nd2 <- function() s %!% '@{rng2}.nextDouble()'

library("microbenchmark")

microbenchmark(
  runif(1),
  rng1$nextDouble(),
  rng2$nextDouble(),
  nd0(),
  nd1(),
  nd2(),
  times=500
)

####

f <- function(x=4) {
  if ( x < 0 ) stop("'x' must be positive")
  if ( x == 0 ) x <- 10
  s %!% '2*x'
}

f(4)
tryCatch(f(-3), error=function(e) e)
f(0)

h <- function(x=4) {
  if ( x < 0 ) stop("'x' must be positive")
  if ( x == 0 ) x <- 10
  s %~% '2*R.getD0("x")'
}

h(4)
tryCatch(h(-3), error=function(e) e)
h(0)

microbenchmark(
  f(4),
  h(4),        # h is *slow* and it has the memory leak inherent in Scala's REPL.
  times=100)




####

s$.scala.util.Random$nextDouble()
m <- s$.scala.util.Random$new(342L,.EVALUATE=FALSE)
m(23436)$nextDouble()
m(63502)$nextDouble()
m(93222)$nextDouble()
m(93222)$nextDouble()

n <- m(5)$nextDouble(.EVALUATE=FALSE)
n()
n()
n()

s$'.Array[Int]'$new(5L)

####

s$a <- 1:10
a <- s$.a
a$apply(3L)

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

microbenchmark(
  a$nextDouble(),
  b$nextDouble(),
  times=1000
  )
  

####

f <- function(x, wantNull=TRUE) s %.!% '
  val r = R.getReference(x)
  if ( wantNull ) null else r
'

a <- f(II(1:10), FALSE)
a$name()

g <- function(func, y=scalaNull("PersistentReference")) s %!% '
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

