source("common.R",print.eval=TRUE)


see <- "David"
mkFunc1 <- function() {
  see <- "B."
  x <- "Milly"
  print(environment())
  r <- s$def(I("Mack"),I("Bob")) %~% '
    x1+" "+x2+" "+R.getS0("see")
  '
  r
}
y1 <- mkFunc1()
identical(y1("Lisa","Dahl"),"Lisa Dahl B.")


mkFunc2 <- function() {
  see <- "MMMM."
  x <- "Milly"
  r <- s$def(x=I("Mack"),y=I("Bob")) %.~% '
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
system.time(e <- s$def(x=I(0),y=I(4L),name=I("dog")) %~% '
  name + " " + ( x + y )
')

# Taking advantage of caching
system.time(f <- s$def(x=I(0),y=I(4L),name=I("dog")) %~% '
  name + " " + ( x + y )
')

####

nextDouble <- s$def(rng=s$null("scala.util.Random")) %~% "rng.nextDouble()"

mkRNG1 <- s$def() %.~% 'new scala.util.Random()'
mkRNG2 <- function() s %.~% 'new scala.util.Random()'

rng1 <- mkRNG1()
rng2 <- mkRNG2()

rng1$nextInt(I(10L))
rng2$nextInt(I(10L))

str <- rng1$toString(.EVALUATE=TRUE,.AS.REFERENCE=TRUE)
str$length()

nd0 <- rng1$nextDouble(.EVALUATE=FALSE)
nd1 <- s$def() %~% 'R.cached("@{toString(rng1)}").asInstanceOf[@{rng1[[\'type\']]}].nextDouble()'
nd2 <- s$def() %~% '@{rng2}.nextDouble()'

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

s$.scala.util.Random$nextDouble()
m <- s$.scala.util.Random$new(I(342L),.EVALUATE=FALSE)
m(23436)$nextDouble()
m(63202)$nextDouble()
m(93222)$nextDouble()
m(93332)$nextDouble()

n <- m(5)$nextDouble(.EVALUATE=FALSE)   # Wrapping with I() is not needed.
n()
n()
n()

s$'.Array[Int]'$new(I(5L))

####

s$a <- 1:10
a <- s$.a
a$apply(I(3L))
tryCatch(a$apply(3L),error=function(e) e)

s$.scala.util.Random$nextDouble()
s$.scala.util.Random$nextDouble()

s %~% 'import scala.util.Random'
s$.Random$nextDouble()

a <- s$def() %~% 'Random.nextDouble'
a()

s$.scala.util.Random
a <- ( s$def() %~% 'scala.util.Random' )()
a$nextDouble(.EVALUATE=TRUE)

####

f <- s$def(x=s$null("(Int,Int)")) %~% 'x._1 + x._2'
g <- s %~% "(300,400)"
f(g)
f(s %~% "(30,40)")

f2 <- s$def() %~% 'println("Yes")'
f2()
capture.output(f2())

a <- s %.~% "(300,234)"
f1 <- s$def(x=s$null("(Int,Int)"),y=numeric()) %~% 'x._1 + x._2 + y.sum'
f1(a,c(2,3,4,6))

f1 <- s$def(x=s$null("(Int,Int)"),y=s$null("Array[Double]")) %~% 'x._1 + x._2 + y.sum'
b <- s %.~% "Array[Double](2,3,4,5)"
f1(a,b)

####

(s$def() %~% 'println("Yes")')()
(s$def() %~% '0')()
(s$def() %~% 'null')()

####

tryCatch((s$def() %~% 'a+b')(),error = function(e) e)
tryCatch((s$def() %~% 'a+')(),error = function(e) e)
tryCatch((s$def() %~% 'import org.asdfad')(),error = function(e) {e})
tryCatch((s$def() %~% 'throw new RuntimeException()')(),error = function(e) {e})
s %~% "5+6"   # Everything's still okay!

