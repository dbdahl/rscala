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
  rng1$nextDouble(),
  rng2$nextDouble(),
  nd0(),
  nd1(),
  nd2(),
  times=500
)

####

s$do("scala.util.Random")$nextDouble()
m <- s$do("scala.util.Random")$new(I(342L),.EVALUATE=FALSE)
m(I(5L))$nextDouble()

s$.scala.util.Random$nextDouble()
m <- s$.scala.util.Random$new(I(342L),.EVALUATE=FALSE)
m(5L)$nextDouble(.EVALUATE=FALSE)   # Wrapping with I() is not needed.
m(5L)$nextDouble()

s$'.Array[Int]'$new(I(5L))
s$do("Array[Int]")$new(I(5L))



s$a <- 1:10
a <- s$.a
a$apply(I(3L))
tryCatch(a$apply(3L),error=function(e) e)

s$do("scala.util.Random")$nextDouble()
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

####

tryCatch(s$def('x,y','x+y')(),error = function(e) e)
s$def('x :    Int,y:Double','x+y')(2,3)
s$def('x:Int, y:Double','x+y')(4,2)

s$def('x:Boolean, y:Double','if ( x ) y else 2*y')(TRUE,2)
s$def('x:Boolean, y:Double','if ( x ) y else 2*y')(FALSE,2)

s$def('x:String, y:Int','if ( x=="bob" ) y else 2*y')('bob',2)
s$def('x:String, y:Int','if ( x=="bob" ) y else 2*y')('bobby',2)

s$def('x:Array[String], y:Array[Int]','if ( x(0)=="bob" ) y(0) else 2*y(0)')('bob',2)
s$def('x:Array[String], y:Array[Int]','if ( x(0)=="bob" ) y(0) else 2*y(0)')('bobby',2)
s$def('x:Array[Boolean], y:Array[Double]','if ( x(0) ) y(0) else 2*y(0)')(TRUE,2)
s$def('x:Array[Boolean], y:Array[Double]','if ( x(0) ) y(0) else 2*y(0)')(TRUE,2)

s$def('x:Array[String], y:Array[Int]','if ( x(0)=="bob" ) y(0) else 2*y(0)')(c('bob','bill'),c(2,10))
s$def('x:Array[String], y:Array[Int]','if ( x(0)=="bob" ) y(0) else 2*y(0)')(c('bobby','bill'),c(2,10))
s$def('x:Array[Boolean], y:Array[Double]','if ( x(0) ) y(0) else 2*y(0)')(c(TRUE,FALSE),c(2,10))
s$def('x:Array[Boolean], y:Array[Double]','if ( x(0) ) y(0) else 2*y(0)')(c(FALSE,TRUE),c(2,10))

s$def('x:Array[Array[String]], y:Array[Array[Int]]','if ( x(0)(0)=="bob" ) y(0)(0) else 2*y(0)(0)')(matrix(c('bob','bill'),ncol=1),matrix(c(2,10),ncol=1))
s$def('x:Array[Array[String]], y:Array[Array[Int]]','if ( x(0)(0)=="bob" ) y(0)(0) else 2*y(0)(0)')(matrix(c('bobby','bill'),ncol=1),matrix(c(2,10),ncol=1))
s$def('x:Array[Array[Boolean]], y:Array[Array[Double]]','if ( x(0)(0) ) y(0)(0) else 2*y(0)(0)')(matrix(c(TRUE,FALSE),ncol=1),matrix(c(2,10),ncol=1))
s$def('x:Array[Array[Boolean]], y:Array[Array[Double]]','if ( x(0)(0) ) y(0)(0) else 2*y(0)(0)')(matrix(c(FALSE,TRUE),ncol=1),matrix(c(2,10),ncol=1))

f <- s$def("x: Double, w: Int, r: Double","
  math.sqrt(x)+w-r
")
f
f(3,4,54)

g <- s$def('x:Array[String], y:Array[Int]','if ( x(0)=="bob" ) y(0) else 2*y(0)')
g("David",3)

f1 <- s$def("x: Double, w: Int, r: Double","
  math.sqrt(x)+w-r
")
f1(3,4,54)

f2 <- s$def("x: Double","2*x")
f2(f1(3,4,54))

system.time(sapply(1:100,function(i) f1(i,4,54)))

