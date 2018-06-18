library(rscala)
rscala::scala(assign.name="o")
rscala::'%~%'(o,"3+4")

library(microbenchmark)
microbenchmark(
 { scala(); s; close(s) },
 times=10
)



library(rscala2)

callback <- function(s) {
  s %@% '
    println("Hi there")
    import scala.util.{Random => R}
    val a = 45
  '
}

scala(assign.callback=callback)
s$R.nextDouble()
s %~% "a"
close(s)


scala()
s %@% '
  import scala.util.{Random => R}
  val b = 34asd
'


library(rscala2)
library(microbenchmark)
microbenchmark(
 { scala(); s(); close(s) },
 times=10
)
warnings()

scala(assign.name=)

gcs(x=rnorm(10000))

library(rscala2)
scala()
s %~% "Thread.sleep(10000)"
close(s)



s(x=rnorm(10)) %~% "x.sum"
s(x=I(rnorm(10))) %~% "x.sum"
s(x=rnorm(1),nrow=1) %~% "x+100*nrow"
s(x=I(rnorm(1))) %~% "x.sum"
s(x=I(rnorm(100))) %~% "x.sum"

# Check supported types
f <- function(x) identical(x, s(x=x) %~% "x")
sapply(list(1,c(1,2),matrix(as.double(1:6),nrow=3)), f)
sapply(list(1L,c(1L,2L),matrix(1:6,nrow=3)), f)
sapply(list(TRUE,c(TRUE,FALSE),matrix(c(TRUE,FALSE,TRUE),nrow=3)), f)
sapply(list(as.raw(1L),as.raw(c(1L,2L)),matrix(as.raw(1:6),nrow=3)), f)
sapply(list("David",c("David","Dahl"),matrix(c("David","Dahl"),nrow=1)), f)

# Interrupts
s %~% "Thread.sleep(10000); 5"
scalaLast(s)

s %~% "1"
rng <- s$.new_scala.util.Random()
rng$nextGaussian()
rng$.nextGaussian()


s %~% "3+4"

s(x=list(a=3,b=3)) %~% "x"
s(x=matrix(1:10,nrow=2)) %~% "x"

s %~% '
  Array(Array(1,3),Array(4,5))
'


x <- matrix(1:10,nrow=2)
x
identical(x,s(x=x) %~% "x")
b <- s(x=x) %.~% "x"
s(b=b) %~% "b"

s(x=matrix(integer(),nrow=2)) %~% "x"

s(x=c(TRUE,FALSE)) %~% "x"
s(x=TRUE) %~% "x"
s(x=FALSE) %~% "x"

library(microbenchmark)
microbenchmark(
  s %~% "3+4",
  s(x=rnorm(1000)) %~% "x",
  rscala::'%~%'(o,"3+4"),
  {x <- rnorm(1000); rscala::'%~%'(o['x'],"x") },
  times=1000
)

rtScala <- s$java.lang.Runtime.getRuntime()
rtScala$availableProcessors()

a <- rtScala$.availableProcessors()
a$toDouble()

s$java.lang.Runtime.getRuntime.availableProcessors()

rng <- s$.new_scala.util.Random()

library(rJava)
.jinit()
rtJava <- J("java.lang.Runtime")$getRuntime()
rtJava$availableProcessors()

library(microbenchmark)
microbenchmark(
  rtJava$availableProcessors(),
  rtScala$availableProcessors(),
  .jcall(rtJava,"I","availableProcessors"),
  s$java.lang.Runtime.getRuntime.availableProcessors(),
  s(x=rng) %~% "x.nextDouble()",
  rng$nextDouble(),
  times=1000)

s(x=rng) %~% "x.nextDouble()"
s(x=rng) %.~% "x.nextDouble()"

rng <- s$scala.util.Random()

f9 <- function(x) s(x=x) %~% 'Array(Array(x))'
a <- f9(3L)
gc()
attr(s,"details")[["garbage"]]

e <- s %~% 'new scala.util.Random()'

f0 <- function(x) s(x=x) %~% 'println("<:"+x+":>")'
f1 <- function() s %~% 'println("<:"+x+":>")'
f2 <- function() s() %~% 'println("<:"+x+":>")'
f5 <- function() s(x=6) %~% 'println("<:"+x+":>")'
f5 <- function() s(x=6) %~% '
  println("<:"+x+":>")
  x+y+1
'
f5()
f2()


library(microbenchmark)
microbenchmark(
  rt$availableProcessors(),
  .jcall(rt,"I","availableProcessors"),
  s$java.lang.Runtime.getRuntime.availableProcessors(),
  times=10000)



x <- 4
s()

asdfasdf <- s



b <- paste(letters,collapse="")
x <- 3

library(microbenchmark)
microbenchmark(
  .jcall(rt,"I","availableProcessors"),
  s$j(2,3L,x),
  s$j(2,3L,x,b,b),
  s$j(2,3L,x,b,big),
  s$j(rnorm(10)),
  s$j(),
  g(attr(s,"details"),"availableProcessors",list(),FALSE),
  s$java.lang.Runtime.getRuntime.availableProcessors(),
  scalaEcho(s),
  f1(),
  f2(),
  f0(3),
  times=1000)



rscala2:::scalaInvoke(attr(s,"details"),"availableProcessors",list(),FALSE)

g <- rscala2:::scalaInvoke











f0 <- function(x) s %~% 'println("<:"+x+":>")'
f1 <- function(x) s(x=x) %~% 'println("<:"+x+":>")'
f2 <- function(x) s(x=x,b="Dahl",pi=pi) %~% 'println("<:"+x+" "+b+":>")'

big <- paste0(rep(letters,times=100),collapse="")

f0(1)
f1(2)
f2(3L)
x <- "David"
s$java.lang.Runtime.getRuntime.availableProcessors(2,3L,x)
s$java.lang.System.setProperty("scala.is.cool","TRUE")
s$java.lang.System.setProperty("scala.is.cool",big)

library(microbenchmark)
microbenchmark(
f0(3),
s$j(big),
s$j(3L),
s$j(),
s$java.lang.Runtime.getRuntime.availableProcessors(),
.jcall(rt,"I","availableProcessors"),
s$java.lang.System.setProperty("scala.is.cool","TRUE"),
.jcall("java.lang.System","S","setProperty","scala.is.cool","TRUE"),
s$java.lang.System.setProperty("scala.is.cool",big),
.jcall("java.lang.System","S","setProperty","scala.is.cool",big),
times=1000)





  .jcall("java.lang.System","S","setProperty","scala.is.cool","TRUE")


s$java.lang.Runtime.getRuntime.availableProcessors()


s(y=y, x=x, z=c(4,3,4), z=matrix(1:10,nrow=5)) %~% 'bob'

pop(attr(s,"details"))
#





library(microbenchmark)
microbenchmark(
s %~% 'bob'
,
s(y=y, x=x, z=c(4,3,4), z=matrix(1:10,nrow=5)) %~% 'bob'
,times=10000)


push(1L, s)
push(2L, s)
push(3L, s)
pop(s)
pop(s)

push(1:10, s)
is.integer(pop(s))
push(as.double(1:10), s)
is.integer(pop(s))


library(rscala)
scala()
bob <- function(x) s %!% 'x'
bill <- function(x) { push(x); pop(x) }

library(rJava)
.jinit()
rt <- J("java.lang.Runtime")$getRuntime()
rt$availableProcessors()
.jcall(rt,"I","availableProcessors")

a <- integer(2)
a <- 1L
library(microbenchmark)
microbenchmark(
#  bob(a),
  .jcall(rt,"I","availableProcessors"),
  ,
  bill(a),
#  rt$availableProcessors(),
  times=10000)




microbenchmark(
#scalaDef("java.lang.Runtime.getRuntime",2,4L,c(1,2))
#,
scalaDef("java.lang.Runtime.getRuntime")
,times=10)

cat(s$.java.lang.Runtime.getRuntime(2,4L,c(1,2)))


