library(rscala2)
scala()

f0 <- function(x) s(x=x) %~% 'println("<:"+x+":>")'
f1 <- function() s %~% 'println("<:"+x+":>")'
f2 <- function() s() %~% 'println("<:"+x+":>")'
f5 <- function() s(x) %~% 'println("<:"+x+":>")'
f5()

x <- 4
s()

asdfasdf <- s


library(rJava)
.jinit()
rt <- J("java.lang.Runtime")$getRuntime()

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


