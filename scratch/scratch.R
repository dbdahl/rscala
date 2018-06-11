system2("bin/package")
system2("rm",c("pipe-r2sr","pipe-s2r"))
system2("mkfifo","pipe-r2s")
system2("mkfifo","pipe-s2r")
  #system2("sbt","run",wait=FALSE)

library(rscala2)
scala(useSockets=FALSE)

f0 <- function(x) s %~% 'println("<:"+x+":>")'
f0(3)



library(rJava)
.jinit()
rt <- J("java.lang.Runtime")$getRuntime()

counter <- 0
f0 <- function(x) {
  counter <<- counter + 1
  cat(counter,"\n")
  s %~% 'println("<:"+x+":>")'
}
f0(0)

library(microbenchmark)
microbenchmark(
f0(3)
,times=10000)

f0 <- function(x) s %~% 'println("<:"+x+":>")'
f1 <- function(x) s(x=x) %~% 'println("<:"+x+":>")'
f2 <- function(x) s(x=x,b="Dahl",pi=pi) %~% 'println("<:"+x+" "+b+":>")'

big <- paste0(rep(letters,times=100),collapse="")

f0(1)
f0(2)
f0(3)
s$java.lang.Runtime.getRuntime.availableProcessors()
s$java.lang.System.setProperty("scala.is.cool","TRUE")
s$java.lang.System.setProperty("scala.is.cool",big)

library(microbenchmark)
microbenchmark(
f0(3),
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


