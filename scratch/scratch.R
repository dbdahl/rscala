library(rscala2)
scala()

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


