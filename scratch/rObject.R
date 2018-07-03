library(rscala)
library(microbenchmark)

scala()

sampler <- function() {
  list(mean=rnorm(1),prob=rbeta(1,1,1))
}

mm <- function(n) s(sf=s-sampler,n=as.integer(n[1])) * '
  List.fill(n) {
    R.evalObject("%-()",sf)
  }
'

b <- mm(10)
library(microbenchmark)
microbenchmark(
  mm(1)
  ,
  mm(10)
  ,
  mm(100)
  ,
  mm(500)
  ,
  mm(1000)
  ,
  mm(10000)
  ,times=10)

b <- mm(1000)

-(s - list(1,2,3))
-(s - I(list(1,2,3)))

identical(-b, -(s - (-b)))




microbenchmark(
  -b
  ,
  sapply(-b,function(i) i) 
  ,times=10)



undo <- function(b) {
  nn <- s(mm=b) ^ '
    (mm.flatMap(_.x), mm.scanLeft(1)((sum,x) => sum + x.x.length))
  '
  bytes <- nn$"_1"()
  sizes <- nn$"_2"()
  lapply(seq_along(sizes[-1]), function(i) {
    unserialize(bytes[sizes[i]:(sizes[i+1]-1)])
  })
}
p <- undo(b)



nn <- s(mm=mm) * '
  mm.map(_.x)
'
unlist(apply(nn,1,function(i) unserialize(i)))

for ( i in seq_along(sizes) ) {
  unserialize(arr
}

y <- s$R.evalObject
microbenchmark(
lapply(1:10000, function(i) s$R.evalObject("%-()",s-sampler)),
lapply(1:10000, function(i) s$R.evalObject("%-()",sf)),
lapply(1:10000, function(i) y("%-()",sf)),
sapply(e, function(i) -i),
times=3)

p <- s$R.evalObject("sapply(1:10000, function(i) %-())",sf)
-p


microbenchmark(
  1:100,
  as.integer(1:100),
  times=1000)



y <- s - rnorm
(-y)(100)

doit <- function(x, nSamples) {
  s(y=s-x) * 'R.evalD1("I(%-(nSamples))",y)'
}

microbenchmark(
  doit(rnorm,1),
  rnorm(1),
  doit(rnorm,1000),
  rnorm(1000),
  times=100)




