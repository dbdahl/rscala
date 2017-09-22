library(rscala)
s <- scala(debug=FALSE)

mkRNG <- s$.scala.util.Random$new(.EVALUATE=FALSE)
rng2 <- mkRNG()

rng2$nextGaussian()

rng3 <- s$.scala.util.Random$new()

s %~% "3+4"



library(rJava)
.jinit(
  list.files(file.path(scalaInfo(s)$home, "lib"), full.names=TRUE)
)

rng1 <- .jnew("scala.util.Random")
rng2 <- s$.scala.util.Random$new()

nextGaussian <- rng2$nextGaussian(.EVALUATE=FALSE)

library(microbenchmark)
microbenchmark(
  .jnew("scala.util.Random"),
  s$.scala.util.Random$new(),
  rng1$nextGaussian(),
  rng2$nextGaussian(),
  nextGaussian(),
  s$.scala.util.Random$nextGaussian(),
  times=1000)


rng3 <- s$..scala.util.dRandom$new()
s %~% "3+5"

d <- s %~% "
  class Bob {

    def add(x: Int, y:Int) = x+y

  }
  val bob = new Bob()
"

e <- d$add(3L,5L,.EVALUATE=FALSE)
e("5","2")

s %~% "3+4"



