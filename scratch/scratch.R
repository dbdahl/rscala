library(rscala)
s <- scala("commonsMath")
s ^ "3+4"
s ^ function(x=stD1) sample(x)
order2 <- s ^ function(x=stS1,decreasing=stL0) order(x,decreasing=decreasing)
x <- c(3,1,5,6,4)
order2(x,TRUE)
order(x,decreasing=TRUE)

order2(c("a","0","d","b"),TRUE)

showConnections()
s * "3+4"
showConnections()
scala()
showConnections()
gc()
showConnections()







s
s$showCode <- TRUE

a <- 6
s * '2 + R.evalD0("a")'


y <- 2
z <- 3

a <- function() {
  s(x=1,y) * '
    val z = R.evalD0("z")
    x + y*z
  '
}
a()
z <- 5
a()


b <- s(x=1,y) ^ function() {
  z <- evalD0("z")
  x + y*z
}
b()


bNative <- function(x=1,y1=y) {
  x + y1*z
}
bNative()


z <- 10
a()
local({
  z <- 29
  a()
})
a()

b()
local({
  z <- 29
  b()
})
b()

b()
local({
  print(environment())
  z <- 29
  local({
    print(environment())
    z <- 0
    local({
      print(environment())
      z <- -10
      b()
    })
  })
})
b()

bNative()
local({
  z <- 29
  bNative()
})
bNative()

