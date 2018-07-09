library(rscala)
scala()
s$showCode <- TRUE

r <- s(x=2) ^ function(y=scalaType("Double")) {
  2+3*3^2 + x + 2*y
}
r$apply(3)
r(3)
r(3L)

r <- s(x=scalaType("Double")) ^ function(y=2) {
  2+3*3^2 + x + 2*y
}
r$apply(3)

r <- s(x=3) ^ function(y=2) {
  2+3*3^2 + x + 2*y
}
r$apply()

r <- s ^ function(x=3,y=2) {
  2+3*3^2 + x + 2*y
}
r$apply()

r <- s(x=scalaType("Double")) ^ function(y=2) {
  2+3*3^2 + x + 2*y
}
r$apply(3)

r <- function() {
  2+3 * 3^2
}

rlist <- as.list(r)
rscala:::r2scala(rlist[[length(rlist)]])


r <- s %~% {
  2+3 * 3^2
}

r <- s ^ function(sigma=scalaType("Double")) {
  2+3 * sigma^2
}
r$apply(2)

r <- s(x=scalaType("Double"),mean=0, sigma=3) %~% {
  -0.5*log(2*pi*sigma^2) - 0.5/sigma^2 * (x-mean)^2
}

r <- s(x=scalaType("Double")) %~% {
  -0.5*log(2*pi*3^2) - 0.5/3^2 * (x-6)^2
}

r <- s(x=scalaType("Double"),mean=1.0,sigma=3) %~% {
  a <- 1:13
  total <- I("a.sum")
  -0.5*log(2*pi*sigma^2) - 0.5/sigma^2 * (x-mean)^2
}

teekee <- 3

r <- s(x=scalaType("Double"),mean=1.0,sigma=3) %~% {
  a <- 1:13
  total <- I('R.evalD0("teekee")')
  total
}
r$apply(1.0)

teekee <- 1
r <- s(x=scalaType("Double"),mean=1.0,sigma=3) %~% {
  a <- 1:12
  total <- 3*evalI0("teekee+%-",3) + evalI0("100*teekee")
  total
}
r$apply(1.0)
typeof(r$apply(1.0))
s$showCode <- TRUE

r <- s %~% {
  a <- 10:12
  a[3] <- a[1] + a[2]
  a[2] + a[3]
  a <- 1:100
  a[3]
}
r$apply()


cat(rscala:::r2scala(quote({
  a <- 1:4
  a[1] <- 4L
  a[1] <- 4L
}),TRUE))

cat(rscala:::r2scala(quote({
  a <- 1:4
  mean(a)
}),TRUE))

cat(rscala:::r2scala(quote({
  if ( TRUE ) cat("Yes\n")
}),TRUE))




cat(rscala:::r2scala(quote({
  for ( i in 1:10 ) {
    cat(paste0(i,"\n"))
  }
}),TRUE))

r <- s(x=scalaType("Double")) %~% {
  sum(c(T,F,T,T,F))
}

r <- s(x=scalaType("Double")) %~% {
  seq(0.0,1.0,100L)
}
r$apply(100)

r <- s(x=scalaType("Double")) %~% {
  seq(0.0,1.0,0.12)
  ceiling(1.5)
}
r$apply(100)

r <- s(x=scalaType("Int")) %~% {
  runif()
}
r$apply(100L)


r <- s(x=scalaType("Double")) %~% {
  for ( i in c(T,F,T,T,F) ) {
    cat(paste0(i,"\n"))
  }
}
r$apply(100)
s$showCode <- TRUE

r <- s(x=scalaType("Boolean")) %~% {
  answer <- if ( x ) {
    cat("Yes\n")
    "yes"
  } else {
    a <- 2
    cat(paste0("No: ",a," ",pi,"\n"))
    "no"
  }
  nchar(answer)
}
r$apply(TRUE)

  r <- s %~% {
  (1:11)[5]
}
r$apply()
s$showCode <- TRUE

r <- s(x=scalaType("Double"),mean=1.0,sigma=3) %~% {
  a <- 1:13
  total <- I('R.evalD0("teekee")')
  -0.5*log(2*pi*sigma^2) - 0.5/sigma^2 * (x-mean)^2
}

r <- s(x=scalaType("Double"),mean=0,sigma=3) %~% {
  a <- 1L:15
  total <- I('{
    val r = a.sum + x
    2*r
  }')
  a2 <- total + 1
  -0.5*log(2*pi*sigma^2) - 0.5/sigma^2 * (x-mean)^2
}

r$apply(3.0)

s$debugTranscompilation <- FALSE
s$showCode <- TRUE

