library(rscala)
scala()

r <- s %~% {
  2+3 * 3^2
}

r <- s(sigma=3) %~% {
  2+3 * sigma^2
}

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

