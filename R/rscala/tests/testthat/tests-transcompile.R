context("transcompile")

# skip("transcompile")

myExpect <- function(f,args=list(),identical=TRUE,tolerance=sqrt(.Machine$double.eps)) {
  g <- s^f
  h <- attr(g,"rscalaReferenceEnvironment")[["original"]]
  if ( ! identical(f,h) ) stop("Not identical.")
  trans <-  do.call(g,args)
  native <- do.call(f,args)
  if ( identical ) expect_identical(trans, native)
  else expect_equal(trans, native, tolerance=tolerance)
}

test_that("Basic transcompilation works", {
  myExpect(function() { numeric(4) })
  myExpect(function() { double(4) })
  myExpect(function() { integer(4) })
  myExpect(function() { logical(4) })
  myExpect(function() { character(4) })
  myExpect(function() { as.integer(1.0) })
  myExpect(function() { as.numeric(1L) })
  myExpect(function() { as.double(2L) })
  myExpect(function() { as.logical(2.0) })
  myExpect(function() { as.character(1L) })
  myExpect(function() { as.character(1.1) })
  myExpect(function() { as.integer(c(1.0,2.0)) })
  myExpect(function() { as.numeric(c(1L,2L)) })
  myExpect(function() { as.double(c(2L,3L)) })
  myExpect(function() { as.logical(c(2.0,0.0)) })
  myExpect(function() { as.character(c(1L,2L)) })
  myExpect(function() { as.character(c(1.1,2.1)) })
  myExpect(function(x=scalaType("D0")) { 1L + x }, list(3))
  myExpect(function(x=scalaType("I0")) { 1L + x }, list(3L))
  myExpect(function(x=scalaType("D0")) { 1 + x }, list(3)) 
  myExpect(function(x=scalaType("I0")) { 1 + x }, list(3L))
  myExpect(function() { 1L/2L })
  myExpect(function() { 1L %/% 2L })
  myExpect(function(x=scalaType("D0")) { abs(x) }, list(-4))
  myExpect(function(x=scalaType("D0")) { sqrt(x) }, list(4)) 
  myExpect(function(x=scalaType("D0")) { log10(x) }, list(4))
  myExpect(function(x=scalaType("D0")) { exp(x) }, list(4))
  myExpect(function(x=scalaType("D0"),y=3) { c(x,y,3,4)}, list(1))
  myExpect(function() c(T,T,F) | c(T,F,F))
  myExpect(function() c(T,T,F) & c(T,F,F))
  myExpect(function() paste(1.2,"David"))
  myExpect(function() paste0(1.2,"David"))
  myExpect(function() nchar("David"))
  myExpect(function() seq(0,1,0.13))
  myExpect(function() seq_along(c("A","Z","M","P")))
  myExpect(function() seq_len(7))
  myExpect(function() ceiling(1.45))
  myExpect(function() floor(1.45))
  myExpect(function() round(1.45))
  myExpect(function() { var <- 2; val <- 1; val + var })
  myExpect(function() c(11,12,13)[c(1,1,2,2,2,3)])
  myExpect(function() { a <- c(1,2,3)+10; a[c(1,3)] <- 3; a})
  
  f <- function(exclusive=scalaType("Boolean"), all=scalaType("Boolean")) {
    x = c(3,4,5)
    threshold <- 5
    if ( exclusive ) if ( all ) all( x < threshold ) else any( x < threshold )
    else if ( all ) all(x <= threshold) else any( x <= threshold )
  }
  myExpect(f, list(F,F))
  myExpect(f, list(T,F))
  myExpect(f, list(F,T))
  myExpect(f, list(T,T))
  
  myExpect(function(x=scalaType("Double")) {
    a <- 1:x
    for ( i in 2:length(a) ) a[i] <- a[i-1] + i
    a     
  },list(10))
  
  myExpect(function(a=scalaType("Double"), n=100, target=10) {
    sum(a / (1:n + a - 1)) - target
  },list(3),FALSE)
  
  myExpect(function(x=scalaType("Double"), mean=1, sd=3) {
    -0.5*log(2*pi*sd^2) - 0.5/sd^2 * (x-mean)^2
  },list(2.0))
  
})
 
test_that("Misc. items work as expected", {
  expect_identical(x <- {r <- s(x=scalaType("Array[Boolean]")) ^ function() { mean(x) }; r(c(T,F,T,T,F))}, mean(c(T,F,T,T,F)))
  expect_identical({r <- s(x=scalaType("Array[Boolean]")) ^ function() { mean(x) }; r(c(T,F,T,T,F))}, mean(c(T,F,T,T,F)))
  expect_identical({r <- s(x=scalaType("Array[Boolean]")) ^ function() { var(x) }; r(c(T,F,T,T,F))}, var(c(T,F,T,T,F)))
  expect_identical({r <- s(x=scalaType("Array[Boolean]")) ^ function() { sd(x) }; r(c(T,F,T,T,F))}, sd(c(T,F,T,T,F)))
  expect_identical({r <- s(x=scalaType("Double"),y=3) ^ function() { max(c(x,y,3,4))}; r(-1)}, max(c(-1,3,3,4)))
  expect_identical({r <- s(x=scalaType("Double"),y=3) ^ function() { min(c(x,y,3,4))}; r(-1)}, min(c(-1,3,3,4)))
  expect_output({r <- s(x=scalaType("String")) ^ function() { cat(x*3L) }; r("David")}, "DavidDavidDavid")
  expect_identical({r <- s(x=scalaType("Int")) ^ function() { a <- 2:x; I("a.getClass.getName") }; r(10L)}, "[I")
  expect_identical({r <- s^function(x=scalaType("D0")) seqWithLength(0,1,x); r(76)}, seq(0,1,length=76))
  expect_true({r <- s^function() random(); x <- r(); ( 0.0 < x ) && ( x < 1.0 )})
  expect_true({r <- s^function(n=scalaType("D0")) random(n); x <- r(100); all( 0.0 < x ) && all( x < 1.0 )})
})  
  
test_that("Scala snippets can be embedded", {
  expect_identical((s^function() {a=1:13; I("a.sum")})(),sum(1:13))
})

test_that("Recursion works",{
  self <- function(x=scalaType("Int")) {
    scalaType("Int")
    if ( x < 10 ) self(x+1L)
    else x+3L
  }
  expect_identical((s ^ self)(3L),self(3L))
})

test_that("The following work: return, break, next, &&, ||, %%, %/%, T, F, !=",{
  f <- function(y=scalaType("Double"), allowBreak=scalaType("Boolean"), allowNext=scalaType("Boolean"), allowReturn=scalaType("Boolean")) {
    scalaType("Double")
    x = y
    while ( x < 10 ) {
      if ( ( x %% 5 == 0 ) || ( x %/% 2 == 2 ) ) cat("@ ")
      x <- x + 1
      if ( ( allowBreak == T )  && ( x == 6 ) ) break
      if ( ( allowNext != F )   && ( x == 8 ) ) next
      cat(x," ")
      if ( allowReturn && ( x == 9 ) ) return(2*x)
    }
    x
  }
  expect_output((s^f)(3.1, T, T, T), capture.output(invisible(f(3.1, T, T, T))), fixed=TRUE)
  myExpect(f,list(3.1, T, T, T))
  expect_output((s^f)(3.1, F, T, T), capture.output(invisible(f(3.1, F, T, T))), fixed=TRUE)
  myExpect(f,list(3.1, F, T, T))
  expect_output((s^f)(3.1, F, F, T), capture.output(invisible(f(3.1, F, F, T))), fixed=TRUE)
  myExpect(f,list(3.1, F, F, T))
  expect_output((s^f)(3.1, F, F, F), capture.output(invisible(f(3.1, F, F, F))), fixed=TRUE)
  myExpect(f,list(3.1, F, F, F))
  
})
