context("transcompile")

# skip("transcompile")

test_that("Basic transcompilation works", {
  expect_identical({r <- s ^ function() { numeric(4) }; r()}, numeric(4))
  expect_identical({r <- s ^ function() { double(4) }; r()}, double(4))
  expect_identical({r <- s ^ function() { integer(4) }; r()}, integer(4))
  expect_identical({r <- s ^ function() { logical(4) }; r()}, logical(4))
  expect_identical({r <- s ^ function() { character(4) }; r()}, character(4))
  expect_identical({r <- s ^ function(x=scalaType("Double")) { 1 + 2 + x }; r(5)}, 8)
  expect_identical({r <- s ^ function(x=scalaType("Double"),y=4L) { 1 + 2 + x/y }; r(4)}, 4)
  expect_identical({r <- s ^ function(x=scalaType("Double")) { abs(x) }; r(-4)}, abs(-4))
  expect_identical({r <- s ^ function(x=scalaType("Double")) { sqrt(x) }; r(4)}, sqrt(4))
  expect_identical({r <- s ^ function(x=scalaType("Double")) { log10(x) }; r(4)}, log10(4))
  expect_identical({r <- s ^ function(x=scalaType("Double")) { exp(x) }; r(4)}, exp(4))
  expect_identical({r <- s ^ function(x=scalaType("Double"),y=3) { c(x,y,3,4)}; r(1)}, c(1,3,3,4))
  expect_identical({r <- s(x=scalaType("Array[Boolean]")) ^ function() { mean(x) }; r(c(T,F,T,T,F))}, mean(c(T,F,T,T,F)))
  expect_identical({r <- s(x=scalaType("Array[Boolean]")) ^ function() { var(x) }; r(c(T,F,T,T,F))}, var(c(T,F,T,T,F)))
  expect_identical({r <- s(x=scalaType("Array[Boolean]")) ^ function() { sd(x) }; r(c(T,F,T,T,F))}, sd(c(T,F,T,T,F)))
  expect_identical({r <- s(x=scalaType("Double"),y=3) ^ function() { max(c(x,y,3,4))}; r(-1)}, max(c(-1,3,3,4)))
  expect_identical({r <- s(x=scalaType("Double"),y=3) ^ function() { min(c(x,y,3,4))}; r(-1)}, min(c(-1,3,3,4)))
  expect_output({r <- s(x=scalaType("String")) ^ function() { cat(x*3L) }; r("David")}, "DavidDavidDavid")
  expect_identical({r <- s(x=scalaType("Int")) ^ function() { a <- 2:x; I("a.getClass.getName") }; r(10L)}, "[I")
  expect_identical({r <- s ^ function() { paste(1.2,"David") }; r()}, paste(1.2,"David"))
  expect_identical({r <- s ^ function() { paste0(1.2,"David") }; r()}, paste0(1.2,"David"))
  expect_identical({r <- s ^ function() { nchar("David") }; r()}, nchar("David"))
  expect_identical({r <- s ^ function() { seq(0,1,0.13) }; r()}, seq(0,1,0.13))
  expect_identical({r <- s ^ function() { seq(0,1,76L) }; r()}, seq(0,1,length.out=76))
  expect_identical({r <- s ^ function() { ceiling(1.45) }; r()}, ceiling(1.45))
  expect_identical({r <- s ^ function() { floor(1.45) }; r()}, floor(1.45))
  expect_identical({r <- s ^ function() { round(1.45) }; r()}, round(1.45))
  expect_true({r <- s ^ function() { runif() }; x <- r(); 0.0 <= x && x <= 1.0})
  expect_true({r <- s ^ function() { runif(10) }; x <- r(); length(x) == 10})
  expect_identical({r <- s ^ function() { as.integer(1.0) }; r()}, 1L)
  expect_identical({r <- s ^ function() { as.numeric(1L) }; r()}, 1.0)
  expect_identical({r <- s ^ function() { as.double(2L) }; r()}, 2.0)
  expect_identical({r <- s ^ function() { as.logical(2.0) }; r()}, TRUE)
  expect_identical({r <- s ^ function() { as.character(1L) }; r()}, "1")
  expect_identical({r <- s ^ function() { as.character(1.1) }; r()}, "1.1")
  expect_identical({r <- s ^ function() { as.integer(c(1.0,2.0)) }; r()}, c(1L,2L))
  expect_identical({r <- s ^ function() { as.numeric(c(1L,2L)) }; r()}, c(1.0,2.0))
  expect_identical({r <- s ^ function() { as.double(c(2L,3L)) }; r()}, c(2.0,3.0))
  expect_identical({r <- s ^ function() { as.logical(c(2.0,0.0)) }; r()}, c(TRUE,FALSE))
  expect_identical({r <- s ^ function() { as.character(c(1L,2L)) }; r()}, c("1","2"))
  expect_identical({r <- s ^ function() { as.character(c(1.1,2.1)) }; r()}, c("1.1","2.1"))
})
  
test_that("Scala snippets can be embedded", {
  expect_identical((s^function() {a=1:13; I("a.sum")})(),sum(1:13))
})

test_that("log, pi, pow work", {
  f <- function(x=scalaType("Double"), mean=1, sd=3) {
    -0.5*log(2*pi*sd^2) - 0.5/sd^2 * (x-mean)^2
  }
  expect_equal(f(2),dnorm(2, mean=1, sd=3, log=TRUE))
  expect_equal(f(2),(s^f)(2))
})
 
test_that("Scala reserved words are okay",{
  f <- function() { var <- 2; val <- 1; val + var }
  expect_identical((s^f)(),f())
})
 
test_that("Advanced subsetting works",{
  f <- function() c(11,12,13)[c(1,1,2,2,2,3)]
  expect_identical((s^f)(),f())
  f <- function() {
    a <- c(1,2,3)+10
    a[c(1,3)] <- 3
    a
  }
  expect_identical((s^f)(),f())
})
  
test_that("all, any work",{
  f <- function(exclusive=scalaType("Boolean"), all=scalaType("Boolean")) {
    x = c(3,4,5)
    threshold <- 5
    if ( exclusive ) if ( all ) all( x < threshold ) else any( x < threshold )
    else if ( all ) all(x <= threshold) else any( x <= threshold )
  }
  expect_identical((s^f)(F,F),f(F,F))
  expect_identical((s^f)(T,F),f(T,F))
  expect_identical((s^f)(F,T),f(F,T))
  expect_identical((s^f)(T,T),f(T,T))
})
   
test_that("Integer division",{
  f <- function()  c(1L / 2L, as.double(1L %/% 2L))
  expect_identical((s ^ f)(),f())
})

test_that("For loops work",{
  # For loops 
  f <- function(x=scalaType("Double")) {
    a <- 1:x
    for ( i in 2:length(a) ) a[i] <- a[i-1] + i
    a     
  }
  expect_equal((s^f)(10),f(10))
})

test_that("Vectorized calculations work",{
  f <- function(a=scalaType("Double"), n=100, target=10) {
    sum(a / (1:n + a - 1)) - target
  }
  expect_equal((s^f)(3),f(3))
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
  expect_equal( (s^f)(3.1, T, T, T),                          f(3.1, T, T, T))
  expect_output((s^f)(3.1, F, T, T), capture.output(invisible(f(3.1, F, T, T))), fixed=TRUE)
  expect_equal( (s^f)(3.1, F, T, T),                          f(3.1, F, T, T))
  expect_output((s^f)(3.1, F, F, T), capture.output(invisible(f(3.1, F, F, T))), fixed=TRUE)
  expect_equal( (s^f)(3.1, F, F, T),                          f(3.1, F, F, T))
  expect_output((s^f)(3.1, F, F, F), capture.output(invisible(f(3.1, F, F, F))), fixed=TRUE)
  expect_equal( (s^f)(3.1, F, F, F),                          f(3.1, F, F, F))
})
