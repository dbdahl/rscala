context("transcompile")

# skip("transcompile")

test_that("compilation works", {
  expect_identical({r <- s ^ function() { numeric(4) }; r()}, numeric(4))
  expect_identical({r <- s ^ function() { double(4) }; r()}, double(4))
  expect_identical({r <- s ^ function() { integer(4) }; r()}, integer(4))
  expect_identical({r <- s ^ function() { logical(4) }; r()}, logical(4))
  expect_identical({r <- s ^ function() { character(4) }; r()}, character(4))
  expect_identical({r <- s ^ function() { 1 + 2 }; r()}, 3)
  expect_identical({r <- s ^ function(x=scalaType("Double")) { 1 + 2 + x }; r(5)}, 8)
  expect_identical({r <- s ^ function(x=scalaType("Double"),y=4L) { 1 + 2 + x/y }; r(4)}, 4)
  expect_identical({r <- s ^ function(x=scalaType("Double")) { pi }; r(-4)}, pi)
  expect_identical({r <- s ^ function(x=scalaType("Double")) { T }; r(-4)}, TRUE)
  expect_identical({r <- s ^ function(x=scalaType("Double")) { F }; r(-4)}, FALSE)
  expect_identical({r <- s ^ function(x=scalaType("Double")) { abs(x) }; r(-4)}, abs(-4))
  expect_identical({r <- s ^ function(x=scalaType("Double")) { sqrt(x) }; r(4)}, sqrt(4))
  expect_identical({r <- s ^ function(x=scalaType("Double")) { log(x) }; r(4)}, log(4))
  expect_identical({r <- s ^ function(x=scalaType("Double")) { log10(x) }; r(4)}, log10(4))
  expect_identical({r <- s ^ function(x=scalaType("Double")) { exp(x) }; r(4)}, exp(4))
  expect_identical({r <- s ^ function(x=scalaType("Double"),y=3) { x^y }; r(4)}, 4^3)
  expect_identical({r <- s ^ function(x=scalaType("Double"),y=3) { c(x,y,3,4)}; r(1)}, c(1,3,3,4))
  expect_identical({r <- s ^ function(x=scalaType("Double"),y=3) { length(c(x,y,3,4))}; r(1)}, length(c(1,3,3,4)))
  expect_identical({r <- s ^ function(x=scalaType("Double"),y=3) { sum(c(x,y,3,4))}; r(1)}, sum(c(1,3,3,4)))
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
  expect_identical({r <- s ^ function() { 1:10 }; r()}, 1:10)
  expect_identical({r <- s ^ function() { seq(0,1,0.13) }; r()}, seq(0,1,0.13))
  expect_identical({r <- s ^ function() { seq(0,1,76L) }; r()}, seq(0,1,length.out=76))
  expect_identical({r <- s ^ function() { ceiling(1.45) }; r()}, ceiling(1.45))
  expect_identical({r <- s ^ function() { floor(1.45) }; r()}, floor(1.45))
  expect_identical({r <- s ^ function() { round(1.45) }; r()}, round(1.45))
  expect_true({r <- s ^ function() { runif() }; x <- r(); 0.0 <= x && x <= 1.0})
  expect_true({r <- s ^ function() { runif(10) }; x <- r(); length(x) == 10})
  expect_identical({r <- s(x=scalaType("Boolean")) ^ function() { if ( x ) 1.0 else 0.0 }; r(TRUE)}, 1.0)
  expect_identical({r <- s(x=scalaType("Boolean")) ^ function() { if ( x ) 1.0 else 0.0 }; r(FALSE)}, 0.0)
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
  expect_identical({
      r <- s(x=scalaType("Double")) ^ function() {
        a <- 1:x
        for ( i in 2:length(a) ) {
          a[i] <- a[i-1] + i
        }
        a
      }
      r(10)
    },
    {
      f <- function(x) {
        a <- 1:x
        for ( i in 2:length(a) ) {
          a[i] <- a[i-1] + i
        }
        a     
      }
      f(10)
    }
  )
})
