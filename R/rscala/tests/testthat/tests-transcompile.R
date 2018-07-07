context("transcompile")

# skip("transcompile")

test_that("compilation works", {
  expect_identical({r <- s %~% { 1 + 2 }; r$apply()}, 3)
  expect_identical({r <- s(x=scalaType("Double")) %~% { 1 + 2 + x }; r$apply(5)}, 8)
  expect_identical({r <- s(x=scalaType("Double"),y=4L) %~% { 1 + 2 + x/y }; r$apply(4)}, 4)
  expect_identical({r <- s(x=scalaType("Double")) %~% { pi }; r$apply(-4)}, pi)
  expect_identical({r <- s(x=scalaType("Double")) %~% { T }; r$apply(-4)}, TRUE)
  expect_identical({r <- s(x=scalaType("Double")) %~% { F }; r$apply(-4)}, FALSE)
  expect_identical({r <- s(x=scalaType("Double")) %~% { abs(x) }; r$apply(-4)}, abs(-4))
  expect_identical({r <- s(x=scalaType("Double")) %~% { sqrt(x) }; r$apply(4)}, sqrt(4))
  expect_identical({r <- s(x=scalaType("Double")) %~% { log(x) }; r$apply(4)}, log(4))
  expect_identical({r <- s(x=scalaType("Double")) %~% { log10(x) }; r$apply(4)}, log10(4))
  expect_identical({r <- s(x=scalaType("Double")) %~% { exp(x) }; r$apply(4)}, exp(4))
  expect_identical({r <- s(x=scalaType("Double"),y=3) %~% { x^y }; r$apply(4)}, 4^3)
  expect_identical({r <- s(x=scalaType("Double"),y=3) %~% { c(x,y,3,4)}; r$apply(1)}, c(1,3,3,4))
  expect_identical({r <- s(x=scalaType("Double"),y=3) %~% { length(c(x,y,3,4))}; r$apply(1)}, length(c(1,3,3,4)))
  expect_identical({r <- s(x=scalaType("Double"),y=3) %~% { sum(c(x,y,3,4))}; r$apply(1)}, sum(c(1,3,3,4)))
  expect_identical({r <- s(x=scalaType("Array[Boolean]")) %~% { mean(x) }; r$apply(c(T,F,T,T,F))}, mean(c(T,F,T,T,F)))
  expect_identical({r <- s(x=scalaType("Array[Boolean]")) %~% { var(x) }; r$apply(c(T,F,T,T,F))}, var(c(T,F,T,T,F)))
  expect_identical({r <- s(x=scalaType("Array[Boolean]")) %~% { sd(x) }; r$apply(c(T,F,T,T,F))}, sd(c(T,F,T,T,F)))
  expect_identical({r <- s(x=scalaType("Double"),y=3) %~% { max(c(x,y,3,4))}; r$apply(-1)}, max(c(-1,3,3,4)))
  expect_identical({r <- s(x=scalaType("Double"),y=3) %~% { min(c(x,y,3,4))}; r$apply(-1)}, min(c(-1,3,3,4)))
  expect_output({r <- s(x=scalaType("String")) %~% { cat(x*3L) }; r$apply("David")}, "DavidDavidDavid")
  expect_identical({r <- s(x=scalaType("Int")) %~% { a <- 2:x; I("a.getClass.getName") }; r$apply(10L)}, "[I")
  expect_identical({r <- s %~% { paste(1.2,"David") }; r$apply()}, paste(1.2,"David"))
  expect_identical({r <- s %~% { paste0(1.2,"David") }; r$apply()}, paste0(1.2,"David"))
  expect_identical({r <- s %~% { nchar("David") }; r$apply()}, nchar("David"))
  expect_identical({r <- s %~% { 1:10 }; r$apply()}, 1:10)
  expect_identical({r <- s %~% { seq(0,1,0.13) }; r$apply()}, seq(0,1,0.13))
  expect_identical({r <- s %~% { seq(0,1,76L) }; r$apply()}, seq(0,1,length.out=76))
  expect_identical({r <- s %~% { ceiling(1.45) }; r$apply()}, ceiling(1.45))
  expect_identical({r <- s %~% { floor(1.45) }; r$apply()}, floor(1.45))
  expect_identical({r <- s %~% { round(1.45) }; r$apply()}, round(1.45))
  expect_true({r <- s %~% { runif() }; x <- r$apply(); 0.0 <= x && x <= 1.0})
  expect_true({r <- s %~% { runif(10) }; x <- r$apply(); length(x) == 10})
  expect_identical({r <- s(x=scalaType("Boolean")) %~% { if ( x ) 1.0 else 0.0 }; r$apply(TRUE)}, 1.0)
  expect_identical({r <- s(x=scalaType("Boolean")) %~% { if ( x ) 1.0 else 0.0 }; r$apply(FALSE)}, 0.0)
  expect_identical({r <- s %~% { as.integer(1.0) }; r$apply()}, 1L)
  expect_identical({r <- s %~% { as.numeric(1L) }; r$apply()}, 1.0)
  expect_identical({r <- s %~% { as.double(2L) }; r$apply()}, 2.0)
  expect_identical({r <- s %~% { as.logical(2.0) }; r$apply()}, TRUE)
  expect_identical({r <- s %~% { as.character(1L) }; r$apply()}, "1")
  expect_identical({r <- s %~% { as.character(1.1) }; r$apply()}, "1.1")
  expect_identical({r <- s %~% { as.integer(c(1.0,2.0)) }; r$apply()}, c(1L,2L))
  expect_identical({r <- s %~% { as.numeric(c(1L,2L)) }; r$apply()}, c(1.0,2.0))
  expect_identical({r <- s %~% { as.double(c(2L,3L)) }; r$apply()}, c(2.0,3.0))
  expect_identical({r <- s %~% { as.logical(c(2.0,0.0)) }; r$apply()}, c(TRUE,FALSE))
  expect_identical({r <- s %~% { as.character(c(1L,2L)) }; r$apply()}, c("1","2"))
  expect_identical({r <- s %~% { as.character(c(1.1,2.1)) }; r$apply()}, c("1.1","2.1"))
  expect_identical({
      r <- s(x=scalaType("Int")) %~% {
        a <- 1:x
        for ( i in 2:length(a) ) {
          a[i] <- a[i-1] + i
        }
        a
      }
      r$apply(10L)
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