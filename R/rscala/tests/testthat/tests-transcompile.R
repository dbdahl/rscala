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
  myExpect(function(x=stD0) { 1L + x }, list(3))
  myExpect(function(x=stI0) { 1L + x }, list(3L))
  myExpect(function(x=stD0) { 1 + x }, list(3)) 
  myExpect(function(x=stI0) { 1 + x }, list(3L))
  myExpect(function() { 1L/2L })
  myExpect(function() { 1L %/% 2L })
  myExpect(function(x=stD0) { abs(x) }, list(-4))
  myExpect(function(x=stD0) { sqrt(x) }, list(4)) 
  myExpect(function(x=stD0) { log10(x) }, list(4))
  myExpect(function(x=stD0) { exp(x) }, list(4))
  myExpect(function(x=stD0,y=3) { c(x,y,3,4)}, list(1))
  myExpect(function() c(T,T,F) | c(T,F,F))
  myExpect(function() c(T,T,F) & c(T,F,F))
  myExpect(function() paste(1.2,"David"))
  myExpect(function() paste0(1.2,"David"))
  myExpect(function() nchar("David"))
  myExpect(function() seq(0,1,0.13))
  myExpect(function() seq(0,1,by=0.23))
  myExpect(function() seq(0,1,length.out=56))
  myExpect(function() seq(0,1,length.out=56.3))
  myExpect(function() seq(0,1,length.out=56.7))
  myExpect(function() seq_along(c("A","Z","M","P")))
  myExpect(function() seq_len(7.3))
  myExpect(function() seq_len(7.8))
  myExpect(function() rep(7,3))
  myExpect(function() rep(c(2,7),times=c(3.1,4.7)))
  myExpect(function() rep(c(2,7),each=3.1))
  myExpect(function() rep(c(2,7),each=3.6))
  myExpect(function() ceiling(1.45))
  myExpect(function() floor(1.45))
  myExpect(function() round(1.45))
  myExpect(function() { var <- 2; val <- 1; val + var })
  myExpect(function() { c(1,c(3L,4L)) })
  myExpect(function() { c(1L,c(3,4L)) })
  myExpect(function() { c("David",3L) })
  myExpect(function() { c(c(1.2,3.2),"David") })
  myExpect(function() { order(c(1.2,3.2)) })
  myExpect(function() { order(c("e","da","jd","d"),decreasing=TRUE) })
  myExpect(function() { a <- c("e","da","jd","d"); a[order(a,decreasing=TRUE)] })
  myExpect(function() { a <- as.integer(c(4,2,9,2,1,3,9,1)); a[order(a)] })
})

test_that("Apply and update work", {
  myExpect(function() c(11,12,13)[c(1,1,2,2,2,3)])
  myExpect(function() c(11L,12L,13L)[2])
  myExpect(function() { a <- c(1L,2L,3L); a[c(1,3)] <- 3L; a})
  myExpect(function() { a <- c(1,2,3); a[3] <- 3; a})
  myExpect(function(i=stI0,let=letters) let[i],list(2))
  myExpect(function(i=stD0,let=letters) let[i],list(2))
  myExpect(function(i=stI1,let=letters) let[i],list(c(2,3)))
  myExpect(function(i=stD1,let=letters) let[i],list(c(2,3)))
  myExpect(function(i=stL1,let=letters) let[i],list(c(TRUE,FALSE,rep(c(TRUE,FALSE,TRUE),times=8))))
  myExpect(function(i=stI0,let=c(T,T,F,T,F)) let[i],list(2))
  myExpect(function(i=stI1,let=c(T,T,F,T,F)) let[i],list(c(2,3)))
  myExpect(function(i=stI0,let=letters) { let[i] <- "3"; let[i] },list(2))
  myExpect(function(i=stI1,let=letters) { let[i] <- "3"; let[i] },list(c(2L,3L)))
  myExpect(function(i=stL1,let=letters) { let[i] <- "3"; let[i] },list(c(TRUE,FALSE,rep(c(TRUE,FALSE,TRUE),times=8))))
  myExpect(function(i=stI0,let=c(T,T,F,T,F)) { let[i] <- TRUE; let[i] },list(2))
  myExpect(function(i=stI1,let=c(T,T,F,T,F)) { let[i] <- FALSE; let[i] },list(c(2,3)))
})

test_that("More basic transcompilation works", {
  
  f <- function(exclusive=scalaType("Boolean"), all=stL0) {
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
  expect_identical(x <- {r <- s() ^ function(x=scalaType("Array[Boolean]")) { mean(x) }; r(c(T,F,T,T,F))}, mean(c(T,F,T,T,F)))
  expect_identical({r <- s ^ function(x=scalaType("Array[Boolean]")) { mean(x) }; r(c(T,F,T,T,F))}, mean(c(T,F,T,T,F)))
  expect_equal({r <- s ^ function(x=stL1) { var(x) }; r(c(T,F,T,T,F))}, var(c(T,F,T,T,F)))
  expect_equal({r <- s ^ function(x=scalaType("Array[Boolean]")) { sd(x) }; r(c(T,F,T,T,F))}, sd(c(T,F,T,T,F)))
  expect_identical({r <- s ^ function(x=stD0,y=3) { max(c(x,y,3,4))}; r(-1)}, max(c(-1,3,3,4)))
  expect_identical({r <- s(y=3) ^ function(x=scalaType("Double")) { min(c(x,y,3,4))}; r(-1)}, min(c(-1,3,3,4)))
  expect_output({r <- s() ^ function(x=scalaType("String")) { cat(x*3L) }; r("David")}, "DavidDavidDavid")
  expect_identical({r <- s() ^ function(x=scalaType("Int")) { a <- 2:x; I("a.getClass.getName") }; r(10L)}, "[I")
  expect_true({r <- s ^ function() random(); x <- r(); ( 0.0 < x ) && ( x < 1.0 )})
  expect_true({r <- s ^ function(n=stD0) random(n); x <- r(100); all( 0.0 < x ) && all( x < 1.0 )})
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
