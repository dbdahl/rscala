context("callbacks")

# skip("callbacks")

x <- 1

test_that("primitives work in callbacks", {
  expect_identical(s(x=x) * 'R.evalI0("%-",x)', as.integer(x))
  expect_identical(s(x=I(x)) * 'R.evalI0("%-",x)', as.integer(x))
  expect_error(s(x=I(x)) * 'R.evalI0("I(%-)",x)', "^Invocation error")
  expect_identical(s(x=x) * 'R.evalD0("%-",x)', x)
  expect_identical(s(x=I(x)) * 'R.evalD0("%-",x)', x)
  expect_error(s(x=I(x)) * 'R.evalD0("I(%-)",x)', "^Invocation error")
  expect_identical(s(x=x) * 'R.evalL0("%-",x)', as.logical(x))
  expect_identical(s(x=I(x)) * 'R.evalL0("%-",x)', as.logical(x))
  expect_error(s(x=I(x)) * 'R.evalL0("I(%-)",x)', "^Invocation error")
  expect_identical(s(x=x) * 'R.evalR0("%-",x)', as.raw(x))
  expect_identical(s(x=I(x)) * 'R.evalR0("%-",x)', as.raw(x))
  expect_error(s(x=I(x)) * 'R.evalR0("I(%-)",x)', "^Invocation error")
  expect_identical(s(x=x) * 'R.evalS0("%-",x)', as.character(x))
  expect_identical(s(x=I(x)) * 'R.evalS0("%-",x)', as.character(x))
  expect_error(s(x=I(x)) * 'R.evalS0("I(%-)",x)', "^Invocation error")
})

test_that("length 1 vectors work in callbacks", {
  expect_identical(s(x=I(x)) * 'R.evalI1("I(%-)",x)', as.integer(x))
  expect_identical(s(x=I(x)) * 'R.evalD1("I(%-)",x)', x)
  expect_identical(s(x=I(x)) * 'R.evalL1("I(%-)",x)', as.logical(x))
  expect_identical(s(x=I(x)) * 'R.evalR1("I(%-)",x)', as.raw(x))
  expect_identical(s(x=I(x)) * 'R.evalS1("I(%-)",x)', as.character(x))
})

x <- c(1,2)

test_that("length 2 vectors work in callbacks", {
  expect_identical(s(x=x) * 'R.evalI1("%-",x)', as.integer(x))
  expect_identical(s(x=x) * 'R.evalD1("%-",x)', x)
  expect_identical(s(x=x) * 'R.evalL1("%-",x)', as.logical(x))
  expect_identical(s(x=x) * 'R.evalR1("%-",x)', as.raw(x))
  expect_identical(s(x=x) * 'R.evalS1("%-",x)', as.character(x))
})

x <- matrix(c(0,1),nrow=1)
xi <- x; storage.mode(xi) <- "integer"
xd <- x; storage.mode(xd) <- "double"
xl <- x; storage.mode(xl) <- "logical"
xr <- x; storage.mode(xr) <- "raw"
xc <- x; storage.mode(xc) <- "character"

test_that("matrices work in callbacks", {
  expect_identical(s(x=x) * 'R.evalI2("%-",x)', xi)
  expect_identical(s(x=x) * 'R.evalD2("%-",x)', xd)
  expect_identical(s(x=x) * 'R.evalL2("%-",x)', xl)
  expect_identical(s(x=x) * 'R.evalR2("%-",x)', xr)
  expect_identical(s(x=x) * 'R.evalS2("%-",x)', xc)
})

test_that("misc. stuff works as expected", {
  expect_error(s * 'R.eval("%-",List(1,2))',"^Invocation error")  # Unsupported type
})

myLittleF <- function() {
  x <- 5L
  s(x=x) * 'R.eval("a <- %-",x)'
  a == x
}

test_that("we evaluate in the calling R environment.", {
  x <- 5L; s(x=x) * 'R.eval("a <- %-",x)'; expect_identical(a,x)
  expect_true(myLittleF())
})
