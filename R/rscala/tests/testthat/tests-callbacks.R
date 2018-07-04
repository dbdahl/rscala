context("callbacks")

# skip("callbacks")

test_that("primitives work in callbacks", {
  x <- as.integer(1); expect_identical(s(x=x) * 'R.evalI0("%-",x)', x)
  x <- as.integer(1); expect_identical(s(x=I(x)) * 'R.evalI0("%-",x)', x)
  x <- as.integer(1); expect_error(s(x=I(x)) * 'R.evalI0("I(%-)",x)', "^Invocation error")
  x <- as.double(1); expect_identical(s(x=x) * 'R.evalD0("%-",x)', x)
  x <- as.double(1); expect_identical(s(x=I(x)) * 'R.evalD0("%-",x)', x)
  x <- as.double(1); expect_error(s(x=I(x)) * 'R.evalD0("I(%-)",x)', "^Invocation error")
  x <- as.logical(1); expect_identical(s(x=x) * 'R.evalL0("%-",x)', x)
  x <- as.logical(1); expect_identical(s(x=I(x)) * 'R.evalL0("%-",x)', x)
  x <- as.logical(1); expect_error(s(x=I(x)) * 'R.evalL0("I(%-)",x)', "^Invocation error")
  x <- as.raw(1); expect_identical(s(x=x) * 'R.evalR0("%-",x)', x)
  x <- as.raw(1); expect_identical(s(x=I(x)) * 'R.evalR0("%-",x)', x)
  x <- as.raw(1); expect_error(s(x=I(x)) * 'R.evalR0("I(%-)",x)', "^Invocation error")
  x <- as.character(1); expect_identical(s(x=x) * 'R.evalS0("%-",x)', x)
  x <- as.character(1); expect_identical(s(x=I(x)) * 'R.evalS0("%-",x)', x)
  x <- as.character(1); expect_error(s(x=I(x)) * 'R.evalS0("I(%-)",x)', "^Invocation error")
})

test_that("length 1 vectors work in callbacks", {
  x <- as.integer(1); expect_identical(s(x=I(x)) * 'R.evalI1("I(%-)",x)', x)
  x <- as.double(1); expect_identical(s(x=I(x)) * 'R.evalD1("I(%-)",x)', x)
  x <- as.logical(1); expect_identical(s(x=I(x)) * 'R.evalL1("I(%-)",x)', x)
  x <- as.raw(1); expect_identical(s(x=I(x)) * 'R.evalR1("I(%-)",x)', x)
  x <- as.character(1); expect_identical(s(x=I(x)) * 'R.evalS1("I(%-)",x)', x)
})

test_that("length 2 vectors work in callbacks", {
  x <- as.integer(c(1,2)); expect_identical(s(x=x) * 'R.evalI1("%-",x)', x)
  x <- as.double(c(1,2)); expect_identical(s(x=x) * 'R.evalD1("%-",x)', x)
  x <- as.logical(c(1,2)); expect_identical(s(x=x) * 'R.evalL1("%-",x)', x)
  x <- as.raw(c(1,2)); expect_identical(s(x=x) * 'R.evalR1("%-",x)', x)
  x <- as.character(c(1,2)); expect_identical(s(x=x) * 'R.evalS1("%-",x)', x)
})

test_that("matrices work in callbacks", {
  x <- matrix(as.integer(c(1,2)),nrow=1); expect_identical(s(x=x) * 'R.evalI2("%-",x)', x)
  x <- matrix(as.double(c(1,2)),nrow=1); expect_identical(s(x=x) * 'R.evalD2("%-",x)', x)
  x <- matrix(as.logical(c(0,1)),nrow=1); expect_identical(s(x=x) * 'R.evalL2("%-",x)', x)
  x <- matrix(as.raw(c(1,2)),nrow=1); expect_identical(s(x=x) * 'R.evalR2("%-",x)', x)
  x <- matrix(as.character(c(1,2)),nrow=1); expect_identical(s(x=x) * 'R.evalS2("%-",x)', x)
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
