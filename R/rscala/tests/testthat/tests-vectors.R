context("vectors")

# skip("vectors")

test_that("length 1 as primitives, bridge without a parameter list", {
  expect_identical(s * '0', 0L)
  expect_identical(s * '1', 1L)
  expect_identical(s * '0.0', 0)
  expect_identical(s * '1.0', 1)
  expect_identical(s * 'true', TRUE)
  expect_identical(s * 'false', FALSE)
  expect_identical(s * '0.toByte', as.raw(0))
  expect_identical(s * '1.toByte', as.raw(1))
  expect_identical(s * '"yes"', "yes")
  expect_identical(s * '"no"', "no")
})

test_that("length 1 as primitives, bridge with a parameter list", {
  expect_identical(s(x=0L) * 'x', 0L)
  expect_identical(s(x=1L) * 'x', 1L)
  expect_identical(s(x=0.0) * 'x', 0)
  expect_identical(s(x=1.0) * 'x', 1)
  expect_identical(s(x=TRUE) * 'x', TRUE)
  expect_identical(s(x=FALSE) * 'x', FALSE)
  expect_identical(s(x=as.raw(0)) * 'x', as.raw(0))
  expect_identical(s(x=as.raw(1)) * 'x', as.raw(1))
  expect_identical(s(x="yes") * 'x', "yes")
  expect_identical(s(x="no") * 'x', "no")
})

test_that("length 1 as vector, bridge with a parameter list", {
  expect_identical(s(x=I(0L)) * 'x(0)', 0L)
  expect_identical(s(x=I(1L)) * 'x(0)', 1L)
  expect_identical(s(x=I(0.0)) * 'x(0)', 0)
  expect_identical(s(x=I(1.0)) * 'x(0)', 1)
  expect_identical(s(x=I(TRUE)) * 'x(0)', TRUE)
  expect_identical(s(x=I(FALSE)) * 'x(0)', FALSE)
  expect_identical(s(x=I(as.raw(0))) * 'x(0)', as.raw(0))
  expect_identical(s(x=I(as.raw(1))) * 'x(0)', as.raw(1))
  expect_identical(s(x=I("yes")) * 'x(0)', "yes")
  expect_identical(s(x=I("no")) * 'x(0)', "no")
})

test_that("length 0 as primitives, bridge with a parameter list", {
  expect_identical(s(x=integer()) * 'x', integer())
  expect_identical(s(x=double()) * 'x', double())
  expect_identical(s(x=logical()) * 'x', logical())
  expect_identical(s(x=raw()) * 'x', raw())
  expect_identical(s(x=character()) * 'x', character())
})

test_that("length 3 as primitives, bridge with a parameter list", {
  expect_identical(s(x=c(0L,1L,2L)) * 'x', c(0L,1L,2L))
  expect_identical(s(x=c(0,1,2)) * 'x', c(0,1,2))
  expect_identical(s(x=c(TRUE,FALSE,TRUE)) * 'x', c(TRUE,FALSE,TRUE))
  expect_identical(s(x=as.raw(c(0,1,2))) * 'x', as.raw(c(0,1,2)))
  expect_identical(s(x=c("yes","no","yes")) * 'x', c("yes","no","yes"))
})
