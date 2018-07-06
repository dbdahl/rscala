context("transcompile")

# skip("transcompile")

test_that("compilation works", {
  expect_identical({r <- s %~% { 1 + 2 }; r$apply()}, 3)
  expect_identical({r <- s(x=scalaType("Double")) %~% { 1 + 2 + x }; r$apply(5)}, 8)
  expect_identical({r <- s(x=scalaType("Double")) %~% { 1 + 2 + x }; r$apply(0)}, 3)
  expect_identical({r <- s(x=scalaType("Double"),y=4L) %~% { 1 + 2 + x/y }; r$apply(0)}, 3)
  expect_identical({r <- s(x=scalaType("Double"),y=4L) %~% { 1 + 2 + x/y }; r$apply(4)}, 4)
  expect_identical({r <- s(x=scalaType("Double")) %~% { log(x) }; r$apply(4)}, log(4))
  expect_identical({r <- s(x=scalaType("Int")) %~% { I("a: Array[Int]") <- 1:x; I("a.sum") }; r$apply(10L)}, sum(1:10))
})
