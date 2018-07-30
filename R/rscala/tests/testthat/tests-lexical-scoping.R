context("lexical-scoping")

# skip("lexical-scoping")

y <- 2
z <- 3

a1 <- function() {
  s(x=1,y) * '
    val z = R.evalD0("z")
    x + y*z
  '
}

a2 <- s(x=1,y) ^ function() {
  z <- evalD0("z")
  x + y*z
}

mkNative <- function(x=1) {
  y1 <- y
  function() {
    x + y1*z
  }
}
a3 <- mkNative()

test_that("serialization captures output", {
  print(a1())
  expect_identical(a1(), a2())
  expect_identical(a2(), a3())
  z <- 0
  print(a1())
  expect_identical(a1(), a2())
  expect_identical(a2(), a3())
  expect_identical(local({
    z <- 29
    local({
      z <- 0
      local({
        z <- -10
        a2()
      })
    })
  }),a2())
  
})
