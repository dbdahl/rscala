context("robjects")

# skip("robjects")

test_that("R objects serialization works", {
  y <- s - '0'; expect_identical('0', -y)
  y <- s - rnorm; expect_identical(rnorm, -y)
})
