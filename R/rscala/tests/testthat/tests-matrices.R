context("matrices")

# skip("matrices")

md2by3 <- matrix(c(1,2,3,4,5,6),nrow=2)
mi2by3 <- matrix(as.integer(c(1,2,3,4,5,6)),nrow=2)
ml2by3 <- matrix(as.logical(c(1,0,1,0,1,1)),nrow=2)
mr2by3 <- matrix(as.raw(c(1,2,3,4,5,6)),nrow=2)
mc2by3 <- matrix(as.character(c(1,2,3,4,5,6)),nrow=2)

test_that("regular matrices transfer to Scala as expected by rows", {
  expect_identical(s(m=md2by3) * "m(0)", md2by3[1,])
  expect_identical(s(m=mi2by3) * "m(0)", mi2by3[1,])
  expect_identical(s(m=ml2by3) * "m(0)", ml2by3[1,])
  expect_identical(s(m=mr2by3) * "m(0)", mr2by3[1,])
  expect_identical(s(mc2by3) * "mc2by3(0)", mc2by3[1,])
  expect_identical(s(mc2by3,letters,dummy=pi) * "val dummy2 = dummy; mc2by3(0)", mc2by3[1,])
})

test_that("regular matrices transfer to Scala as expected in full", {
  expect_identical(s(m=md2by3) * "m", md2by3)
  expect_identical(s(m=mi2by3) * "m", mi2by3)
  expect_identical(s(m=ml2by3) * "m", ml2by3)
  expect_identical(s(m=mr2by3) * "m", mr2by3)
  expect_identical(s(m=mc2by3) * "m", mc2by3)
})

md2by1 <- matrix(c(1,2),nrow=2)
mi2by1 <- matrix(as.integer(c(1,2)),nrow=2)
ml2by1 <- matrix(as.logical(c(1,0)),nrow=2)
mr2by1 <- matrix(as.raw(c(1,2)),nrow=2)
mc2by1 <- matrix(as.character(c(1,2)),nrow=2)

test_that("column vector transfer to Scala as expected by rows", {
  expect_identical(s(m=md2by1) * "m(0)", md2by1[1,])
  expect_identical(s(m=mi2by1) * "m(0)", mi2by1[1,])
  expect_identical(s(m=ml2by1) * "m(0)", ml2by1[1,])
  expect_identical(s(m=mr2by1) * "m(0)", mr2by1[1,])
  expect_identical(s(m=mc2by1) * "m(0)", mc2by1[1,])
})

test_that("column vector transfer to Scala as expected in full", {
  expect_identical(s(m=md2by1) * "m", md2by1)
  expect_identical(s(m=mi2by1) * "m", mi2by1)
  expect_identical(s(m=ml2by1) * "m", ml2by1)
  expect_identical(s(m=mr2by1) * "m", mr2by1)
  expect_identical(s(m=mc2by1) * "m", mc2by1)
})

md1by2 <- matrix(c(1,2),nrow=1)
mi1by2 <- matrix(as.integer(c(1,2)),nrow=1)
ml1by2 <- matrix(as.logical(c(1,0)),nrow=1)
mr1by2 <- matrix(as.raw(c(1,2)),nrow=1)
mc1by2 <- matrix(as.character(c(1,2)),nrow=1)

test_that("row vector transfer to Scala as expected by rows", {
  expect_identical(s(m=md1by2) * "m(0)", md1by2[1,])
  expect_identical(s(m=mi1by2) * "m(0)", mi1by2[1,])
  expect_identical(s(m=ml1by2) * "m(0)", ml1by2[1,])
  expect_identical(s(m=mr1by2) * "m(0)", mr1by2[1,])
  expect_identical(s(m=mc1by2) * "m(0)", mc1by2[1,])
})

test_that("row vector transfer to Scala as expected in full", {
  expect_identical(s(m=md1by2) * "m", md1by2)
  expect_identical(s(m=mi1by2) * "m", mi1by2)
  expect_identical(s(m=ml1by2) * "m", ml1by2)
  expect_identical(s(m=mr1by2) * "m", mr1by2)
  expect_identical(s(m=mc1by2) * "m", mc1by2)
})

md2by0 <- matrix(double(),nrow=2,ncol=0)
mi2by0 <- matrix(integer(),nrow=2,ncol=0)
ml2by0 <- matrix(logical(),nrow=2,ncol=0)
mr2by0 <- matrix(raw(),nrow=2,ncol=0)
mc2by0 <- matrix(character(),nrow=2,ncol=0)

test_that("matrix with no columns and multipe rows transfer to Scala as expected in full", {
  expect_identical(s(m=md2by0) * "m", md2by0)
  expect_identical(s(m=mi2by0) * "m", mi2by0)
  expect_identical(s(m=ml2by0) * "m", ml2by0)
  expect_identical(s(m=mr2by0) * "m", mr2by0)
  expect_identical(s(m=mc2by0) * "m", mc2by0)
})

md1by0 <- matrix(double(),nrow=1,ncol=0)
mi1by0 <- matrix(integer(),nrow=1,ncol=0)
ml1by0 <- matrix(logical(),nrow=1,ncol=0)
mr1by0 <- matrix(raw(),nrow=1,ncol=0)
mc1by0 <- matrix(character(),nrow=1,ncol=0)

test_that("matrix with no columns and one row transfer to Scala as expected in full", {
  expect_identical(s(m=md1by0) * "m", md1by0)
  expect_identical(s(m=mi1by0) * "m", mi1by0)
  expect_identical(s(m=ml1by0) * "m", ml1by0)
  expect_identical(s(m=mr1by0) * "m", mr1by0)
  expect_identical(s(m=mc1by0) * "m", mc1by0)
})

md0by2 <- matrix(double(),nrow=0,ncol=2)
mi0by2 <- matrix(integer(),nrow=0,ncol=2)
ml0by2 <- matrix(logical(),nrow=0,ncol=2)
mr0by2 <- matrix(raw(),nrow=0,ncol=2)
mc0by2 <- matrix(character(),nrow=0,ncol=2)

test_that("matrix with no rows transfer to Scala as expected in full", {
  expect_error(s(m=md0by2) * "m", "^Number of rows must be at least 1")
  expect_error(s(m=mi0by2) * "m", "^Number of rows must be at least 1")
  expect_error(s(m=ml0by2) * "m", "^Number of rows must be at least 1")
  expect_error(s(m=mr0by2) * "m", "^Number of rows must be at least 1")
  expect_error(s(m=mc0by2) * "m", "^Number of rows must be at least 1")
})

