context("matrices")

md2by3 <- matrix(c(1,2,3,4,5,6),nrow=2)
mi2by3 <- matrix(as.integer(c(1,2,3,4,5,6)),nrow=2)
ml2by3 <- matrix(as.logical(c(1,0,1,0,1,1)),nrow=2)
mr2by3 <- matrix(as.raw(c(1,2,3,4,5,6)),nrow=2)
mc2by3 <- matrix(as.character(c(1,2,3,4,5,6)),nrow=2)

test_that("regular matrices transfer to Scala as expected by rows", {
  expect_identical(s(m=md2by3) %~% "m(0)", md2by3[1,])
  expect_identical(s(m=mi2by3) %~% "m(0)", mi2by3[1,])
  expect_identical(s(m=ml2by3) %~% "m(0)", ml2by3[1,])
  expect_identical(s(m=mr2by3) %~% "m(0)", mr2by3[1,])
  expect_identical(s(m=mc2by3) %~% "m(0)", mc2by3[1,])
})

test_that("regular matrices transfer to Scala as expected in full", {
  expect_identical(s(m=md2by3) %~% "m", md2by3)
  expect_identical(s(m=mi2by3) %~% "m", mi2by3)
  expect_identical(s(m=ml2by3) %~% "m", ml2by3)
  expect_identical(s(m=mr2by3) %~% "m", mr2by3)
  expect_identical(s(m=mc2by3) %~% "m", mc2by3)
})

md2by1 <- matrix(c(1,2),nrow=2)
mi2by1 <- matrix(as.integer(c(1,2)),nrow=2)
ml2by1 <- matrix(as.logical(c(1,0)),nrow=2)
mr2by1 <- matrix(as.raw(c(1,2)),nrow=2)
mc2by1 <- matrix(as.character(c(1,2)),nrow=2)

test_that("column vector transfer to Scala as expected by rows", {
  expect_identical(s(m=md2by1) %~% "m(0)", md2by1[1,])
  expect_identical(s(m=mi2by1) %~% "m(0)", mi2by1[1,])
  expect_identical(s(m=ml2by1) %~% "m(0)", ml2by1[1,])
  expect_identical(s(m=mr2by1) %~% "m(0)", mr2by1[1,])
  expect_identical(s(m=mc2by1) %~% "m(0)", mc2by1[1,])
})

test_that("column vector transfer to Scala as expected in full", {
  expect_identical(s(m=md2by1) %~% "m", md2by1)
  expect_identical(s(m=mi2by1) %~% "m", mi2by1)
  expect_identical(s(m=ml2by1) %~% "m", ml2by1)
  expect_identical(s(m=mr2by1) %~% "m", mr2by1)
  expect_identical(s(m=mc2by1) %~% "m", mc2by1)
})
