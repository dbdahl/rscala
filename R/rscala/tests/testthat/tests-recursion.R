context("recursion")

# skip("recursion")

f <- function(counter) {
  if ( counter >= 10 ) return(counter)
  # cat("Hello",counter,"from R.\n")
  f(s(counter=as.integer(counter[1])) * '
    // println("Hello %s from Scala.".format(counter))
    R.evalI0("%- + 1L",counter)
  ')
}

g <- function(counter) {
  if ( counter >= 10 ) return(counter)
  # cat("Hello",counter,"from R.\n")
  s(counter=as.integer(counter[1])) * '
    // println(s"Hello ${counter} from Scala.")
    R.evalI0("g(%-)",counter+1)
  '
}

hh <- function(x) s(x=as.integer(x[1])) * '
  // println(s"Hello $x from Scala.")
  R.evalI0("h(%-)",x+1)
'

h <- function(counter) {
  if ( counter >= 10 ) return(counter)
  # cat("Hello",counter,"from R.\n")
  hh(counter)
}

# Note escaped \n because R doesn't have a raw strings
i <- function(x=0L) s(x=as.integer(x[1])) * '
  if ( x < 10 ) {
    // println(s"Hello $x from Scala.")
    R.evalI0(raw"""
        # cat("Hello $x from R.\\n")
        i(%-)"""
      ,x+1)
  } else x
'

test_that("recursive callbacks work", {
  expect_identical(f(0), 10L)
  expect_identical(g(0), 10L)
  expect_identical(h(0), 10L)
  expect_identical(i(0), 10L)
  expect_identical(s * 'R.evalI0("""s * "3 + 4"""")', 7L)
})
