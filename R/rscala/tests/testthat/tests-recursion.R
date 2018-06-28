context("recursion")

# skip("recursion")

f <- function(counter, verbose=FALSE) {
  if ( counter >= 10 ) return(counter)
  if ( verbose ) cat("Hello",counter,"from R.\n")
  f(s(counter=as.integer(counter[1]), verbose=verbose) * '
    if ( verbose ) println("Hello %s from Scala.".format(counter))
    R.evalI0("%- + 1L",counter)
  ', verbose=verbose)
}

g <- function(counter, verbose=FALSE) {
  if ( counter >= 10 ) return(counter)
  if ( verbose ) cat("Hello",counter,"from R.\n")
  s(counter=as.integer(counter[1]), verbose=verbose) * '
    if ( verbose ) println(s"Hello ${counter} from Scala.")
    R.evalI0("g(%-, verbose=%-)", counter+1, verbose)
  '
}

hh <- function(x, verbose) s(x=as.integer(x[1]), verbose=verbose) * '
  if ( verbose ) println(s"Hello $x from Scala.")
  R.evalI0("h(%-, verbose=%-)", x+1, verbose)
'

h <- function(counter, verbose=FALSE) {
  if ( counter >= 10 ) return(counter)
  if ( verbose ) cat("Hello",counter,"from R.\n")
  hh(counter, verbose=verbose)
}

# Note escaped \n because R doesn't have a raw strings
i <- function(x=0L, verbose=FALSE) s(x=as.integer(x[1]), verbose=verbose) * '
  if ( x < 10 ) {
    if ( verbose ) println(s"Hello $x from Scala.")
    R.evalI0(raw"""
        if ( %- ) cat("Hello $x from R.\\n")
        i( %-, verbose=%- )"""
      , verbose, x+1, verbose)
  } else x
'

test_that("recursive callbacks work", {
  expect_identical(f(0, TRUE), 10L)
  expect_identical(g(0, TRUE), 10L)
  expect_identical(h(0, TRUE), 10L)
  expect_identical(i(0, TRUE), 10L)
  expect_identical(s * 'R.evalI0("""s * "3 + 4"""")', 7L)   # Make sure everything is still okay!
})
