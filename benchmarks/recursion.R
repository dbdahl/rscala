library(rscala)

s <- scalaInterpreter()

# This is not true recursion.
f <- function(counter) {
  if ( counter >= 10 ) return()
  cat("Hello",counter,"from R.\n")
  f(s %~% '
    println("Hello "+@{counter}+" from Scala.")
    R.evalI0("@{counter+1}")
  ')
}


# This is true recursion.
g <- function(counter) {
  if ( counter >= 10 ) return()
  cat("Hello",counter,"from R.\n")
  s %@% '
    println("Hello "+@{counter}+" from Scala.")
    R.eval("g(@{counter+1})")
  '
}


library(microbenchmark)

microbenchmark(f(0),g(0),times=5)

