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

f(0)


# This would be true recursion, but it does not work!
f <- function(counter) {
  if ( counter >= 10 ) return()
  cat("Hello",counter,"from R.\n")
  s %@% '
    println("Hello "+@{counter}+" from Scala.")
    R.eval("f(@{counter+1})")
  '
}


f(0)

