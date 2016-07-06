library(rscala)

s <- scalaInterpreter()

# This is not recursion via callbacks.
f <- function(counter) {
  if ( counter >= 10 ) return(counter)
  cat("Hello",counter,"from R.\n")
  f(s %~% '
    println("Hello from Scala.")
    R.evalI0("@{counter+1}")
  ')
}


# This is recursion via callbacks.
g <- function(counter) {
  if ( counter >= 10 ) return(counter)
  cat("Hello",counter,"from R.\n")
  s %@% '
    println("Hello @{counter} from Scala.")
    R.eval("g(@{counter+1})")
  '
}


# This is recursion via callbacks using predefined functions
hh <- s$def('x: Int','
  println(s"Hello $x from Scala.")
  R.eval(s"h(${x+1})")
')

h <- function(counter) {
  if ( counter >= 10 ) return()
  cat("Hello",counter,"from R.\n")
  hh(counter)
}


# This is very cool!
i <- s$def('x: Int','
  if ( x < 10 ) {
    println(s"Hello $x from Scala.")
    R.eval(s"""cat("Hello ${x+1} from R."); i(${x+1})""")
  }
')



library(microbenchmark)

microbenchmark(f(0),g(0),h(0),i(0),times=5)

