source("common.R",print.eval=TRUE)


# This is not recursion via callbacks.
f <- function(counter) {
  if ( counter >= 10 ) return(counter)
  cat("Hello",counter,"from R.\n")
  f(s %~% '
    println("Hello @{counter} from Scala.")
    R.evalI0("@{counter+1}")
  ')
}
f(0)


# This is recursion via callbacks.
g <- function(counter) {
  if ( counter >= 10 ) return(counter)
  cat("Hello",counter,"from R.\n")
  s %@% '
    println("Hello @{counter} from Scala.")
    R.eval("g(@{counter+1})")
  '
}
g(0)


# This is recursion via callbacks using a predefined function
hh <- function(x=0L) s %!% '
  println(s"Hello $x from Scala.")
  R.eval(s"h(${x+1})")
'

h <- function(counter) {
  if ( counter >= 10 ) return()
  cat("Hello",counter,"from R.\n")
  hh(counter)
}
h(0)


# This is recursion via callbacks using a Scala function
i <- function(x=0L) s %!% '
  if ( x < 10 ) {
    println(s"Hello $x from Scala.")
    R.eval(s"""cat("Hello ${x} from R.\n"); i(${x+1})""")
  }
'
i(0)



library(microbenchmark)
set.seed(13124)

microbenchmark(f(0),g(0),h(0),i(0),times=5)

cat("####\n")

microbenchmark(h(0),i(0),times=100)




# When serialize=TRUE however, we are limited by R's sink stack.
tryCatch(i(-15),error=function(e) e)

