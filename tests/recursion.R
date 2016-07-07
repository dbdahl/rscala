library(rscala)

serialize <- as.logical(Sys.getenv("RSCALA_SERIALIZE"))
serialize <- FALSE
cat(serialize,"\n")
s <- scalaInterpreter(serialize=serialize)

cat(s %~% "util.Properties.versionNumberString","\n")



# This is not recursion via callbacks.
f <- function(counter) {
  if ( counter >= 10 ) return(counter)
  cat("Hello",counter,"from R.\n")
  f(s %~% '
    println("Hello @{counter+1} from Scala.")
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
h(0)


# This is very cool!
i <- s$def('x: Int','
  if ( x < 10 ) {
    println(s"Hello $x from Scala.")
    R.eval(s"""cat("Hello ${x+1} from R.\n"); i(${x+1})""")
  }
')
i(0)



library(microbenchmark)
set.seed(13124)

microbenchmark(f(0),g(0),h(0),i(0),times=5)

cat("####\n")

microbenchmark(h(0),i(0),times=100)




# When serialize=TRUE however, we are limited by R's sink stack.
serializeOriginal <- intpSettings(s)$serialize
intpSettings(s,serialize=FALSE)
i(-15)                                # This is okay because we are not serializing.

intpSettings(s,serialize=TRUE)
tryCatch(i(-15),error=function(e) e)  # But this causes an error because of R's limited sink stack.

intpSettings(s,serialize=serializeOriginal)

# And we never fully recover, as evidenced by the exception below.
close(s)

