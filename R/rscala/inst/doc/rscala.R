## ----include=FALSE------------------------------------------------------------
library(knitr)
opts_chunk$set(
engine='R', tidy=FALSE
)

## ----Setup document, echo=FALSE, results='hide'---------------------
options(prompt = "R> ", continue = "+  ", width = 70, useFancyQuotes = FALSE)
allowScala <- ! isTRUE(tryCatch({rscala::scalaConfig(reconfig="offline")}, error=function(e) TRUE))
a <- toString(packageVersion("rscala"))
cells <- strsplit(a,"\\.")[[1]]
versionString <- paste0(cells[1:3],collapse=".")
if ( length(cells) > 3 ) versionString <- paste0(versionString,"-SNAPSHOT")

## ----Install package, eval=FALSE------------------------------------
#  install.packages("rscala")

## ----Show supported Scala versions----------------------------------
names(rscala::scalaVersionJARs())

## ----Configure and (if necessary) install Java & Scala, eval=FALSE----
#  rscala::scalaConfig()

## ----Load library---------------------------------------------------
library("rscala")

## ----Instantiate a bridge as a user would, eval=FALSE---------------
#  s <- scala()

## ----Instantiate a bridge for this document, echo=FALSE, results='hide', eval=allowScala----
set.seed(234L)
s <- scala(serialize.output = TRUE)
s + 'scala.util.Random.setSeed(3242)'

## ----Define a function using the '+' operator, eval=allowScala------
s + '
  def binomialCoefficient(n: Int, k: Int) = {
    ( 1 to k ).map( i => ( n - i + 1 ) / i.toDouble ).product.toInt
  }
'

## ----Access a user defined function, eval=allowScala----------------
s + 'println("10 choose 3 is " + binomialCoefficient(10, 3) + ".")'

## ----Use the '*' operator, eval=allowScala--------------------------
choose(10, 3) == s * 'binomialCoefficient(10, 3)'

## ----Show equivalent matrices in R and Scala, eval=allowScala-------
fromScala <- s * 'Array(Array(1, 2, 3), Array(4, 5, 6))'
fromR     <- matrix(1:6, nrow = 2, byrow = TRUE)
identical(fromScala, fromR)

## ----Pass data to Scala, eval=allowScala----------------------------
s(name = "Hannah") * 'name.toUpperCase == name.toUpperCase.reverse'

## ----Pass data to Scala with multiple named and unnamed arguments, eval=allowScala----
names <- c("Hannah", "David", "Reinier")
s(names, convertToUpperCase = TRUE) * '
  val x = if ( convertToUpperCase ) names.map(_.toUpperCase) else names
  x.map { y => y == y.reverse }
'

## ----Use I() to pass a length-one vector to Scala as an array, eval=allowScala----
x <- letters[sample(length(letters), rbinom(1, size = 2, prob = 0.5))]
s(x = I(x)) * 'x.map(_.toUpperCase).mkString'

## ----Return a reference if the result is not a copyable type, eval=allowScala----
rng <- s * 'new scala.util.Random()'
rng

oneToTenReference <- s ^ 'Array.range(1, 11)'
oneToTenReference

## ----Pass a reference to a bridge, eval=allowScala------------------
s(rng, len = 15L) * 'rng.alphanumeric.take(len).mkString'

## ----Call a methods of a reference, eval=allowScala-----------------
rng$setSeed(24234L)
rng$nextInt(10L)
oneToTenReference$sum()

## ----Guarantee a reference is returned when calling a method, eval=allowScala----
rng$.nextInt(10L)

## ----Assess an instance variable as if it were a method, eval=allowScala----
rng$self()

## ----Show how to instantiate an object or obtain a null reference, eval=allowScala----
seed <- 123L
rng <- s$.new_java.util.Random(seed)
map <- s$".new_scala.collection.mutable.HashMap[String, Double]"()
nullString <- s$.null_String()

## ----Show more uses of the '$' operator and quote if needed, eval=allowScala----
myList <- s$List(1L, 2L, 3L)
augmentedList <- myList$':+'(100L)
paste0(augmentedList$toString(), " now contains 100.")

## ----Show even more uses of the '$' operator, eval=allowScala-------
s$binomialCoefficient(10L, 3L) == choose(10, 3)
oneToTenReference <- s$.Array.range(1L, 11L)
myScalaList <- s$List(1, 2, 3, 4)
s$scala.util.Properties.versionNumberString()

## ----Show how to chain method calls, eval=allowScala----------------
s$java.util.TimeZone.getDefault()$getDisplayName()

## ----Show how to call back from Scala using the embedded R object, eval=allowScala----
s * '
  R.eval("primes <- %-", Array(2, 3, 5, 7, 11, 13, 17, 19, 23, 29))
  val rFunction = R.evalObject("function(x) x * primes")
  val primesTimesTwo = R.evalI1("%-(2)", rFunction)
  R.evalI2("matrix(%-, nrow = %-)", primesTimesTwo, 2)
'
exists("primes")

## ----Define a function to compute expected number of clusters, eval=allowScala----
f <- function(n, alpha) sapply(alpha, function(a) sum(a / (1:n + a - 1)))
f(100, 1.0)

## ----Demonstrate Scala calling a user-defined R function, eval=allowScala----
bisection <- function(func, lower = 1.0, upper = 1.0, epsilon = 0.0000001) {
  s(lower, upper, epsilon) * '
    def g(x: Double) = R.evalD0("func(%-)", x)
    val (fLower, fUpper) = (g(lower), g(upper))
    if ( fLower * fUpper > 0 ) sys.error("Root is not straddled.")
    type D = Double
    @scala.annotation.tailrec
    def engine(l: D, u: D, fLower: D, fUpper: D): Double = {
      if ( math.abs( l - u ) <= epsilon ) ( l + u ) / 2
      else {
        val c = ( l + u ) / 2
        val fCenter = g(c)
        if ( fLower * fCenter < 0 ) engine(l, c, fLower, fCenter)
        else engine(c, u, fCenter, fUpper)
      }
    }
    engine(lower, upper, fLower, fUpper)
  '
}
bisection(function(a) f(1000, a) - 10, 0.1, 20)

## ----Show recursion between R and Scala, eval=allowScala------------
recursive.sum <- function(n) s(n) * '
  if ( n <= 0 ) 0L else n + R.evalI0("recursive.sum(%-)", n - 1)
'
recursive.sum(10)

## ----Show how subsequent calls avoid compilation for the sake of speed, eval=allowScala----
rng_rscala <- s$.new_java.util.Random()
first  <- system.time( rng_rscala$nextGaussian() )['elapsed']
second <- system.time( rng_rscala$nextGaussian() )['elapsed']
c(first = first, second = second, ratio = first / second)

## ----Benchmark against the rJava package, eval=FALSE----------------
#  library("rJava")
#  rJava::.jinit()
#  rng_rJava          <- .jnew("java.util.Random")
#  rng_rJava_LowLevel <- function() .jcall(rng_rJava, "D", "nextGaussian")
#  microbenchmark::microbenchmark(times = 1000, rng_rJava_LowLevel(),
#    rng_rscala$nextGaussian(), rng_rJava$nextGaussian())

## ----Show simplest .onLoad function, eval=FALSE---------------------
#  .onLoad <- function(libname, pkgname) {
#    assign("s", scala(), envir = parent.env(environment()))
#  }

## ----Show supported Scala versions again----------------------------
names(rscala::scalaVersionJARs())

## ----Show .onLoad function that loads the package JAR files, eval=FALSE----
#  .onLoad <- function(libname, pkgname) {
#    assign("s", scala(pkgname), envir = parent.env(environment()))
#  }

## ----Show how to use the '+' operator without blocking the R prompt, eval=FALSE----
#  .onLoad <- function(libname, pkgname) {
#    s <- scala(pkgname)
#    scalaLazy(function(s) s + 'import org.ddahl.bamboo._')
#    assign("s", s, envir = parent.env(environment()))
#  }

## ----Close the bridge when a package is unloaded, eval=FALSE--------
#  .onUnload <- function(libpath) {
#    close(s)
#  }

## ----Piggy-back on another package, eval=FALSE----------------------
#  .onLoad <- function(libname, pkgname) {
#    s <- pkg1:::s
#    assign("s", envir = parent.env(environment()))
#  }

## ----Close bridge before this document ends, echo=FALSE, results='hide', eval=allowScala----
close(s)

