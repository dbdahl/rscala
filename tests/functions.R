jars <- c("commons-math3-3.2.jar","shallot.jar")
source("common.R",print.eval=TRUE)

tryCatch(s %~% 'new org.apache.commons.math3.random.EmpiricalDistribution()',error=function(e) e)   # Scala 2.11.x class loader is weird.  This line avoid subsequent problems.


scalap(s,"org.apache.commons.math3.random.RandomDataGenerator")
s$.org.apache.commons.math3.random.RandomDataGenerator
rdg <- s$.org.apache.commons.math3.random.RandomDataGenerator$new()

rdg$reSeed(39234L)
rexp <- rdg$nextExponential(2,.EVALUATE=FALSE)

library(microbenchmark)
options(width=120)
microbenchmark(rdg$nextExponential(4),rexp(4),times=1000L)
microbenchmark(rdg$nextExponential(4),rexp(4),times=1000L)
microbenchmark(rdg$nextExponential(4),rexp(4),times=1000L)

s$.org.ddahl.shallot.parameter.Mass
massFactory3 <- s$.org.ddahl.shallot.parameter.Mass$factory(1.0,3.0,rdg)
s %~% "3+4"

mass <- s$.org.ddahl.shallot.parameter.Mass$apply(3.4)
massFactory1 <- s$.org.ddahl.shallot.parameter.Mass$factory(mass)
massFactory2 <- s$.org.ddahl.shallot.parameter.Mass$factory(3.0)
massFactory3 <- s$.org.ddahl.shallot.parameter.Mass$factory(1.0,3.0,rdg)
massFactory3$apply()$logValue()
massFactory3$apply()$logValue()
massFactory3$apply()$logValue()



s %@% '
class Bob {

  val a = 3.0
  var b = 4.0

  def sum(c: Int) = { a + b + c }

}
'

d <- s$.Bob$new()
d[['type']]                           # Note the weird 'iw$' prepended to 'Bob'
tryCatch(d$b(),error=function(e) e)   # Doesn't work

d[['type']] <- "Bob"     # Cast it to be 'Bob'
d$b()                    # Now it does

d$sum(4L)
m <- d$sum(5L,.EVALUATE=FALSE)

m(3)                                                   # Casting is not necessary here
d$sum(as.integer(3))                                   # But it is here

