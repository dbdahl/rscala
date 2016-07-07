library(rscala)

serialize <- as.logical(Sys.getenv("RSCALA_SERIALIZE"))
cat(serialize,"\n")
s <- scalaInterpreter(c("commons-math3-3.2.jar","shallot.jar"),serialize=serialize)

s %~% "util.Properties.versionNumberString"

scalap(s,"org.apache.commons.math3.random.RandomDataGenerator")
rdg <- tryCatch(s$do("org.apache.commons.math3.random.RandomDataGenerator")$new(),error=function(e) e)  # There is some incompatability between Scala REPL classloader and the Apache Commons Math jar.
rdg <- s$do("org.apache.commons.math3.random.RandomDataGenerator")$new()                                # Rerun and it works fine.
rexp <- rdg$nextExponential(2,evaluate=FALSE)

library(microbenchmark)
options(width=120)
microbenchmark(rdg$nextExponential(4),rexp(4),times=1000L)
microbenchmark(rdg$nextExponential(4),rexp(4),times=1000L)
microbenchmark(rdg$nextExponential(4),rexp(4),times=1000L)

massFactory3 <- s$do("org.ddahl.shallot.parameter.Mass")$factory(1.0,3.0,s$do("org.apache.commons.math3.random.RandomDataGenerator")$new())
s %~% "3+4"

scalap(s,"org.ddahl.shallot.parameter.Mass")
mass <- s$do("org.ddahl.shallot.parameter.Mass")$apply(3.4)
massFactory1 <- s$do("org.ddahl.shallot.parameter.Mass")$factory(mass)
massFactory2 <- s$do("org.ddahl.shallot.parameter.Mass")$factory(3.0)
massFactory3 <- s$do("org.ddahl.shallot.parameter.Mass")$factory(1.0,3.0,rdg)
massFactory3$apply()$logValue()
massFactory3$apply()$logValue()
massFactory3$apply()$logValue()



s %~% '
class Bob {

  val a = 3.0
  var b = 4.0

  def sum(c: Int) = { a + b + c }

}
'

d <- s$do("Bob")$new()
d[['type']]                           # Note the weird 'iw$' prepended to 'Bob'
tryCatch(d$b(),error=function(e) e)   # Doesn't work

d[['type']] <- "Bob"     # Cast it to be 'Bob'
d$b()                    # Now it does

d$sum(as.integer(4))
m <- d$sum(as.integer(5),evaluate=FALSE)

m(3)                                    # Casting is not necessary here
d$sum(as.integer(3))                    # But it is here
tryCatch(d$sum(3),error=function(e) e)  # and here

