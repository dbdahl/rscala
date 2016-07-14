library(rscala)

scalaInfo(scala.home="~/local/scala/scala-2.10.6",verbose=TRUE)
scalaInfo(scala.home="~/local/scala/scala-2.11.8",verbose=TRUE)
scalaInfo(scala.home="~/local/scala/scala-2.12.0-M5",verbose=TRUE)

serialize <- TRUE
debug <- FALSE
output <- FALSE

s10 <- scala(scala.home="~/local/scala/scala-2.10.6",debug=debug,serialize=serialize,stdout=output,stderr=output)
s11 <- scala(scala.home="~/local/scala/scala-2.11.8",debug=debug,serialize=serialize,stdout=output,stderr=output)
s12 <- scala(scala.home="~/local/scala/scala-2.12.0-M5",debug=debug,serialize=serialize,stdout=output,stderr=output)

s10 %~% "scala.util.Properties.versionNumberString"
s11 %~% "scala.util.Properties.versionNumberString"
s12 %~% "scala.util.Properties.versionNumberString"

s10 %@% "3+asd"
s11 %@% "3+asd"
s12 %@% "3+asd"

capture.output(s10 %@% 'println("Hi s10")')
capture.output(s11 %@% 'println("Hi s11")')
capture.output(s12 %@% 'println("Hi s12")')

