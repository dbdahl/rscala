library(rscala)

serialize <- as.logical(Sys.getenv("RSCALA_SERIALIZE"))
cat(serialize,"\n")
s <- scalaInterpreter(serialize=serialize)

s %~% "util.Properties.versionNumberString"
set.seed(943510)


scalaSettings(s,serialize=TRUE)
s$a <- rnorm(100)

s %@% 'println("Yes")'


scalaSettings(s,serialize=FALSE)
s$a <- rnorm(100)

s %@% 'println("Yes")'

