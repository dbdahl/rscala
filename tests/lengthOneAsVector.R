library(rscala)

serialize <- as.logical(Sys.getenv("RSCALA_SERIALIZE"))
cat(serialize,"\n")
s <- scala(serialize=serialize)

s %~% "scala.util.Properties.versionNumberString"

scalaSettings(s,length.one.as.vector=FALSE)
s$a <- 4
s %~% 'R.pi._2'

scalaSettings(s,length.one.as.vector=TRUE)
s$a <- 4
s %~% 'R.pi._2'

