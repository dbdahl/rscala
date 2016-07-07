library(rscala)

serialize <- as.logical(Sys.getenv("RSCALA_SERIALIZE"))
cat(serialize,"\n")
s <- scalaInterpreter(serialize=serialize)


s %~% "util.Properties.versionNumberString"

intpSettings(s,length.one.as.vector=FALSE)
s$a <- 4
s %~% 'R.pi._2'

intpSettings(s,length.one.as.vector=TRUE)
s$a <- 4
s %~% 'R.pi._2'

