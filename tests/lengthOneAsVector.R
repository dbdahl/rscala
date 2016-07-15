library(rscala)

serialize <- as.logical(Sys.getenv("RSCALA_SERIALIZE"))
output <- as.logical(Sys.getenv("RSCALA_OUTPUT"))
version <- Sys.getenv("RSCALA_SCALA_VERSION")
s <- scala(serialize=serialize,stdout=output,stderr=output)
if ( version != s %~% "scala.util.Properties.versionNumberString" ) stop("Version mismatch.")

scalaSettings(s,length.one.as.vector=FALSE)
s$a <- 4
s %~% 'R.pi._2'

scalaSettings(s,length.one.as.vector=TRUE)
s$a <- 4
s %~% 'R.pi._2'

