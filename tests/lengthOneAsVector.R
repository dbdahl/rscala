library(rscala)

serialize <- as.logical(Sys.getenv("RSCALA_SERIALIZE"))
output <- as.logical(Sys.getenv("RSCALA_OUTPUT"))
cat(serialize,"\n")
cat(output,"\n")
s <- scala(serialize=serialize,stdout=output,stderr=output)
s %~% "scala.util.Properties.versionNumberString"


scalaSettings(s,length.one.as.vector=FALSE)
s$a <- 4
s %~% 'R.pi._2'

scalaSettings(s,length.one.as.vector=TRUE)
s$a <- 4
s %~% 'R.pi._2'

