library(rscala)

serialize <- as.logical(Sys.getenv("RSCALA_SERIALIZE"))
output <- as.logical(Sys.getenv("RSCALA_OUTPUT"))
cat(serialize,"\n")
cat(output,"\n")
s <- scala(serialize=serialize,stdout=output,stderr=output)
s %~% "scala.util.Properties.versionNumberString"


capture.output(s %~% "new java.util.Random().nextDoubllllllllle")
capture.output(s %~% "new java.util.Random().nextDouble")
capture.output(s %~% "3+2")
capture.output(s %~% "println(3+2)")
capture.output(s %~% 'R.eval("""cat(R.version.string)""")')

