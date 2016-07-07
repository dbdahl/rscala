library(rscala)

serialize <- as.logical(Sys.getenv("RSCALA_SERIALIZE"))
cat(serialize,"\n")
s <- scalaInterpreter(serialize=serialize)

s %~% "util.Properties.versionNumberString"

capture.output(s %~% "new java.util.Random().nextDoubllllllllle")
capture.output(s %~% "new java.util.Random().nextDouble")
capture.output(s %~% "3+2")
capture.output(s %~% "println(3+2)")
capture.output(s %~% 'R.eval("""cat(R.version.string)""")')

