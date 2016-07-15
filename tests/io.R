library(rscala)

serialize <- as.logical(Sys.getenv("RSCALA_SERIALIZE"))
output <- as.logical(Sys.getenv("RSCALA_OUTPUT"))
version <- Sys.getenv("RSCALA_SCALA_VERSION")
s <- scala(serialize=serialize,stdout=output,stderr=output)
if ( version != s %~% "scala.util.Properties.versionNumberString" ) stop("Version mismatch.")

tryCatch(capture.output(s %~% "new java.util.Random(23455).nextDoubllllllllle"),error=function(e) e)
capture.output(s %~% "new java.util.Random(234523).nextDouble")
capture.output(s %~% "3+2")
capture.output(s %~% "println(3+2)")
capture.output(s %~% 'R.eval("""cat(R.version.string)""")')

