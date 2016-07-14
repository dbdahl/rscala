library(rscala)

serialize <- as.logical(Sys.getenv("RSCALA_SERIALIZE"))
output <- as.logical(Sys.getenv("RSCALA_OUTPUT"))
cat(serialize,"\n")
cat(output,"\n")
s <- scala(serialize=serialize,stdout=output,stderr=output)
s %~% "scala.util.Properties.versionNumberString"


s %~% "val a = @{1+3}"
s$a
s$.a

name <- "David Dahl"
s %~% 'val name = "@{name}".reverse'
s$name

