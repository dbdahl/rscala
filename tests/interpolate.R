library(rscala)

serialize <- as.logical(Sys.getenv("RSCALA_SERIALIZE"))
cat(serialize,"\n")
s <- scala(serialize=serialize)

s %~% "scala.util.Properties.versionNumberString"

s %~% "val a = @{1+3}"
s$a
s$.a

name <- "David Dahl"
s %~% 'val name = "@{name}".reverse'
s$name

