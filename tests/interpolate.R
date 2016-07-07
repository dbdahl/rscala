library(rscala)

serialize <- as.logical(Sys.getenv("RSCALA_SERIALIZE"))
cat(serialize,"\n")
s <- scalaInterpreter(serialize=serialize)

s %~% "util.Properties.versionNumberString"

s %~% "val a = @{1+3}"
s$a
s$.a

name <- "David Dahl"
s %~% 'val name = "@{name}".reverse'
s$name

