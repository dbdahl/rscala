source("common.R",print.eval=TRUE)


s %~% "val a = @{1+3}"
s$a
s$.a

name <- "David Dahl"
s %~% 'val name = "@{name}".reverse'
s$name

