heap.maximum <- "64M"
source("common.R",print.eval=TRUE)

tryCatch(a <- s$".Array[Double]"$new(I(20000000L)),error=function(e) e)
close(s)
cat("----\n")

heap.maximum <- "386M"
source("common.R",print.eval=TRUE)
tryCatch(a <- s$".Array[Double]"$new(I(20000000L)),error=function(e) e)
close(s)
cat("----\n")

options(rscala.heap.maximum="64M")
source("common.R",print.eval=TRUE)
tryCatch(a <- s$".Array[Double]"$new(I(20000000L)),error=function(e) e)
close(s)
cat("----\n")

options(rscala.heap.maximum="128M")
source("common.R",print.eval=TRUE)
for ( i in 1:2000 ) {
  a <- s$".Array[Double]"$new(I(100000L))  # No memory problems
}

tryCatch({
for ( i in 1:2000 ) {
  s %~% "new Array[Double](100000)"           # Memory problems
}
},error=function(e) e)


