heap.maximum <- "192M"
source("common.R",print.eval=TRUE)
tryCatch(a <- s$".Array[Double]"$new(20000000L),error=function(e) e)
close(s)
cat("----\n")

heap.maximum <- "386M"
source("common.R",print.eval=TRUE)
tryCatch(a <- s$".Array[Double]"$new(20000000L),error=function(e) e)
close(s)
cat("----\n")

heap.maximum <- "64M"
source("common.R",print.eval=TRUE)
tryCatch(a <- s$".Array[Double]"$new(20000000L),error=function(e) e)
close(s)
cat("----\n")

heap.maximum <- "128M"
source("common.R",print.eval=TRUE)
for ( i in 1:2000 ) {
  cat(i,"\n")
  a <- s$".Array[Double]"$new(200000L)      # No memory problems
}

tryCatch({
for ( i in 1:2000 ) {
  cat(i,"\n")
  a <- s %~% "new Array[Double](200000)"    # Memory problems
}
},error=function(e) e)


