library(rscala)
library(microbenchmark)

cat(system2("hostname",stdout=TRUE),"\n",sep="")
scalaExec()
sessionInfo()

openAndClose <- function() {
  scala()
  s * "3"
  close(s)
}

microbenchmark(
  openAndClose(),
  times=10
)
