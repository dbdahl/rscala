library(rscala)
library(microbenchmark)

cat(system2("hostname",stdout=TRUE),"\n",sep="")
scalaInfo()
sessionInfo()

scala()
nReps <- 10000

#####

rng <- s$.scala.util.Random$new()
rng$nextInt()      # First-time compilation
rng$nextInt(1L)    # First-time compilation
nextInt <- rng$nextInt
nextInt()          # Alreary compiled
nextInt(1L)        # Alreary compiled

a <- function(rng) s %!% 'rng.nextInt()'
b <- function(rng) s %.!% 'rng.nextInt()'
a(rng)
b(rng)

microbenchmark(
  b(rng),
  a(rng),
  rng$nextInt(.AS.REFERENCE=TRUE),
  rng$nextInt(),
  nextInt(),
  rng$nextInt(100L,.AS.REFERENCE=TRUE),
  rng$nextInt(100L),
  nextInt(100L),
  times=nReps
)


