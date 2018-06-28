library(rscala)
library(microbenchmark)

cat(system2("hostname",stdout=TRUE),"\n",sep="")
scalaInfo()
sessionInfo()

scala()
nReps <- 10000

#####

rng <- s$.new_scala.util.Random()
rng$nextInt()      # First-time compilation
rng$nextInt(1L)    # First-time compilation
nextInt <- rng$nextInt
nextInt()          # Alreary compiled
nextInt(1L)        # Alreary compiled

microbenchmark(
  s(rng=rng) ^ 'rng.nextInt()',
  s(rng=rng) * 'rng.nextInt()',
  rng$.nextInt(),
  rng$nextInt(),
  nextInt(),
  rng$.nextInt(100L),
  rng$nextInt(100L),
  nextInt(100L),
  times=nReps
)
