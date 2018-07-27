rscala::.rscalaOptions(heap.maximum="2G")
library(sdols)
library(parallel)
load('psm_ex.RData')
# Check whether this runs
salso(psm, loss = 'lowerBoundVariationOfInformation') 

cl <- makeCluster(4, type = 'FORK')
res <- parLapply(cl, 1:1000, salso(psm, loss = 'lowerBoundVariationOfInformation'))
stopCluster(cl)

mclapply(1:1000, salso(psm, loss = 'lowerBoundVariationOfInformation'), mc.cores = 2)
