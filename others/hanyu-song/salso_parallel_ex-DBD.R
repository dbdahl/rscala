library(parallel)
load('psm_ex.RData')

cl <- makeCluster(4, type = 'FORK')
clusterEvalQ(cl,library(sdols))

### IMPORTANT: You need to call library(sdols) in clusterEvalQ **BEFORE** doing
### so in the main script.

# Check whether this runs
library(sdols)
salso(psm, loss = 'lowerBoundVariationOfInformation') 

res <- parLapply(cl, 1:10, function(i) salso(psm, loss = 'lowerBoundVariationOfInformation', multicore=FALSE))

stopCluster(cl)

