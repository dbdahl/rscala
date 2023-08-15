# if ( ! isTRUE(tryCatch({rscala::scalaConfig(reconfig="offline")}, error=function(e) TRUE)) ) {
#   library(testthat)
#   library(rscala)
#   test_check("rscala")
# }
