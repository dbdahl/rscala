if ( ! ( "windows" %in% tolower(Sys.info()[["sysname"]]) ) ) {
  library(testthat)
  library(rscala)
  test_check("rscala")
}

