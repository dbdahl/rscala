heap.maximum <- "4G"
source("common.R",print.eval=TRUE)

func1a <- function(x=matrix(0.0,nrow=100000,ncol=300)) {  # 228 MB
  s %!% 'x'
}

func1b <- function(x=matrix(0.0,nrow=1000,ncol=300)) {  # 2.28 MB
  s %!% 'x'
}

func2a <- function(x=integer(60000000)) {  # 228 MB
  s %!% 'x'
}

func2b <- function(x=integer(600000)) {  # 2.28 MB
  s %!% 'x'
}


library(microbenchmark)
set.seed(13124)

microbenchmark(
  func1a(),
  func1b(),
  func2a(),
  func2b(),
  times=5)

