heap.maximum <- "32G"
source("common.R",print.eval=TRUE)

library(help="rscala")

func0a <- function(x=matrix(0.0,nrow=100000,ncol=300)) {  # 228 MB
  s %!% 'x'
}

func1a <- function(x=matrix(0L,nrow=200000,ncol=300)) {  # 228 MB
  s %!% 'x'
}

func1b <- function(x=matrix(0L,nrow=2000,ncol=300)) {  # 2.28 MB
  s %!% 'x'
}

func2a <- function(x=integer(60000000)) {  # 228 MB
  s %!% 'x'
}

func2b <- function(x=integer(600000)) {  # 2.28 MB
  s %!% 'x'
}

func3a <- function(x=integer(60000000)) {  # 228 MB
  s %!% 'x.length'
}

func4a <- function(x=integer(0)) {  # 228 MB
  s %!% 'new Array[Int](60000000)'
}


s$a <- 1:60000000
invisible(s$a)

library(microbenchmark)
set.seed(13124)

microbenchmark(
  func0a(),
  func1a(),
  func1b(),
  func2a(),
  func2b(),
  func3a(),
  func4a(),
  s$a <- 1:60000000,
  s$a,
  times=5)

