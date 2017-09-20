heap.maximum <- "3G"
source("common.R",print.eval=TRUE)

library(help="rscala")

func0a <- function(x=matrix(0.0,nrow=10000,ncol=300)) {  # 22.8 MB
  s %!% 'x'
}

func1a <- function(x=matrix(0L,nrow=20000,ncol=300)) {  # 22.8 MB
  s %!% 'x'
}

func1b <- function(x=matrix(0L,nrow=200,ncol=300)) {  # 0.228 MB
  s %!% 'x'
}

func2a <- function(x=integer(6000000)) {  # 22.8 MB
  s %!% 'x'
}

func2b <- function(x=integer(60000)) {  # 0.228 MB
  s %!% 'x'
}

func3a <- function(x=integer(6000000)) {  # 22.8 MB
  s %!% 'x.length'
}

func4a <- function(x=integer(0)) {  # 0.228 MB
  s %!% 'new Array[Int](6000000)'
}

s$a <- 1:6000000
invisible(s$a)
rng <- s$.scala.util.Random$new()
invisible(sapply(1:1000,function(i) rng$nextGaussian()))   # Burn-in

library(microbenchmark)
set.seed(13124)

microbenchmark(
  rng$nextGaussian(),
  unit="us",
  times=2500)

microbenchmark(
  rng$nextGaussian(),
  func0a(),
  func1a(),
  func1b(),
  func2a(),
  func2b(),
  func3a(),
  func4a(),
  s$a <- 1:6000000,
  s$a,
  unit="ms",
  times=25)

